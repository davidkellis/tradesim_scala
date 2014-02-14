package dke.tradesim

import java.io.InputStream

import bitManipulation._

object intList {
  class BitWriter {
    var pos = 0
    var nextPos = 0
    var bytesWritten = 0
    var currentByte = 0
    var out = new java.io.ByteArrayOutputStream()

    def write(int: Int, n: Int) {
      nextPos = pos + n

      while (remainingBitsToWrite > 0) {
        val numberOfBitsToWrite = if (remainingBitsToWrite > numberOfFreeBitsInCurrentByte) {
          // then, we need to write to all of the remaining free bits in the current byte
          numberOfFreeBitsInCurrentByte
        } else {
          // write the remaining bits of <int> to a portion of the current byte
          remainingBitsToWrite
        }
        val rightmostBitsMask = (1 << numberOfBitsToWrite) - 1
        val rightShiftCount = remainingBitsToWrite - numberOfBitsToWrite
        currentByte = (currentByte << numberOfBitsToWrite) | ((int >>> rightShiftCount) & rightmostBitsMask)
        pos += numberOfBitsToWrite

        if (isAtBeginningOfByteBoundary) writeByte    // if we're at the beginning of a byte-boundary, we need to write the current byte to the bitstring and create a new byte-buffer
      }
    }

    def close {
      if (!isAtBeginningOfByteBoundary) write(0, numberOfFreeBitsInCurrentByte)
      out.close()
    }

    def toByteArray: Array[Byte] = out.toByteArray

    private

    def writeByte {
      out.write(currentByte)
      currentByte = 0
      bytesWritten += 1
    }

    def isAtBeginningOfByteBoundary = currentByteBitPosition == 0

    def currentByteBitPosition = pos % 8

    def numberOfFreeBitsInCurrentByte = 8 - currentByteBitPosition

    def remainingBitsToWrite = nextPos - pos
  }

  class BitReader(val byteSeq: InputStream) {
    var pos = 0
    var nextPos = 0
    var bytesRead = 0
    var currentByte: Byte = 0

    def read(n: Int): BigInt = {
      var sum = BigInt(0)
      if (n > 0) {
        nextPos = pos + n
        while (remainingBitsToRead > 0) {
          if (isAtBeginningOfByteBoundary) currentByte = readByte      // if we're at the beginning of a byte-boundary, we need to read the "current byte" into memory
          val numberOfBitsToRead = if (remainingBitsToRead > numberOfUnreadBitsInCurrentByte) {
              // then, read all the unread bits in the current byte
              numberOfUnreadBitsInCurrentByte
            } else {
              // read just a portion of the current byte
              remainingBitsToRead
            }
          sum = (sum << numberOfBitsToRead) | extractIntLR(currentByte, currentByteBitPosition, currentByteBitPosition + numberOfBitsToRead - 1)
          pos += numberOfBitsToRead
        }
      }
      sum
    }

    private

    def readByte: Byte = {
      val byte = byteSeq.read()
      if (byte == -1) throw new java.io.IOException("End of Stream Reached!")
      bytesRead += 1
      byte.toByte
    }


    def isAtBeginningOfByteBoundary = currentByteBitPosition == 0

    def currentByteBitPosition = pos % 8

    def numberOfUnreadBitsInCurrentByte = 8 - currentByteBitPosition

    def remainingBitsToRead = nextPos - pos
  }

  object FrameOfReferenceIntListEncoder {
    def write(bw: BitWriter, ints: Seq[BigInt]) {
      val numberOfInts = ints.length
      if (numberOfInts > 0) {
        val signedMin = ints.min
        val offsets = ints.map(_ - signedMin)   // all offsets are guaranteed to be non-negative
        val intBitSize = bitLength(offsets.max)

        val unsignedMinInt =
      }
    }

    def read(br: BitReader): Seq[BigInt] = {

    }
  }

  object VariableByteIntEncoder {
    /**
     * bw is a BitWriter object
     * int is an unsigned integer
     * returns the number of bytes written to the BitWriter
     */
    def write(bw: BitWriter, int: BigInt): Int = {
      val bitCount = bitLength(int)
      val bitCountMod7 = bitCount % 7
      val evenlyDivisibleBitCount = if (bitCountMod7 == 0) bitCount else bitCount + (7 - bitCountMod7)    // number of bits required to hold <int> in a whole number of 7-bit words
      val sevenBitWordCount = evenlyDivisibleBitCount / 7
      var tmpInt = 0
      1.until(sevenBitWordCount).foreach { wordIndex =>
        val shiftAmount = (sevenBitWordCount - wordIndex) * 7
        tmpInt = ((int >> shiftAmount).toInt & 0x7F) | 0x80
        bw.write(tmpInt, 8)
      }
      tmpInt = int.toInt & 0x7F
      bw.write(tmpInt, 8)

      sevenBitWordCount
    }

    // returns an unsigned int read from BitReader object <br>
    def read(br: BitReader): BigInt = {
      var int = br.read(8)
      var sum = BigInt(int.toInt & 0x7F)
      while (mostSignificantBit(int) == 1) {
        int = br.read(8)
        sum = (sum << 7) | (int & 0x7F)
      }
      sum
    }
  }
}
