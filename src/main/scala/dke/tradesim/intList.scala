package dke.tradesim

import java.io.{ByteArrayInputStream, InputStream}

import bitManipulation._

object intList {
  class BitWriter {
    var pos = 0
    var nextPos = 0
    var bytesWritten = 0
    var currentByte = 0
    var out = new java.io.ByteArrayOutputStream()

    // <int> is treated as an unsigned integer/bit-string (i.e. no extra sign bit is written)
    def write(int: BigInt, n: Int) {
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
        currentByte = (currentByte << numberOfBitsToWrite) | ((int >> rightShiftCount).toInt & rightmostBitsMask)
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

    // returns an unsigned integer representing the <n> bits read from the bit-stream
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

  class BinaryPackingIntListEncoder(val blockSizeInBits: Int = 128) {
    def encode(ints: Seq[BigInt]): java.io.ByteArrayOutputStream = {
      val bw = new BitWriter
      if (ints.nonEmpty) {
        val deltaEncodedInts = deltaEncodeInts(ints.sorted)

        val signedStartInt = deltaEncodedInts.head
        val remainingInts = deltaEncodedInts.tail
        val slices = remainingInts.sliding(blockSizeInBits, blockSizeInBits).toIndexedSeq
        val numberOfSlices = slices.length

        VariableByteSignedIntEncoder.writeSigned(bw, signedStartInt)
        VariableByteSignedIntEncoder.write(bw, numberOfSlices)

        slices.foreach(sliceOfInts => FrameOfReferenceIntListEncoder.write(bw, sliceOfInts))
      }
      bw.close
      bw.out
    }

    def decode(binaryEncodedIntList: ByteArrayInputStream): Seq[BigInt] = {
      if (binaryEncodedIntList.available == 0) return List.empty[BigInt]

      val br = new BitReader(binaryEncodedIntList)

      val signedStartInt = VariableByteSignedIntEncoder.readSigned(br)
      val numberOfSlices = VariableByteSignedIntEncoder.read(br)

      var deltaEncodedIntList = List(signedStartInt)
      var i = 0
      while (i < numberOfSlices) {
        deltaEncodedIntList = deltaEncodedIntList ++ FrameOfReferenceIntListEncoder.read(br)
        i += 1
      }

      deltaDecodeInts(deltaEncodedIntList)
    }
  }

  object FrameOfReferenceIntListEncoder {
    def write(bw: BitWriter, ints: Seq[BigInt]) {
      val numberOfInts = ints.length
      if (numberOfInts > 0) {
        val signedMin = ints.min
        val offsets = ints.map(_ - signedMin)   // all offsets are guaranteed to be non-negative
        val intBitSize = bitLength(offsets.max)

        VariableByteSignedIntEncoder.writeSigned(bw, signedMin)
        VariableByteSignedIntEncoder.write(bw, numberOfInts)
        VariableByteSignedIntEncoder.write(bw, intBitSize)
        
        offsets.foreach(int => bw.write(int, intBitSize))
      }
    }

    def read(br: BitReader): Seq[BigInt] = {
      val signedMinInt = VariableByteSignedIntEncoder.readSigned(br)
      val numberOfInts = VariableByteSignedIntEncoder.read(br).toInt
      val intBitSize = VariableByteSignedIntEncoder.read(br).toInt

      var ints = Seq.empty[BigInt]
      var i = 0
      while (i < numberOfInts) {
        ints = ints :+ (signedMinInt + br.read(intBitSize))
        i += 1
      }
      ints
    }
  }

  object VariableByteSignedIntEncoder {
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

    /**
     * bw is a BitWriter object
     * int is a signed integer
     * returns the number of bytes written to the BitWriter
     */
    def writeSigned(bw: BitWriter, int: BigInt): Int = {
      val bitCount = bitLength(int) + 1   // + 1 to account for the sign bit we prefix the integer with
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
      while (mostSignificantBit(int, Some(7)) == 1) {
        int = br.read(8)
        sum = (sum << 7) | (int & 0x7F)
      }
      sum
    }

    // returns a signed int read from BitReader object <br>
    def readSigned(br: BitReader): BigInt = {
      var int = br.read(8)
      var count = 1
      var sum = BigInt(int.toInt & 0x7F)
      while (mostSignificantBit(int, Some(7)) == 1) {
        int = br.read(8)
        count += 1
        sum = (sum << 7) | (int & 0x7F)
      }
      uToS(sum, 7 * count - 1)
    }
  }
}
