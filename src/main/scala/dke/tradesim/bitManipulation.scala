package dke.tradesim

object bitManipulation {
  def mostSignificantBit(i: Byte): Int = getBit(i, 7)
  def mostSignificantBit(i: Int): Int = getBit(i, 31)
  def mostSignificantBit(i: Long): Int = getBit(i, 63)

  // mostSignificantBit(BigInt(5)) => 1
  // mostSignificantBit(BigInt(-5)) => 0
  // mostSignificantBit(BigInt(0)) => 0
  def mostSignificantBit(i: BigInt, mostSignificantBitIndex: Option[Int] = None): Int = getBit(i, mostSignificantBitIndex.getOrElse(mostSignificantBitPosition(i)))

  // returns the 0-based index position of the most-significant-bit with respect to the least-significant-bit (i.e. the least-significant-bit is as index 0)
  def mostSignificantBitPosition(i: BigInt): Int = {
    val len = bitLength(i)
    if (len == 0) 0 else len - 1
  }

  // ithLeastSignificantBit is a 0-based index from the right-most (least-significant) bit of the byte
  def getBit(byte: Byte, ithLeastSignificantBit: Int): Int = (byte >> ithLeastSignificantBit) & 0x1
  def getBit(int: Int, ithLeastSignificantBit: Int): Int = (int >> ithLeastSignificantBit) & 0x1
  def getBit(long: Long, ithLeastSignificantBit: Int): Int = ((long >> ithLeastSignificantBit) & 0x1).toInt
  def getBit(bigInt: BigInt, ithLeastSignificantBit: Int): Int = ((bigInt >> ithLeastSignificantBit) & 0x1).toInt

  // extractInt(0b11101010, 6, 2)
  // => 26
  // extractInt(0b11101010, 5, 2)
  // => 10
  def extractInt(byte: Byte, msbit: Int, lsbit: Int): Int = {
    if (msbit < lsbit) throw new Exception("most-significant-bit position cannot be less than the least-significant-bit position")
    if (lsbit < 0 || msbit > 7) throw new Exception("least-significant-bit position must be >= 0 and most-significant-bit position must be < 8")
    lsbit.to(msbit).foldLeft(0) { (sum, ith_lsbit) => sum + getBit(byte, ith_lsbit) * (1 << (ith_lsbit - lsbit)) }
  }

  // extractIntLR(0b11101010, 0, 2)
  // => 7
  // extractIntLR(0b11101010, 1, 3)
  // => 6
  // extractIntLR(0b11101010, 5, 7)
  // => 2
  def extractIntLR(byte: Byte, leftIndex: Int, rightIndex: Int): Int = {
    if (leftIndex > rightIndex) throw new Exception("leftIndex cannot be greater than the rightIndex")
    extractInt(byte, 7 - leftIndex, 7 - rightIndex)
  }

//  def bitLength(i: Int): Int = java.lang.Integer.SIZE - java.lang.Integer.numberOfLeadingZeros(i)
//  def bitLength(i: Long): Int = java.lang.Long.SIZE - java.lang.Long.numberOfLeadingZeros(i)
  def bitLength(i: Int): Int = bitLength(BigInt(i))
  def bitLength(i: Long): Int = bitLength(BigInt(i))
  def bitLength(i: BigInt): Int = i.bitLength

  def bitLengthInts(ints: Seq[Int]): Int = ints.foldLeft(0){ (sum, i) => sum + bitLength(i) }
  def bitLengthLongs(ints: Seq[Long]): Int = ints.foldLeft(0){ (sum, i) => sum + bitLength(i) }
  def bitLengthBigInts(ints: Seq[BigInt]): Int = ints.foldLeft(0){ (sum, i) => sum + bitLength(i) }

  def uToS(unsignedInt: BigInt, mostSignificantBitIndex: Int): BigInt = {
    val msb = mostSignificantBit(unsignedInt, Some(mostSignificantBitIndex))
    if (msb == 1) {
      -(BigInt(1) << mostSignificantBitIndex) + unsignedInt.flipBit(mostSignificantBitIndex)
    } else unsignedInt
  }

  def printBits(int: Byte) { 7.to(0).by(-1).foreach(i => print(getBit(int, i))) }
  def printBits(int: Int) { 31.to(0).by(-1).foreach(i => print(getBit(int, i))) }
  def printBits(int: Long) { 63.to(0).by(-1).foreach(i => print(getBit(int, i))) }
  def printBits(int: BigInt) { 128.to(0).by(-1).foreach(i => print(getBit(int, i))) }
  def printBits(ints: Array[Byte]) { ints.foreach{i => printBits(i); print(" ")} }
  def printBits(ints: Seq[BigInt]) { ints.foreach{i => printBits(i); print(" ")} }

  /**
   * deltaEncodeInts(List(50, 60, 70, 75))
   * res14: Seq[Int] = List(50, 10, 10, 5)
   */
  def deltaEncodeInts(ints: Seq[BigInt]): Seq[BigInt] = {
    def encodeList(ints: Seq[BigInt], prev: BigInt, newList: Seq[BigInt]): Seq[BigInt] = {
      if (ints.isEmpty) newList
      else {
        val next = ints.head
        encodeList(ints.tail, next, newList :+ (next - prev))
      }
    }
    encodeList(ints, 0, List())
  }

  /**
   * deltaDecodeInts(List(50, 10, 10, 5))
   * res13: Seq[Int] = List(50, 60, 70, 75)
   */
  def deltaDecodeInts(ints: Seq[BigInt]): Seq[BigInt] = {
    var sum = BigInt(0)
    ints.foldLeft(List[BigInt]()) { (list, i) => sum += i; list :+ sum }
  }
}
