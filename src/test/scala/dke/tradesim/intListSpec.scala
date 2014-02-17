package dke.tradesim

//import org.junit.runner.RunWith
//import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec
import intList._
import bitManipulation._
import java.io.{IOException, ByteArrayInputStream}

//@RunWith(classOf[JUnitRunner])
class intListSpec extends FunSpec {
  describe("bit writing and reading") {
    it("writes and reads bits") {
      val bw = new BitWriter
      bw.write(BigInt(6), 3)
      bw.write(BigInt(3), 2)
      bw.write(BigInt(1), 3)
      bw.write(BigInt(481), 9)
      bw.write(BigInt(67), 7)
      bw.close

      val br = new BitReader(new ByteArrayInputStream(bw.toByteArray))

      assert(br.read(1) === 1)
      assert(br.read(3) === 5)
      assert(br.read(4) === 9)
      assert(br.read(4) === 15)
      assert(br.read(8) === 12)
      assert(br.read(4) === 3)
      intercept[IOException] {
        br.read(4)
      }
    }
  }

  describe("BinaryPackingIntListEncoder") {
    it("encodes a list of integers") {
      val e = new BinaryPackingIntListEncoder(128)
      val ints = List(1, 2, 50).map(BigInt(_))

      val encodedInts = e.encode(ints)
      val decodedInts = e.decode(new ByteArrayInputStream(encodedInts.toByteArray))

      assert(ints.sorted === decodedInts.sorted)
    }
  }

  describe("VariableByteSignedIntEncoder") {
    it("writes integers to a bitwriter") {
      val bw = new BitWriter
      VariableByteSignedIntEncoder.write(bw, 500)
      VariableByteSignedIntEncoder.writeSigned(bw, -5)
      VariableByteSignedIntEncoder.write(bw, 270)
      bw.close

      val br = new BitReader(new ByteArrayInputStream(bw.toByteArray))
      assert(VariableByteSignedIntEncoder.read(br) === BigInt(500))
      assert(VariableByteSignedIntEncoder.readSigned(br) === BigInt(-5))
      assert(VariableByteSignedIntEncoder.read(br) === BigInt(270))
    }
  }
}
