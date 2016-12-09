package com.statemachinesystems.csexp

import java.io.{ByteArrayInputStream, IOException, InputStream}
import java.nio.ByteBuffer

import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}


class TokenReaderSpec extends WordSpec with PropertyChecks with Matchers {

  val Zero = characters('0')
  val Digits = characters('0' to '9': _*)
  val Parens = characters('(', ')')
  val Colon = characters(':')

  val InputBytes = Gen.chooseNum[Byte](Byte.MinValue, Byte.MaxValue, (Digits union Parens union Colon).toSeq: _*)

  "A TokenReader" should {

    "read a stream of tokens" in {
      forAll(tokenStreams) { stream =>
        val bytes = writeBytes(stream).toArray
        tokenStreamOf(bytes) shouldBe stream
      }
    }

    "reject an illegal input byte in the start state" in {
      forAll(InputBytes suchThat excluding(Digits union Parens)) { byte =>
        errorMessageFrom(Array(byte)) shouldBe IllegalInputByte(byte & 0xFF).toString
      }
    }

    "reject leading zeros in atom lengths" in {
      forAll(Gen.oneOf(Digits.toSeq)) { byte =>
        errorMessageFrom(Array[Byte]('0', byte)) shouldBe LeadingZeroInAtomLength.toString
      }
    }

    "reject an illegal input byte in atom lengths" in {
      val atomLengths = Gen.chooseNum(0, 100).map(_.toString.getBytes)
      val illegalAtomLengthBytes = InputBytes suchThat excluding(Digits union Colon)

      forAll(atomLengths, illegalAtomLengthBytes) { (length, illegalByte) =>
        val input = length ++ Array(illegalByte)
        errorMessageFrom(input) shouldBe IllegalByteInAtomLength(illegalByte & 0xFF).toString
      }
    }

    "reject EOF in atom lengths" in {
      val atomLengths = Gen.chooseNum(0, 10).map(_.toString.getBytes)

      forAll(atomLengths) { length =>
        errorMessageFrom(length) shouldBe UnexpectedEndOfInputInAtomLength.toString
      }
    }

    s"reject an atom length greater than ${Int.MaxValue}" in {
      val excessiveLengths = Gen.chooseNum(Int.MaxValue.toLong + 1, Long.MaxValue).map(_.toString.getBytes)

      forAll(excessiveLengths) { length =>
        errorMessageFrom(length) shouldBe LengthLimitExceeded.toString
      }
    }

    "reject atoms whose length is less than expected" in {
      val atomsWithActualLengthLessThanExpected = for {
        expectedLength <- Gen.chooseNum(1, 1024)
        actualLength <- Gen.chooseNum(1, expectedLength)
        if actualLength < expectedLength
      } yield {
        (expectedLength, actualLength)
      }

      forAll(atomsWithActualLengthLessThanExpected) {
        case (expectedLength, actualLength) =>
          val atomBytes = invalidAtomBytes(expectedLength, actualLength)
          errorMessageFrom(atomBytes) shouldBe InsufficientInputData(expectedLength, actualLength).toString
      }
    }

    "safely wrap an input exception in an error token" in {
      val exception = new IOException("expected")

      val tokenStreamsWithInputFailures = for {
        exampleStream <- tokenStreams if exampleStream.nonEmpty
        inputBytes = writeBytes(exampleStream).toArray
        throwAtIndex <- Gen.chooseNum(0, inputBytes.length - 1)
        inputStream = new ByteArrayInputStream(inputBytes)
      } yield tokenStreamOf(new ThrowingInputStream(inputStream, throwAtIndex, exception))

      forAll(tokenStreamsWithInputFailures) { stream =>
        val errorToken = stream.dropWhile {
          case e: Error => false
          case _ => true
        }.head

        errorMessageFrom(errorToken) shouldBe InputFailure(exception).toString
      }
    }
  }

  private def tokenStreams = Gen.containerOf[Stream, Token] {
    Gen.oneOf(Gen.const(LParen), Gen.const(RParen), atoms(maxLength = 50))
  }

  private def characters(chars: Char*): Set[Byte] =
    chars.map(_.toByte).toSet

  private def excluding(set: Set[Byte]): Byte => Boolean =
    n => !set.contains(n)

  private def atoms(maxLength: Int) = for {
    atomSize <- Gen.chooseNum(0, maxLength)
    bytes <- Gen.listOfN(atomSize, InputBytes)
  } yield atomOf(bytes: _*)

  private def tokenStreamOf(bytes: Array[Byte]): Stream[Token] =
    tokenStreamOf(new ByteArrayInputStream(bytes))

  private def tokenStreamOf(inputStream: InputStream): Stream[Token] =
    TokenReader.read(Input.fromStream(inputStream))

  private def errorMessageFrom(bytes: Array[Byte]): String = tokenStreamOf(bytes) match {
    case Stream(Error(errorType)) => errorType.toString
    case other => s"Not an error message: $other"
  }

  private def errorMessageFrom(token: Token): String = token match {
    case Error(errorType) => errorType.toString
    case other => s"Not an error message: $other"
  }

  private def atomOf(s: String): Atom = Atom(ByteBuffer.wrap(s.getBytes))

  private def atomOf(bytes: Byte*): Atom = Atom(ByteBuffer.wrap(bytes.toArray))

  private def invalidAtomBytes(expectedLength: Int, actualLength: Int) =
    s"$expectedLength:".getBytes ++ Gen.listOfN(actualLength, InputBytes).map(_.toArray).sample.get

  private def writeBytes(tokens: Stream[Token]): Stream[Byte] = tokens.flatMap {
    case LParen => Seq('('.toByte)
    case RParen => Seq(')'.toByte)
    case Atom(buf) => s"${buf.array.length}:".getBytes ++ buf.array.toSeq
    case Error(_) => ???
  }

  private class ThrowingInputStream(underlying: InputStream,
                                    throwAtIndex: Int,
                                    exception: Exception) extends InputStream {
    private var index = 0

    override def read(): Int = {
      if (index == throwAtIndex) {
        throw exception
      } else {
        index += 1
        underlying.read()
      }
    }
  }
}
