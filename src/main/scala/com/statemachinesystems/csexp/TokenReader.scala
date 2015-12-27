package com.statemachinesystems.csexp

import java.nio.ByteBuffer

import scala.annotation.tailrec
import scala.util.Try

object TokenReader {

  import Input.Eof

  private lazy val EmptyByteBuffer = ByteBuffer.allocate(0).asReadOnlyBuffer

  def read(in: Input): Try[Stream[Token]] = Try {
    readTokenStream(in)
  }

  private def readTokenStream(in: Input): Stream[Token] = {
    def continue(token: Token) = token match {
      case error: Error => Stream(error)
      case _ => Stream.cons(token, readTokenStream(in))
    }

    in.read() match {
      case '(' =>
        continue(LParen)

      case ')' =>
        continue(RParen)

      case firstDigit@('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') =>
        continue(readAtom(in, firstDigit))

      case '0' =>
        continue(readEmptyAtom(in))

      case Eof =>
        Stream.empty

      case byte =>
        Stream(Error(IllegalInputByte(byte)))
    }
  }

  private def readAtom(in: Input, firstDigit: Int): Token = {
    val errorOrAtom = for {
      length <- readLength(in, firstDigit - '0').right
      buf <- in.readBytes(length).right
    } yield buf

    errorOrAtom.fold(Error, Atom)
  }

  private def readEmptyAtom(in: Input): Token = in.read() match {
    case ':' =>
      Atom(EmptyByteBuffer)

    case Eof =>
      Error(UnexpectedEndOfInputInAtomLength)

    case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
      Error(LeadingZeroInAtomLength)

    case byte =>
      Error(IllegalByteInAtomLength(byte))
  }

  @tailrec
  private def readLength(in: Input, length: Long): Either[ErrorType, Int] =
    if (length > Int.MaxValue)
      Left(LengthLimitExceeded)
    else
      in.read() match {
        case ':' =>
          Right(length.toInt)

        case digit@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') =>
          readLength(in, length * 10 + digit - '0')

        case Eof =>
          Left(UnexpectedEndOfInputInAtomLength)

        case byte =>
          Left(IllegalByteInAtomLength(byte))
      }
}
