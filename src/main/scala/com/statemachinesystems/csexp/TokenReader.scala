package com.statemachinesystems.csexp

import java.nio.ByteBuffer

import scala.annotation.tailrec
import scala.util.Try

object TokenReader {

  import Input.Eof

  private lazy val EmptyByteBuffer = ByteBuffer.allocate(0).asReadOnlyBuffer

  def read(in: Input): Stream[Token] =
    readTokenStream(in)

  private def readTokenStream(in: Input): Stream[Token] = {
    def continue(token: Token) = token match {
      case error: Error => Stream(error)
      case _ => Stream.cons(token, readTokenStream(in))
    }

    try {
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
    } catch {
      case Input.InputException(errorType) => Stream(Error(errorType))
    }
  }

  private def readAtom(in: Input, firstDigit: Int): Token = try {
    val length = readLength(in, firstDigit - '0')
    Atom(in.readBytes(length))
  } catch {
    case Input.InputException(errorType) => Error(errorType)
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
  private def readLength(in: Input, length: Long): Int =
    if (length > Int.MaxValue)
      throw Input.InputException(LengthLimitExceeded)
    else
      in.read() match {
        case ':' =>
          length.toInt

        case digit@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') =>
          readLength(in, length * 10 + digit - '0')

        case Eof =>
          throw Input.InputException(UnexpectedEndOfInputInAtomLength)

        case byte =>
          throw Input.InputException(IllegalByteInAtomLength(byte))
      }
}
