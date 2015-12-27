package com.statemachinesystems.csexp

import java.io.{BufferedInputStream, InputStream}
import java.nio.ByteBuffer

import scala.annotation.tailrec
import scala.util.Try


object TokenReader {

  private val Eof = -1

  private lazy val EmptyByteBuffer = ByteBuffer.allocate(0).asReadOnlyBuffer

  def read(in: InputStream): Try[Stream[Token]] = Try {
    readTokenStream(if (in.markSupported) in else new BufferedInputStream(in))
  }

  private def readTokenStream(in: InputStream): Stream[Token] = {
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

  private def readAtom(in: InputStream, firstDigit: Int): Token =
    readLength(in, firstDigit - '0') match {
      case Left(errorType) => Error(errorType)
      case Right(length) => readBytes(in, length)
    }

  private def readEmptyAtom(in: InputStream): Token = in.read() match {
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
  private def readLength(in: InputStream, length: Long): Either[ErrorType, Int] =
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

  private def readBytes(in: InputStream, length: Int): Token = {
    val bytes = new Array[Byte](length)

    @tailrec
    def readIntoArray(offset: Int = 0): Int =
      if (offset < length)
        in.read(bytes, offset, length - offset) match {
          case count if count > 0 => readIntoArray(offset + count)
          case done => offset
        }
      else
        offset

    val bytesRead = readIntoArray()

    if (bytesRead == length)
      Atom(ByteBuffer.wrap(bytes).asReadOnlyBuffer)
    else
      Error(InsufficientInputData(expectedLength = length, actualLength = bytesRead))
  }
}
