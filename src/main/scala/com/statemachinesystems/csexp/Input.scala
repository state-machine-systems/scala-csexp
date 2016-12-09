package com.statemachinesystems.csexp

import java.io.{BufferedInputStream, IOException, InputStream}
import java.nio.ByteBuffer

import scala.annotation.tailrec

trait Input {

  @throws[Input.InputException]
  def read(): Int

  @throws[Input.InputException]
  def readBytes(length: Int): ByteBuffer
}

object Input {

  case class InputException(errorType: ErrorType) extends Exception

  val Eof = -1

  def fromStream(in: InputStream) =
    new InputStreamInput(if (in.markSupported) in else new BufferedInputStream(in))
}

class InputStreamInput(in: InputStream) extends Input {

  def read() = try {
    in.read()
  } catch {
    case e: IOException => throw Input.InputException(InputFailure(e))
  }

  def readBytes(length: Int) = {
    val bytes = new Array[Byte](length)

    @tailrec
    def readIntoArray(offset: Int = 0): Int = {
      if (offset < length) {
        val count = try {
          in.read(bytes, offset, length - offset)
        } catch {
          case e: IOException => throw Input.InputException(InputFailure(e))
        }
        if (count > 0)
          readIntoArray(offset + count)
        else
          offset
      } else {
        offset
      }
    }

    val bytesRead = readIntoArray()

    if (bytesRead == length)
      ByteBuffer.wrap(bytes).asReadOnlyBuffer
    else
      throw Input.InputException(InsufficientInputData(expectedLength = length, actualLength = bytesRead))
  }
}
