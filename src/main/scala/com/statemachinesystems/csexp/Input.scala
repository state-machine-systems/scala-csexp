package com.statemachinesystems.csexp

import java.io.{BufferedInputStream, InputStream}
import java.nio.ByteBuffer

import scala.annotation.tailrec

trait Input {

  def read(): Int

  def readBytes(length: Int): Either[ErrorType, ByteBuffer]
}

object Input {

  val Eof = -1

  def fromStream(in: InputStream) =
    new InputStreamInput(if (in.markSupported) in else new BufferedInputStream(in))
}

class InputStreamInput(in: InputStream) extends Input {

  def read() =
    in.read()

  def readBytes(length: Int) = {
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
      Right(ByteBuffer.wrap(bytes).asReadOnlyBuffer)
    else
      Left(InsufficientInputData(expectedLength = length, actualLength = bytesRead))
  }
}
