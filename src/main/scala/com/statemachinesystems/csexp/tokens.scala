package com.statemachinesystems.csexp

import java.nio.ByteBuffer

sealed trait Token

case object LParen extends Token

case object RParen extends Token

case class Atom(contents: ByteBuffer) extends Token

case class Error(errorType: ErrorType) extends Token

sealed trait ErrorType

case class IllegalInputByte(byte: Int) extends ErrorType {
  override def toString = s"Illegal input byte: $byte"
}

case object LeadingZeroInAtomLength extends ErrorType {
  override def toString = "Leading zeros not allowed in atom lengths"
}

case object LengthLimitExceeded extends ErrorType {
  override def toString = "Length limit exceeded"
}

case class IllegalByteInAtomLength(byte: Int) extends ErrorType {
  override def toString = s"Expected digit but saw byte: $byte"
}

case object UnexpectedEndOfInputInAtomLength extends ErrorType {
  override def toString = "Unexpected end of input in atom length"
}

case class InsufficientInputData(expectedLength: Int, actualLength: Int) extends ErrorType {
  override def toString = s"Read $actualLength from stream, required $expectedLength"
}

case class InputFailure(exception: Exception) extends ErrorType {
  override def toString = s"Input failure: $exception"
}