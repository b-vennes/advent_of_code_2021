package models

import cats.data.{Validated, ValidatedNec}

case class Digit private (value: Short) extends AnyVal

object Digit {
  sealed trait DigitCreateFailure

  object DigitCreateFailures {
    case class InvalidDigit(value: Short) extends DigitCreateFailure
  }

  def create(value: Short): ValidatedNec[DigitCreateFailure, Digit] =
    Validated.condNec(0 <= value && value <= 9, new Digit(value), DigitCreateFailures.InvalidDigit(value))
}
