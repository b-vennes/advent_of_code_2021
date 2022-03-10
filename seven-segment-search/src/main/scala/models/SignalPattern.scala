package models

import cats.implicits._
import cats.data.{Validated, ValidatedNec}

final case class SignalPattern private (letters: String) extends AnyVal

object SignalPattern {
  sealed trait CreateFailure

  object CreateFailures {
    final case object EmptySignal extends CreateFailure
    final case class InvalidLetters(letters: String) extends CreateFailure
  }

  // letters a-g are valid
  val ValidLetters = "abcdefgh"

  def create(letters: String): ValidatedNec[CreateFailure, SignalPattern] =
    (
      Validated.condNec(
        letters.nonEmpty,
        letters,
        CreateFailures.EmptySignal
      ),
      Validated.condNec(
        letters.forall(letter => ValidLetters.contains(letter)),
        letters,
        CreateFailures.InvalidLetters(letters)
      )
    ).mapN((_, _) => new SignalPattern(letters))
}
