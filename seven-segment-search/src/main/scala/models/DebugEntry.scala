package models

import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.implicits._

final case class DebugEntry private (
    signals: NonEmptyChain[SignalPattern],
    outputs: NonEmptyChain[SignalPattern]
)

object DebugEntry {
  sealed trait CreateFailure
  object CreateFailures {
    final case class InvalidSignalsLength(signals: NonEmptyChain[SignalPattern]) extends CreateFailure
    final case class InvalidOutputsLength(outputs: NonEmptyChain[SignalPattern]) extends CreateFailure
  }

  val SignalLength = 10
  val OutputLength = 4

  def create(
      signals: NonEmptyChain[SignalPattern],
      outputs: NonEmptyChain[SignalPattern]
  ): ValidatedNec[CreateFailure, DebugEntry] =
    (
      Validated.condNec(
        signals.length === SignalLength,
        signals,
        CreateFailures.InvalidSignalsLength(signals)
      ),
      Validated.condNec(
        outputs.length === OutputLength,
        outputs,
        CreateFailures.InvalidOutputsLength(outputs)
      )
    ).mapN((signals, outputs) => new DebugEntry(signals, outputs))
}
