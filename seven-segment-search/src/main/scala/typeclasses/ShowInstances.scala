package typeclasses

import cats.Show
import models.{DebugEntry, Digit, SignalPattern, UniqueDigitSummary}

trait ShowInstances {
  implicit val debugEntryShow: Show[DebugEntry] = Show.fromToString

  implicit val digitShow: Show[Digit] = Show.fromToString

  implicit val signalPatternShow: Show[SignalPattern] = Show.fromToString

  implicit val uniqueDigitSummaryShow: Show[UniqueDigitSummary] = Show.fromToString

  implicit val signalPatternCreateFailureShow: Show[SignalPattern.CreateFailure] = Show.fromToString

  implicit val debugEntryCreateFailureShow: Show[DebugEntry.CreateFailure] = Show.fromToString
}

object ShowInstances {
  object instances extends ShowInstances
}
