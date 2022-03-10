package typeclasses

import cats.Monoid
import models.UniqueDigitSummary

trait MonoidInstances {
  implicit val uniqueDigitSummaryMonoid: Monoid[UniqueDigitSummary] =
    Monoid.instance(
      UniqueDigitSummary.empty,
      (x, y) =>
        UniqueDigitSummary(
          x.onesCounts + y.onesCounts,
          x.foursCounts + y.foursCounts,
          x.sevensCount + y.sevensCount,
          x.eightsCount + y.eightsCount
        )
    )
}

object MonoidInstances {
  object instances extends MonoidInstances
}
