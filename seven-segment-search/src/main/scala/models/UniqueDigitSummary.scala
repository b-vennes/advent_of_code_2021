package models

case class UniqueDigitSummary(onesCounts: Long, foursCounts: Long, sevensCount: Long, eightsCount: Long)

object UniqueDigitSummary {
  def empty: UniqueDigitSummary =
    new UniqueDigitSummary(0, 0, 0, 0)
}
