package interpreters

import cats.implicits._
import algebras.DebugEntryAnalysis
import cats.Applicative
import cats.data.{State, StateT}
import models.{DebugEntry, UniqueDigitSummary}

object StateDebugEntryAnalysis {
  type AnalysisStateT[F[_], T] = StateT[F, UniqueDigitSummary, T]

  def make[F[_]: Applicative]: DebugEntryAnalysis[AnalysisStateT[F, *]] = new Impl[F]

  private class Impl[F[_]: Applicative] extends DebugEntryAnalysis[AnalysisStateT[F, *]] {
    override def analyzeEntry(entry: DebugEntry): AnalysisStateT[F, Unit] =
      StateT.modify(summary =>
        UniqueDigitSummary(
          summary.onesCounts + entry.outputs.filter(_.letters.length === 2).length,
          summary.foursCounts + entry.outputs.filter(_.letters.length === 4).length,
          summary.sevensCount + entry.outputs.filter(_.letters.length === 3).length,
          summary.eightsCount + entry.outputs.filter(_.letters.length === 7).length
        )
      )

    override def getUniqueDigitSummary: AnalysisStateT[F, UniqueDigitSummary] = StateT.get
  }
}
