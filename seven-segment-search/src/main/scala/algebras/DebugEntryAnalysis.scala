package algebras

import models.{DebugEntry, UniqueDigitSummary}

trait DebugEntryAnalysis[F[_]] {
  def analyzeEntry(entry: DebugEntry): F[Unit]

  def getUniqueDigitSummary: F[UniqueDigitSummary]
}
