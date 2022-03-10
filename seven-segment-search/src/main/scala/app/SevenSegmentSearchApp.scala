package app

import algebras._
import cats.effect.{IO, IOApp}
import cats.implicits._
import fs2.io.file.Path
import interpreters.StateDebugEntryAnalysis.AnalysisStateT
import interpreters._
import typeclasses.implicits._

object SevenSegmentSearchApp extends IOApp.Simple {

  type F[T] = IO[T]

  val inputFile: Path = Path("input.txt")

  val debugEntries: DebugEntries[F] = FileInputDebugEntries.make[F](inputFile)

  val analysis: DebugEntryAnalysis[AnalysisStateT[F, *]] = StateDebugEntryAnalysis.make[F]

  val publish: ResultPublish[F] = StdOutputResultPublish.make[F]

  override def run: IO[Unit] =
    for {
      entries <- debugEntries.get
      result <- entries.traverse_(analysis.analyzeEntry).runEmptyS
      _ <- publish.publish(result)
    } yield ()
}
