package interpreters

import cats.implicits._
import algebras.DebugEntries
import cats.data.Chain
import cats.effect.IO
import cats.effect.kernel.Concurrent
import fs2.io.file.{Files, Path}
import fs2.{Stream, text}
import models.DebugEntry
import typeclasses.implicits.debugEntryParser

object FileInputDebugEntries {
  def make[F[_]: Files: Concurrent](inputFile: Path): DebugEntries[F] = new Impl[F](inputFile)

  private class Impl[F[_]: Files: Concurrent](inputFile: Path) extends DebugEntries[F] {
    override def get: F[Chain[DebugEntry]] =
      (for {
        inputText <-
          Files[F]
            .readAll(inputFile)
            .through(text.utf8.decode)
        entryText <- Stream.emits(inputText.split("\n"))
        entryResult = debugEntryParser.parse(entryText).valueOr(error => throw new Throwable(error.toString))
      } yield entryResult._2).compile.toList.map(Chain.fromSeq)
  }
}
