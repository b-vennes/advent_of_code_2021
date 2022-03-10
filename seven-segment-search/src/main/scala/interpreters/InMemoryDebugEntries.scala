package interpreters

import algebras.DebugEntries
import cats.Id
import cats.data.Chain
import models.DebugEntry

object InMemoryDebugEntries {
  def make(entries: Chain[DebugEntry]): DebugEntries[Id] = new DebugEntries[Id] {
    override def get: Id[Chain[DebugEntry]] = entries
  }
}
