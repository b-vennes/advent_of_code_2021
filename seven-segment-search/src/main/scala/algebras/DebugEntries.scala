package algebras

import cats.data.Chain
import models.DebugEntry

trait DebugEntries[F[_]] {
  def get: F[Chain[DebugEntry]]
}
