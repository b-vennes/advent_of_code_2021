package algebras

import models.UniqueDigitSummary

trait ResultPublish[F[_]] {
  def publish(solution: UniqueDigitSummary): F[Unit]
}
