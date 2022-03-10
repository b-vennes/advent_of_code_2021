package interpreters

import cats.implicits._
import algebras.ResultPublish
import cats.Applicative
import models.UniqueDigitSummary

object StdOutputResultPublish {
  def make[F[_]: Applicative]: ResultPublish[F] = new Impl[F]

  private class Impl[F[_]: Applicative] extends ResultPublish[F] {
    override def publish(solution: UniqueDigitSummary): F[Unit] =
      (show"Counts of 1, 4, 7, and 8: " +
        show"${solution.onesCounts + solution.foursCounts + solution.sevensCount + solution.eightsCount}")
        .pure[F]
        .map(println)
  }
}
