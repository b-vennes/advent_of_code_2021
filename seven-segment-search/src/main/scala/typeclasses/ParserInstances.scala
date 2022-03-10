package typeclasses

import cats.data.NonEmptyChain
import cats.implicits._
import cats.parse.Parser
import cats.parse.Rfc5234.{alpha, sp}
import models.{DebugEntry, SignalPattern}
import typeclasses.ShowInstances.instances._

trait ParserInstances {
  implicit val signalPatternParser: Parser[SignalPattern] =
    alpha.rep.string
      .flatMap(letters =>
        SignalPattern
          .create(letters)
          .map(Parser.pure)
          .valueOr(failure => Parser.failWith(failure.show))
      )

  implicit val debugEntryParser: Parser[DebugEntry] = {
    ((signalPatternParser ~ sp).rep ~ Parser.string("|") ~ (sp ~ signalPatternParser).rep)
      .flatMap(parsedValues =>
        DebugEntry
          .create(
            NonEmptyChain.fromNonEmptyList(parsedValues._1._1.map(_._1)),
            NonEmptyChain.fromNonEmptyList(parsedValues._2.map(_._2))
          )
          .map(Parser.pure)
          .valueOr(failure => Parser.failWith(failure.show))
      )
  }
}
