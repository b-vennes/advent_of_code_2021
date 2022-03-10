package object typeclasses {
  trait Implicits extends ParserInstances with ShowInstances with MonoidInstances

  object implicits extends Implicits
}
