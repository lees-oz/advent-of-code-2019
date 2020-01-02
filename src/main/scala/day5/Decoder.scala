package day5

import cats.effect.IO

trait Decoder[A] {
  def decode(s: State): IO[A]
}

object Decoder {
  def apply[A](implicit d: Decoder[A]): Decoder[A] = implicitly[Decoder[A]]
}