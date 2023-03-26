import cats.*
import cats.implicits.*

sealed trait Ctx[R[_], A]:
  val run: R[A]
  def map[B](f: A => B): Ctx[R, B]
  def flatMap[B, R1[_]](f: A => Ctx[R1, B]): Ctx[[t] =>> R[R1[t]], B]

final case class Pure[A](val run: A) extends Ctx[Id, A]:
  override def map[B](f: A => B): Ctx[Id, B] = Pure(f(run))
  override def flatMap[B, R1[_]](f: A => Ctx[R1, B]): Ctx[R1, B] = f(run)

final class Join[Arg, R[_], A](val inner: Arg ?=> Ctx[R, A])
    extends Ctx[[x] =>> Arg ?=> R[x], A]:
  val run = inner.run
  override def map[B](f: A => B): Ctx[[x] =>> (Arg) ?=> R[x], B] = Join(
    inner.map(f)
  )
  override def flatMap[B, R1[_]](
      f: A => Ctx[R1, B]
  ): Ctx[[t] =>> (Arg) ?=> R[R1[t]], B] =
    Join(inner.flatMap(f))
