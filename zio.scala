class ZIO[R,E,A]
type UIO[A] = ZIO[Nothing, Nothing, A]

object ZIO:
  class WithZIO[A]: 
    def apply[B, C, D, R](fn: A => ZIO[D, B, C]): ZIO[R,B,C] = ???

  class WithPure[A]:
    def apply[B](fn: A => B): ZIO[A,Nothing, B] = ???
  def serviceWithZIO[A]: WithZIO[A] = ???
  def serviceWithPure[A]: WithPure[A] = ???
