class ZIO[R,E,A]
type UIO[A] = ZIO[Nothing, Nothing, A]

object ZIO:
  class WithZIO[A]: 
    def apply[B, C, D, R](fn: A => ZIO[D, B, C]): ZIO[R,B,C] = 
      println("called ZIO")
      ZIO[R,B,C]

  class WithPure[A]:
    def apply[B](fn: A => B): ZIO[A,Nothing, B] = 
      println("called pure")
      ZIO[A,Nothing,B]
  def serviceWithZIO[A]: WithZIO[A] = WithZIO[A]
  def serviceWithPure[A]: WithPure[A] = WithPure[A]
