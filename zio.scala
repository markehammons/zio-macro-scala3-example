class ZIO[R,E,A]
type UIO[A] = ZIO[Nothing, Nothing, A]

object ZIO:
  class WithZIO[A]: 
    def apply[B, C](fn: A => ZIO[Nothing, B, C]): ZIO[A,B,C] = ???
  def serviceWithZIO[A]: WithZIO[A] = ???

trait ServiceA derives MonoService: 
  def method(input: Int): UIO[Unit]


val vv: ZIO[ServiceA, Nothing, Unit] = MonoService.instance[ServiceA].method(5)

def x: MonoServiceBacking[ServiceA] { def method(input: Int): ZIO[ServiceA, Nothing, Unit]} = ???

val y = x.method(4)