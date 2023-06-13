
class ServiceB
trait ServiceA derives MonoService: 
  def method(input: Int): UIO[Unit]
  def method2(input: Int): Unit
  def method3(input: Int): ZIO[ServiceB, Throwable, Unit]


val vv: ZIO[ServiceA, Nothing, Unit] = MonoService.instance[ServiceA].method(5)
val vvv: ZIO[ServiceA, Nothing, Unit] = MonoService.instance[ServiceA].method2(4)
val vvvv: ZIO[ServiceA&ServiceB, Throwable, Unit] = MonoService.instance[ServiceA].method3(4)
