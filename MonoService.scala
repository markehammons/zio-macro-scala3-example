import scala.quoted.*

trait MonoService[F]:
  def defs: Array[AnyRef]
  def get: MonoServiceBacking[F] = new MonoServiceBacking[F](defs)

object MonoService:
  inline def derived[F]: MonoService[F] = ${
    derivedImpl[F]()
  }

  private def getFn[F](name: String)(using Quotes, Type[F]): Expr[AnyRef] =
    import quotes.reflect.*

    val f = TypeRepr.of[F].classSymbol.get

    val methodSymbol = f.methodMember(name).head

    val (methodType, rt) = TypeRepr.of[F].memberType(methodSymbol) match
      case MethodType(names, types, rtypeRepr) =>
        val rt = rtypeRepr.dealias.asType match
          case '[UIO[r]] => TypeRepr.of[ZIO[F, Nothing, r]]
          case '[r]      => TypeRepr.of[ZIO[F, Nothing, r]]

        MethodType(names)(_ => types, _ => rt) -> rtypeRepr

    val lambda = Lambda
      .apply(
        Symbol.spliceOwner,
        methodType,
        (symbol, methodInputs) =>
          val zio = TypeRepr.of[ZIO.type].classSymbol.get

          val method = zio.methodMember("serviceWithZIO").head

          '{
            val zio = ZIO
            ${
              rt.dealias.widen.asType match
                case '[ZIO[Nothing, b, c]] =>
                  Apply(
                    TypeApply(
                      Select(
                        TypeApply(
                          Select('{ zio }.asTerm, method),
                          List(TypeTree.of[F])
                        ),
                        TypeRepr
                          .of[ZIO.WithZIO]
                          .classSymbol
                          .get
                          .declaredMethod("apply")
                          .head
                      ),
                      List(TypeTree.of[b], TypeTree.of[c])
                    ),
                    List(
                      Lambda(
                        symbol,
                        MethodType(List("f"))(
                          _ => List(TypeRepr.of[F]),
                          _ => rt
                        ),
                        (symbol, inputs) =>
                          Apply(
                            Select(inputs.head.asExpr.asTerm, methodSymbol),
                            methodInputs.map(_.asExpr.asTerm)
                          ).changeOwner(symbol)
                            .asExprOf[ZIO[Nothing, b, c]]
                            .asTerm
                      )
                    )
                  ).asExpr

                case '[t] => report.errorAndAbort(Type.show[t])
            }
          }.asTerm.changeOwner(symbol)
      )
      .asExprOf[AnyRef]

    lambda

  private def derivedImpl[F]()(using Quotes, Type[F]): Expr[MonoService[F]] =
    import quotes.reflect.*

    val exprs =
      TypeRepr.of[F].classSymbol.get.declaredMethods.map(_.name).map(getFn)

    val code = '{
      new MonoService[F]:
        val defs = Array(${ Varargs(exprs) }*)
    }

    report.warning(code.show)
    code

  transparent inline def instance[F](using s: MonoService[F]) = ${
    instanceImpl[F]('s)
  }

  private def instanceImpl[F](
      monoServiceExpr: Expr[MonoService[F]]
  )(using Type[F], Quotes): Expr[MonoServiceBacking[F]] =
    import quotes.reflect.*

    val symbol = TypeRepr.of[F].classSymbol.get
    val methodRepr = symbol.declaredMethods
      .map(TypeRepr.of[F].memberType)
      .map:
        case MethodType(names, inputs, returnType) =>
          val mt = returnType.dealias.asType match
            case '[ZIO[Nothing, b, c]] =>
              MethodType(names)(_ => inputs, _ => TypeRepr.of[ZIO[F, b, c]])

          report.warning(mt.show)
          mt

    val names = symbol.declaredMethods.map(_.name)

    val refined = names
      .zip(methodRepr)
      .foldLeft(TypeRepr.of[MonoServiceBacking[F]]):
        case (backing, (name, repr)) =>
          Refinement(backing, name, repr)

    //report.errorAndAbort(refined.show)
    refined.asType match
      case '[a] =>
        '{
          $monoServiceExpr.get.asInstanceOf[MonoServiceBacking[F] & a]
        }
