import scala.quoted.*

trait MonoService[F]:
  def defs: Array[AnyRef]
  def get: MonoServiceBacking[F] = new MonoServiceBacking[F](defs)

object MonoService:
  inline def derived[F]: MonoService[F] = ${
    derivedImpl[F]()
  }

  private def callServiceWithZIO[A, B, C, F, R](using
      q: Quotes
  )(using Type[A], Type[B], Type[C], Type[F], Type[R])(
      methodInputs: List[q.reflect.Term],
      owner: q.reflect.Symbol,
      methodSymbol: q.reflect.Symbol
  ): Expr[ZIO[R, B, C]] =
    import quotes.reflect.*
    val zio = TypeRepr.of[ZIO.type].classSymbol.get

    val method = zio.methodMember("serviceWithZIO").head

    '{
      val zio = ZIO
      ${
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
            List(TypeTree.of[B], TypeTree.of[C], TypeTree.of[A], TypeTree.of[R])
          ),
          List(
            Lambda(
              owner,
              MethodType(List("f"))(
                _ => List(TypeRepr.of[F]),
                _ => TypeRepr.of[ZIO[A, B, C]]
              ),
              (symbol, inputs) =>
                Apply(
                  Select(inputs.head.asExpr.asTerm, methodSymbol),
                  methodInputs.map(_.asExpr.asTerm)
                ).changeOwner(symbol)
                  .asExprOf[ZIO[A, B, C]]
                  .asTerm
            )
          )
        ).asExprOf[ZIO[R, B, C]]
      }
    }

  private def callServiceWithPure[A, F](using
      q: Quotes
  )(using Type[A], Type[F])(
      methodInputs: List[q.reflect.Term],
      owner: q.reflect.Symbol,
      methodSymbol: q.reflect.Symbol
  ): Expr[AnyRef] =
    import quotes.reflect.*
    val zio = TypeRepr.of[ZIO.type].classSymbol.get

    val method = zio.methodMember("serviceWithPure").head

    val lambda = Lambda(
      owner,
      MethodType(List("f"))(
        _ => List(TypeRepr.of[F]),
        _ => TypeRepr.of[A]
      ),
      (symbol, inputs) =>
        Apply(
          Select(inputs.head.asExpr.asTerm, methodSymbol),
          methodInputs
        ).changeOwner(symbol).asExprOf[A].asTerm
    )

    '{
      val zio = ZIO

      ${
        Apply(
          TypeApply(
            Select(
              TypeApply(
                Select('{ zio }.asTerm, method),
                List(TypeTree.of[F])
              ),
              TypeRepr
                .of[ZIO.WithPure]
                .classSymbol
                .get
                .declaredMethod("apply")
                .head
            ),
            List(TypeTree.of[A])
          ),
          List(
            lambda
          )
        ).asExprOf[AnyRef]
      }
    }

  private def getFn[F](name: String)(using Quotes, Type[F]): Expr[AnyRef] =
    import quotes.reflect.*

    val f = TypeRepr.of[F].classSymbol.get

    val methodSymbol = f.methodMember(name).head

    val (methodType, ort, crt) = TypeRepr.of[F].memberType(methodSymbol) match
      case MethodType(names, types, rtypeRepr) =>
        val rt = rtypeRepr.dealias.asType match
          case '[ZIO[Nothing, b, c]] => TypeRepr.of[ZIO[F, b, c]]
          case '[ZIO[a, b, c]]       => TypeRepr.of[ZIO[F & a, b, c]]
          case '[r]                  => TypeRepr.of[ZIO[F, Nothing, r]]

        (MethodType(names)(_ => types, _ => rt), rtypeRepr, rt)

    val lambda = Lambda
      .apply(
        Symbol.spliceOwner,
        methodType,
        (symbol, methodInputs) =>
          val methodTerms = methodInputs.map(_.asExpr.asTerm)
          (ort.dealias.asType, crt.asType) match
            case ('[ZIO[a, b, c]], '[ZIO[d,?,?]]) =>
              callServiceWithZIO[a, b, c, F, d](
                methodTerms,
                symbol,
                methodSymbol
              ).asTerm.changeOwner(symbol)
            case ('[a], _) => 
              callServiceWithPure[a, F](methodTerms, symbol, methodSymbol).asTerm.changeOwner(symbol)
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
          val mt =
            MethodType(names)(_ => inputs, _ => getReturnTyp[F](returnType))

          report.warning(mt.show)
          mt

    val names = symbol.declaredMethods.map(_.name)

    val refined = names
      .zip(methodRepr)
      .foldLeft(TypeRepr.of[MonoServiceBacking[F]]):
        case (backing, (name, repr)) =>
          Refinement(backing, name, repr)

    // report.errorAndAbort(refined.show)
    refined.asType match
      case '[a] =>
        '{
          $monoServiceExpr.get.asInstanceOf[MonoServiceBacking[F] & a]
        }
