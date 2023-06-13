//>using scala "3.3.0"
import scala.quoted.*


def getReturnTyp[F](using q: Quotes)(in: q.reflect.TypeRepr)(using Type[F]): q.reflect.TypeRepr = 
  import quotes.reflect.*
  in.dealias.asType match 
    case '[ZIO[Nothing, b,c]] => TypeRepr.of[ZIO[F,b,c]]
    case '[ZIO[a,b,c]] => TypeRepr.of[ZIO[a & F, b, c]]
    case '[c] => TypeRepr.of[ZIO[F,Nothing,c]]
class MonoServiceBacking[F](defs: Array[AnyRef]) extends Selectable:
  transparent inline def applyDynamic(
      inline name: String,
      inline typs: Class[?]*
  )(inline args: Any*): Any = ${
    MonoServiceBacking.applyDynamicImpl[F]('name, 'args, 'defs)
  }

object MonoServiceBacking:
  def applyDynamicImpl[A](
      name: Expr[String],
      args: Expr[Seq[Any]],
      fns: Expr[Array[AnyRef]]
  )(using Quotes, Type[A]): Expr[Any] =
    import quotes.reflect.*

    import scala.compiletime.asMatchable

    val methodSymbol =
      TypeRepr.of[A].classSymbol.get.declaredMethod(name.valueOrAbort).head
    val methodPositionExpr =
      Expr(
        TypeRepr
          .of[A]
          .classSymbol
          .get
          .declaredMethods
          .sortBy(_.name)
          .indexOf(methodSymbol)
      )

    val (fnType, rt) = TypeRepr.of[A].memberType(methodSymbol).asMatchable match
      case MethodType(_, types, rtypeRepr) =>
        val rt = rtypeRepr.asType match
          case '[UIO[r]] => TypeRepr.of[ZIO[A, Nothing, r]]
          case '[r]      => TypeRepr.of[ZIO[A, Nothing, r]]

        Symbol
          .classSymbol(s"scala.Function${types.size}")
          .typeRef
          .appliedTo(types :+ rt) -> rt

    val fnTerm =
      fnType.asType match
        case '[a] =>
          '{ $fns($methodPositionExpr).asInstanceOf[a] }.asTerm

    val inputs = args match
      case Varargs(inputs) => inputs.map(_.asTerm).toList

    val code =
      ValDef.let(Symbol.spliceOwner, inputs): refs =>
        Apply(
          Select(fnTerm, fnType.classSymbol.get.declaredMethod("apply").head),
          refs
        )

    val expr = rt.asType match
      case '[r] =>
        code.asExprOf[r]

    report.info(expr.show)
    expr

