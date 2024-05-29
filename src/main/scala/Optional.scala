import scala.util.boundary, boundary.break, boundary.Label
import scala.util.boundary.Break
import undo._

object optional:
  inline def apply[T](inline body: Label[None.type] ?=> T): Option[T] =
    boundary(Some(body))

  extension [T](r: Option[T])
    inline def ? (using label: Label[None.type]): T = r match
      case Some(x) => x
      case None => break(None)
  
  inline def rollbackOnNone[T](inline body: UndoContext ?=> T)(using Label[None.type]): T =
    undo.rollbackOn[T]({
      case e : Break[None.type] => 
    }):
      body
