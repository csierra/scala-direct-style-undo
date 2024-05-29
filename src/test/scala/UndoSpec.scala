import undo._
import org.scalatest._
import flatspec._
import matchers._
import scala.collection.mutable.ListBuffer

class UndoSpec extends AnyFlatSpec with should.Matchers {

  "undo.rollbackOnError" should "not execute compensators when there are no errors" in {
    val effects = ListBuffer[String]()
    rollbackOnError[Unit]:
      effects.append("+effect") compensate:
        case _ => effects.append("-effect")
      effects.toList shouldBe List("+effect")
    effects.toList shouldBe List("+effect")
  }

  "undo.rollbackOnError" should "execute compensators when a error happens" in {
    val effects = ListBuffer[String]()
    try {
      rollbackOnError[Unit]:
        effects.append("+effect") compensate:
          case _ => effects.append("-effect")
        effects.toList shouldBe List("+effect")
        throw new IllegalStateException("forced error")
    }
    catch {
      _ =>
    }
    effects shouldBe List("+effect", "-effect")
  }

  "undo" should "allow to create subtransactions in methods via (using UndoContext)" in {
    val effects = ListBuffer[String]()
    def subTransaction(using UndoContext) =
      effects.append("+subTransaction effect") compensate:
        case _ => effects.append("-subTransaction effect")

    try {
      rollbackOnError[Unit]:
        effects.append("+effect") compensate:
          case _ => effects.append("-effect")
        subTransaction
        effects.toList shouldBe List("+effect", "+subTransaction effect")
        effects.append("+effect 2") compensate:
          case _ => effects.append("-effect 2")
        effects.toList shouldBe List("+effect", "+subTransaction effect", "+effect 2")
        throw new IllegalStateException("forced error")
    }
    catch {
      _ =>   
    }
    effects.toList shouldBe List("+effect", "+subTransaction effect", "+effect 2", "-effect 2", "-subTransaction effect", "-effect")
  }

  "undo" should "allow to run subtransactions in methods isolated from outer transactions" in {
    val effects = ListBuffer[String]()
    def subTransaction(using UndoContext) =
      effects.append("+subTransaction effect") compensate:
        case _ => effects.append("-subTransaction effect")

    try {
      rollbackOnError[Unit]:
        effects.append("+effect") compensate:
          case _ => effects.append("-effect")
        rollbackOnError:
          subTransaction
        effects.toList shouldBe List("+effect", "+subTransaction effect")
        effects.append("+effect 2") compensate:
          case _ => effects.append("-effect 2")
        effects.toList shouldBe List("+effect", "+subTransaction effect", "+effect 2")
        throw new IllegalStateException("forced error")
    }
    catch {
      _ =>   
    }
    effects.toList shouldBe List("+effect", "+subTransaction effect", "+effect 2", "-effect 2", "-effect")
  }
  "errors in subtransactions" should "rollback the outer transaction" in {
    val effects = ListBuffer[String]()
    def subTransaction(using UndoContext): Unit =
      effects.append("+subTransaction effect") compensate:
        case _ => effects.append("-subTransaction effect")
      throw new IllegalStateException("forced subtransaction error")

    try {
      rollbackOnError[Unit]:
        effects.append("+effect") compensate:
          case _ => effects.append("-effect")
        subTransaction
        effects.toList shouldBe List("+effect", "+subTransaction effect")
        effects.append("+effect 2") compensate:
          case _ => effects.append("-effect 2")
        effects.toList shouldBe List("+effect", "+subTransaction effect", "+effect 2")
    }
    catch {
      _ =>   
    }
    effects.toList shouldBe List("+effect", "+subTransaction effect", "-subTransaction effect", "-effect")
  }
  "errors in isolated subtransactions" should "rollback the outer transaction" in {
    val effects = ListBuffer[String]()
    def subTransaction(using UndoContext): Unit =
      effects.append("+subTransaction effect") compensate:
        case _ => effects.append("-subTransaction effect")
      throw new IllegalStateException("forced subtransaction error")

    try {
      rollbackOnError[Unit]:
        effects.append("+effect") compensate:
          case _ => effects.append("-effect")
        rollbackOnError:
          subTransaction
        effects.toList shouldBe List("+effect", "+subTransaction effect")
        effects.append("+effect 2") compensate:
          case _ => effects.append("-effect 2")
        effects.toList shouldBe List("+effect", "+subTransaction effect", "+effect 2")
    }
    catch {
      _ =>   
    }
    effects.toList shouldBe List("+effect", "+subTransaction effect", "-subTransaction effect", "-effect")
  }
  "compensators" should "execute even if one of them fails" in {
    val effects = ListBuffer[String]()
    try {
      rollbackOnError[Unit]:
        effects.append("+effect") compensate:
          case _ => effects.append("-effect")
        effects.append("+effect 2") compensate:
          case _ => throw new IllegalStateException("compensator error")
        effects.append("+effect 3") compensate:
          case _ => effects.append("-effect 3")
        effects.toList shouldBe List("+effect", "+effect 2", "+effect 3")
        throw new IllegalStateException("forced error")
    }
    catch {
      _ =>   
    }
    effects.toList shouldBe List("+effect", "+effect 2", "+effect 3", "-effect 3", "-effect")
  }

}

