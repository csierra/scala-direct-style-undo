import undo._
import org.scalatest._
import flatspec._
import matchers._
import scala.concurrent.*
import scala.concurrent.duration.*
import undoEventually._
import scala.collection.mutable.ListBuffer
import ExecutionContext.Implicits.global
import java.util.concurrent.{TimeUnit, CountDownLatch}

class FutureUndoSpec extends AnyFlatSpec with should.Matchers {
  "compensateEventually" should "wait for the future and execute compensators when a error happens outside the Future" in {
    val effects = ListBuffer[String]()
    val l1 = new CountDownLatch(1)
    val l2 = new CountDownLatch(1)
    val completed = new CountDownLatch(1)
    try {
      rollbackOnError[Unit]:
        val f1 = Future {
          l1.await(1, TimeUnit.SECONDS)
          effects.append("+effect")
        } compensateEventually {
          case _ => effects.append("-effect")
        }
        Future {
          l2.await(1, TimeUnit.SECONDS)
          effects.append("+effect 2")
          completed.countDown
        } compensateEventually {
          case _ => 
            effects.append("-effect 2")
        }
        l1.countDown()
        Await.result(f1, 1.second)
        //The second future is "registered" for compensation, but hasn't completed yet
        effects.toList shouldBe List("+effect")
        try {
          throw new IllegalStateException("forced error")
        }
        finally {
          l2.countDown()
        }
    }
    catch {
      case e:IllegalStateException =>
    }
    completed.await(1, TimeUnit.SECONDS)
    //Since the second future was registered it waits for its completion and executes effects in order
    effects shouldBe List("+effect", "+effect 2", "-effect 2", "-effect")
  }
  "compensateEventually" should "wait for the future and execute compensators when a error happens outside the Future also in reverse order" in {
    val effects = ListBuffer[String]()
    val l1 = new CountDownLatch(1)
    val l2 = new CountDownLatch(1)
    val completed = new CountDownLatch(1)
    try {
      rollbackOnError[Unit]:
        Future {
          l1.await(1, TimeUnit.SECONDS)
          effects.append("+effect")
          completed.countDown
        } compensateEventually {
          case _ => effects.append("-effect")
        }
        val f2 = Future {
          l2.await(1, TimeUnit.SECONDS)
          effects.append("+effect 2")
        } compensateEventually {
          case _ => 
            effects.append("-effect 2")
        }
        l2.countDown()
        Await.result(f2, 1.second)
        //The second future is "registered" for compensation, but hasn't completed yet
        effects.toList shouldBe List("+effect 2")
        try {
          throw new IllegalStateException("forced error")
        }
        finally {
          l1.countDown()
        }
    }
    catch {
      case e:IllegalStateException =>
    }
    completed.await(1, TimeUnit.SECONDS)
    //Since the second future was registered it waits for its completion and executes effects in order
    effects shouldBe List("+effect 2", "+effect", "-effect 2", "-effect")
  }
  "compensateEventually" should "wait for the future and execute compensators when a error happens insiede the Future" in {
    val effects = ListBuffer[String]()
    val l1 = new CountDownLatch(1)
    val l2 = new CountDownLatch(1)
    val completed = new CountDownLatch(1)
    try {
      rollbackOnError:
        val f1 = Future {
          l1.await(1, TimeUnit.SECONDS)
          effects.append("+effect")
          throw new IllegalStateException("forced error inside the future")
        } compensateEventually {
          case _ => effects.append("-effect")
        }
        Future {
          l2.await(1, TimeUnit.SECONDS)
          effects.append("+effect 2")
          completed.countDown
        } compensateEventually {
          case _ => effects.append("-effect 2")
        }
        l1.countDown()
        try {
          Await.result(f1, 1.second)
        } 
        finally {
        //The second future is "registered" for compensation, but hasn't completed yet
          effects.toList shouldBe List("+effect")
          l2.countDown
        }
    }
    catch {
      case e:IllegalStateException =>
    }
    completed.await(1, TimeUnit.SECONDS)
    effects shouldBe List("+effect", "+effect 2", "-effect 2", "-effect")
  }
  "compensateEventually" should "wait for the future and execute compensators when a error happens insiede the Future also in reverse order" in {
    val effects = ListBuffer[String]()
    val l1 = new CountDownLatch(1)
    val l2 = new CountDownLatch(1)
    val completed = new CountDownLatch(1)
    try {
      rollbackOnError:
        Future {
          l1.await(1, TimeUnit.SECONDS)
          effects.append("+effect")
          completed.countDown
        } compensateEventually {
          case _ => effects.append("-effect")
        }
        val f2 = Future {
          l2.await(1, TimeUnit.SECONDS)
          effects.append("+effect 2")
          throw new IllegalStateException("forced error inside the future")
        } compensateEventually {
          case _ => effects.append("-effect 2")
        }
        l2.countDown()
        try {
          Await.result(f2, 1.second)
        } 
        finally {
        //The second future is "registered" for compensation, but hasn't completed yet
          effects.toList shouldBe List("+effect 2")
          l1.countDown
        }
    }
    catch {
      case e:IllegalStateException =>
    }
    completed.await(1, TimeUnit.SECONDS)
    effects shouldBe List("+effect 2", "+effect", "-effect 2", "-effect")
  }
}
