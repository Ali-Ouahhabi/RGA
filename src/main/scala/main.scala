import RGA.{TimeStamp, Vertex}
import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import akka.pattern.ask
import scala.language.postfixOps

import scala.concurrent.Await
import scala.concurrent.duration._

object Master {
  import RGA.Messages._


  def main(args: Array[String]): Unit = {

  implicit val timeout = Timeout(5 seconds)
  val system = ActorSystem("ZZZ")

    val W1 = system.actorOf(Props(new Worker(1)), name = "1")
    val W2 = system.actorOf(Props(new Worker(2)), name = "2")
    val W3 = system.actorOf(Props(new Worker(3)), name = "3")
    val W4 = system.actorOf(Props(new Worker(4)), name = "4")
    W1 ! addRef(W2)
    W1 ! addRef(W3)
    W1 ! addRef(W4)

    W2 ! addRef(W1)
    W2 ! addRef(W3)
    W2 ! addRef(W4)

    W3 ! addRef(W2)
    W3 ! addRef(W1)
    W3 ! addRef(W4)

    W4 ! addRef(W2)
    W4 ! addRef(W3)
    W4 ! addRef(W1)


    W1 ! addRight(new Vertex( new TimeStamp(0,0)) , "AA1")
    W2 ! addRight(new Vertex( new TimeStamp(0,0)) , "AA2")
    W1 ! addRight(new Vertex( new TimeStamp(0,0)) , "AB1")
    W2 ! addRight(new Vertex( new TimeStamp(0,0)) , "AB2")
    W1 ! addRight(new Vertex( new TimeStamp(0,0)) , "AC1")
    W2 ! addRight(new Vertex( new TimeStamp(0,0)) , "AC2")
    W1 ! addRight(new Vertex( new TimeStamp(0,0)) , "AD1")
    W2 ! addRight(new Vertex( new TimeStamp(0,0)) , "AD2")
    W1 ! addRight(new Vertex( new TimeStamp(0,0)) , "AE1")
    W2 ! addRight(new Vertex( new TimeStamp(0,0)) , "AE2")
    //Thread.sleep(3000)
    W3 ! addRight(new Vertex( new TimeStamp(0,0)) , "AA3")
   // Thread.sleep(3000)
    W4 ! addRight(new Vertex( new TimeStamp(0,0)) , "AA4")

    Thread.sleep(3000)
    println("*******************************************************************************************")
    println("sending get val ")
    println("*******************************************************************************************")

    val F1 = W1 ? report()
    val F2 = W2 ? report()
    val F3 = W3 ? report()
    val F4 = W4 ? report()

    val R1 = Await.result(F1, timeout.duration )
    val R2 = Await.result(F2, timeout.duration )
    val R3 = Await.result(F3, timeout.duration )
    val R4 = Await.result(F4, timeout.duration)

    println(s" recived from W1 $R1")
    println(s" recived from W2 $R2")
    println(s" recived from W3 $R3")
    println(s" recived from W4 $R4")
  }
}
