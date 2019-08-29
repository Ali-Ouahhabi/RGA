import RGA.{RGA, TimeStamp, Vertex}
import akka.actor._

/**
  * Worker handling messages to add an element and remove it form the local  list
  * each add and remove command is done locally then broadcast it to the other known
  * actors in the buf: ArrayBuffer[ActorRef]
  */

case class addRef(actorRef: ActorRef)
case class report()
case class Message(timeStamp: TimeStamp,o: Object)

case class masterW(message: Message)
case class workerW(message: Message)

class Worker(ID: Int) extends Actor {
  import RGA.Messages._


  val state: RGA = new RGA(ID)
  var timeStamp:TimeStamp = new TimeStamp(ID,0)
  val buf = scala.collection.mutable.ArrayBuffer.empty[ActorRef]

  def addMaster(vertex: Vertex,o: Object): Unit ={
    timeStamp.inc()
    val C1:Int = timeStamp.Counter
    val tmp:Vertex = new Vertex(new TimeStamp(ID,C1))
    tmp.atom = o
    // println(" adding message "+o.toString)
    state.addRight(vertex,tmp)

    //broadcast addRight
    //Thread.sleep(2000)

    buf.foreach(x=>{x!addRightToW(ID,C1,o)
      //println(s"$ID, send to, ${x.path.name} val ${tmp.toString}")
    })
  }

  def addWorker(Id:Int,count:Int,o:Object): Unit ={
    //println(s"$ID, received from, ${sender().path.name} "+Id+":"+count)
    val C1:Int = timeStamp.Counter
    val tmp:Vertex = new Vertex(new TimeStamp(Id,count))
    tmp.atom = o
    val befor = new Vertex( new TimeStamp(0,0))
    // println(" adding message "+o.toString)
    state.addRight(befor,tmp)
  }

  override def receive: Receive = {

      case report()=>{
        var tmp = state.toString()
        sender ! tmp
      }
      case addRef(actorRef)=>{
          buf += actorRef
      }
      case  addRight (vertex,o)=>addMaster(vertex,o)
      case addRightToW (id,counter,o)=>
        //println(s"$ID,bfAW received from, ${sender().path.name} "+id+":"+counter)
        addWorker(id,counter,o)
      case  remove(vertex: Vertex)=>{
        state.remove(vertex)
        // broadcast remove
        buf.foreach(x=>x!remove(vertex))
      }
      case  lookup(vertex: Vertex)=>{
        sender ! state.lookup(vertex)
      }
      case  before (vertexL: Vertex,vertexR: Vertex)=>{
        sender ! state.before(vertexL,vertexR)
      }
      case  successor (vertex: Vertex)=>{
        sender ! state.successor(vertex)
      }

  }
}
