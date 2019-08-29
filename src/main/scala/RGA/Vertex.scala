package RGA

/**
  * the data structure used to represent an element of the list
  * @param timeStamp
  */

class Vertex (timeStamp: TimeStamp) extends Comparable[Vertex]{
  var timestamp:TimeStamp = timeStamp // time stamp when it was created
  var atom:Object = null // the data holder
  var next:Vertex = Bottom // the following vertices to this one

  def key :String = timestamp.KeyFromTimeStamp

  def >(vertex: Vertex):Boolean= this.timestamp > vertex.timestamp
  def <(vertex: Vertex):Boolean= this.timestamp < vertex.timestamp
  def ==(vertex: Vertex):Boolean= this.timestamp == vertex.timestamp

  override def compareTo(o: Vertex): Int = timestamp.compareTo(o.timestamp)

  override def toString: String = return key+" -> "+next.key
}

object Bottom extends Vertex(timeStamp = null){
  override def key: String = "RGA.Bottom"
  override def toString: String = return "RGA.Bottom"
}
