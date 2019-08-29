package RGA

import scala.collection.mutable


object Messages {
  case class addRight (val vertex: Vertex,val o: Object)
  //case class addRightToW (val vertex2: RGA.Vertex,val vertexNew2: RGA.Vertex)
  case class addRightToW (val ID:Int,val counter:Int,val o:Object)
  case class remove(val vertex: Vertex)
  case class lookup(val vertex: Vertex)
  case class before (val vertexL: Vertex,val vertexR: Vertex)
  case class successor (val vertex: Vertex)
}

/**
  * Rga class define a modest implementation of the replicated growable array CRDT
  * it defined by two sets VA the added element, VR the removed element and Echain
  * the head of the linked list
  * @param ID Id of the worker it'was added for debugging purpose it can be removed
  */

class RGA (ID: Int){
  // Sets of vertices defined as a hash map with timestamp as key and vertices as value
  /**
    * VA and VR are Set defined ase hashMap the keys are the time stamp of the corresponding vertex
    */
  var VA:mutable.HashMap[String,Vertex] = new mutable.HashMap[String,Vertex]()
  var VR:mutable.HashMap[String,Vertex] = new mutable.HashMap[String,Vertex]()

  /**
    *  EChain is the start point of the chain of the Vertices
    */
  var EChain:Vertex = new Vertex( new TimeStamp(0,0))

  VA+=(EChain.key -> EChain)// adding  the head of the list to VA
  VA+=(Bottom.key -> Bottom) //Bottom represent a null vertex to de fine the last element of the list

  /**
    *
    * @param vertex
    * @return true if vertex belong to VA and not VR
    */
  def lookup(vertex: Vertex): Boolean =
    if(VR.contains(vertex.key)) return false
    else if (VA.contains(vertex.key)) return true
    else return false

  // never used
  /**
    *
    * @param vertexL
    * @param vertexR
    * @return True if there is a path from vertexL to vertexR
    */
  def before(vertexL: Vertex, vertexR: Vertex):Boolean = {
    var vertexLtmp = vertexL
    if(lookup(vertexLtmp)&&lookup(vertexR)&&vertexLtmp.key!=vertexR.key)
      while(vertexLtmp.next!=null){
        if(vertexLtmp.next.key==vertexR.key) return true
        else vertexLtmp = vertexLtmp.next
      }
    return false
  }


  /**
    *
    * @param vertex
    * @return return the following vertex after vertex
    */
  def successor(vertex: Vertex):Vertex = if(lookup(vertex)) return vertex.next else return null
// never used
  def decompose(vertex: Vertex):Tuple2[Object,TimeStamp] = (vertex.atom,vertex.timestamp)

  /**
    * Adding newVertex after vertex the order is done depending on the time stamp of newVertex
    * @param vertex
    * @param newVertex
    * @return
    */
  def addRight(vertex: Vertex,newVertex: Vertex): Boolean ={
    //println("\n")
    var string = this.tostring2()
    //println(s"$ID, pre state, $string")
    //println(s"$ID, addRight,  "+newVertex.toString)
    if(lookup(vertex) && !lookup(newVertex)){
      //println(s"$ID, getting vertex with key ${vertex.key}")
      var l = VA.get(vertex.key).get
      //println(s"$ID, successor of l ${successor(l).key}")
      var r = VA.get(successor(l).key).get

      //println(s"$ID,pre, [l : ${l.toString}] [new : ${newVertex.toString}] [r : ${r.toString}] ")

      while (r!=Bottom && r < newVertex) {
        l = r
        r = successor(l)
        //println(s"$ID,in, l : ${l.toString}  r : ${r.toString}")
      }
      //println(s"$ID, post, to initialization [L.next =  ${l.next} ] [ newvertex  =$newVertex ]  [R = $r ]")
      l.next = newVertex
      //println(s"$ID, post, initialization step 1 [L.next =  ${l.next} ] [ newvertex  =$newVertex ]  [R = $r ]")
      newVertex.next=r
      //println(s"$ID, post, initialization End [L.next =  ${l.next} ] [ newvertex  =$newVertex ]  [R = $r ] ")

      //println(s"$ID,post, [l : ${l.toString}] [new : ${newVertex.toString}] [r : ${r.toString}]")

      VA +=(newVertex.key -> newVertex)
      var string = this.tostring2()
      //println(s"$ID, post state, $string")

      return true
    }else return false
  }

  /**
    *
    * @param vertex
    * @return
    */
  def remove(vertex: Vertex): Boolean ={
    if(lookup(vertex)){
      VR += (vertex.key -> vertex)
      VA -= (vertex.key)
      return true
    }else return false
  }

  def tostring2(): String ={
    var tmp = " "
    for ((k,v) <- VA)
     if(v!=Bottom) tmp += k+" ->"+v.next.key+" ,"
     else  tmp += k+", "

    return  tmp
  }

  /**
    *@return astring representation of what is in the List
    */
  override def toString(): String ={
    var tmp:String = ""
    var c = EChain
    while (c!=Bottom) {
      if(c.atom != null){
        ////println("string construct "+c.atom.toString)
        tmp+=" "+c.atom.toString
      }
      //println(ID+", in key "+c.key)
      c = c.next
      //println(ID+", gone to key "+c.key)
    }
    return tmp
  }
}