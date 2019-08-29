package RGA

/**
  * class representing the time stamp
  * @param id the process id
  * @param counter the counter of the process
  */
class TimeStamp(id:Int, counter:Int) extends Comparable[TimeStamp]{
  def KeyFromTimeStamp:String = Counter+":"+ID
  var Counter = counter
  var ID = id

  def inc()=Counter+=1

  def <(timeStamp: TimeStamp):Boolean = this.Counter < timeStamp.Counter ||
    (this.Counter == timeStamp.Counter && this.ID < timeStamp.ID)

  def >(timeStamp: TimeStamp):Boolean = this.Counter > timeStamp.Counter||
      (this.Counter == timeStamp.Counter&& this.ID > timeStamp.ID)

  def ==(timeStamp: TimeStamp):Boolean = (this.Counter == timeStamp.Counter&& this.ID == timeStamp.ID)

  override def compareTo(o: TimeStamp): Int = {
    if(this > o) return 1
    else if(this == o) return 0
    else if(this < o) return -1
    return -1
  }
}
