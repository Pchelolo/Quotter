package pchel.planerizer
import pchel.planerizer.InfiniteField.Coord
import scala.collection.mutable.ArrayBuffer

object InfiniteField {
  object Coord {
    implicit def tuple2coord(tuple : (Int, Int)) : Coord = new Coord(tuple._1, tuple._2)
    implicit def coord2tuple(coord : Coord) : (Int, Int) = (coord.x, coord.y)
}
class Coord(_x : Int, _y : Int) {
  def this() = this(0,0)
  
  def up = new Coord(_x, _y-1)
  def down = new Coord(_x, _y+1)
  def left = new Coord(_x-1, _y)
  def right = new Coord(_x+1, _y)
  def neighbors = List(this.up, this.right, this.down, this.left)
  
  override def toString = "( "+_x+", "+_y+")"
  
  override def equals(that : Any) = that match {
    case other : Coord => this._x == other.x && this._y == other.y
    case _ => false
  }
  
  override def hashCode = 23 * this.x.hashCode + y.hashCode
  
  def x = _x
  def y = _y
}
}

class InfiniteField[A] {
  private val elements : ArrayBuffer[A] = ArrayBuffer.empty
  private var coordToIndex : Map[Coord , Int] = Map.empty
		  
  def put(element : A, coord : Coord) : Boolean = {
    if ( coordToIndex.keySet contains coord) false
    else {
      coordToIndex += ( coord -> elements.length )
      elements.append(element)
      true
    }
  }
  
  def putOverwrite(element : A, coord : Coord) : Boolean = {
    if (coordToIndex.keySet contains coord) {
      elements(coordToIndex(coord)) = element
      true
    } else false
  }
  
  def get(coord : Coord) : Option[A] = {
    if ( coordToIndex.keySet contains coord) new Some(elements(coordToIndex(coord)))
    else None
  }
  
  def isFull(coord : Coord) : Boolean = coordToIndex.keySet contains coord
  def isEmpty(coord : Coord) : Boolean = ! isFull(coord)
  
  def allPositions = for (coord <- coordToIndex.keys) yield ( coord, elements(coordToIndex(coord)) )

 
  def getEmptyNeighbors(coord : Coord) : Seq[Coord] = for (candidate <- coord.neighbors if isEmpty(candidate)) yield candidate
  
  def getUp(coord : Coord) : Option[A] = get(coord.up)
  def getDown(coord : Coord) : Option[A] = get(coord.down)
  def getLeft(coord : Coord) : Option[A] = get(coord.left)
  def getRight(coord : Coord) : Option[A] = get(coord.right)
  
  def printPicture() {
    var maxX = 0
    var maxY = 0
    var minX = 0
    var minY = 0
    
    for(coord <- coordToIndex.keySet) {
      if (coord.x>maxX) maxX = coord.x
      if (coord.x<minX) minX = coord.x
      if (coord.y>maxY) maxY = coord.y
      if (coord.y<minY) minY = coord.y
    }
    
    (minY to maxY) foreach(y => {
      var line = ""
      (minX to maxX) foreach(x => {
        val s = if(coordToIndex.keySet contains (x,y)) "#" else " "
        line += s
      })
      println(line)
    })
  }
}