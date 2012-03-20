package pchel.planerizer {
import scala.collection.mutable.HashSet

abstract class QuoteNode {
  def relations : HashSet [ Edge ]
  def author : String
  def quote : String
  def nouns : Seq[String]

  def addRelation(node : QuoteNode, weight : Int) : QuoteNode
  def findRelationWeight(that : QuoteNode) : Int
}
  
object Edge {
    implicit def edge2tuple(edge : Edge) : (QuoteNode, Int) = (edge.quoteNode, edge.weight)
    implicit def tuple2edge(tuple : (QuoteNode, Int)) : Edge = new Edge(tuple._1, tuple._2)
    implicit def quotenode2edge(node : QuoteNode) : Edge = new Edge(node, 0)
}
  
class Edge(_quoteNode : QuoteNode,var _weight : Int) {
    def quoteNode = _quoteNode
    def weight = _weight
    
    override def hashCode() = quoteNode.hashCode
    override def equals(that : Any) = that match {
      case other : Edge => quoteNode.equals(other.quoteNode)
      case _ => false
    }
    
    def update(newWeight : Int) {_weight = newWeight}
    
    override def toString() = "[ "+quoteNode.author+" , "+quoteNode.quote+" : "+weight+" ]"
}

class QuoteGraph {
private class QuoteNodeImpl(_author : String, _quote : String) extends QuoteNode {
  self : QuoteNode =>

  private val _relations : HashSet[ Edge ] = HashSet.empty
  private val _nouns : Seq[String] = List.empty;//StemmedResolver.getNouns(quote)
  
  override def relations = _relations
  override def author = _author
  override def quote = _quote
  override def nouns = _nouns

  override def toString() = author+" : "+quote+" - relations "+relations
  override def equals(that : Any) = that match {
    case other : QuoteNode => this.author.equals(other.author) && this.quote.equals(other.quote)
    case _ => false
  }
  override def hashCode() = 23 * author.hashCode + quote.hashCode
  
  override def addRelation(node : QuoteNode, weight : Int) = {
    if( relations.contains( node ) ) error("already Added")
    else {
      relations.add( (node, weight) )
      this
    }
  }
  
  override def findRelationWeight(that : QuoteNode) : Int = {
    var sameWords = 0
    for(thisNoun <- this.nouns)
      for(thatNoun <- that.nouns)
        if( thisNoun equals thatNoun ) sameWords+=1
    sameWords
  }
}

  val nodes : HashSet[QuoteNode] = HashSet.empty
  
  def addNode(author : String, quote : String) : QuoteNode = {
    val newNode = new QuoteNodeImpl(author, quote)
    nodes.findEntry(newNode) match {
      case some : Some[QuoteNode] => some.get
      case None => {
        for(existingNode <- nodes) {
          val relationWeight = newNode.findRelationWeight(existingNode)
          if (relationWeight > 0) {
            newNode.addRelation(existingNode, relationWeight)
            existingNode.addRelation(newNode, relationWeight)
          }
        }
        nodes.add(newNode)
        newNode
      }
    }
  }
  
  def getNode(node : QuoteNode) : Option[QuoteNode] = nodes.findEntry(node)
  def getNode(author : String, quote : String) : Option[QuoteNode] = nodes.findEntry(new QuoteNodeImpl(author, quote))
  
  override def toString() = (for(node <- nodes) yield "\n"+node.toString).toString
}
}

