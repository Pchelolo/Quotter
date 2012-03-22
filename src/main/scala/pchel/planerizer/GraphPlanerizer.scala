package pchel.planerizer
import scala.collection.mutable.HashSet
import pchel.planerizer.InfiniteField._

object GraphPlanerizer {

  private def d(p1 : Coord, p2 : Coord) : Double = Math.pow(p1.x - p2.x, 2) + Math.pow(p1.y - p2.y,2)
  
	def planerize(g : QuoteGraph) : InfiniteField[QuoteNode] = {
		val field = new InfiniteField[QuoteNode]
		val edge = new HashSet[Coord]
		edge.add((0,0))
		
		val toPut = new HashSet[QuoteNode]
		toPut.add(g.getWithMaxRelations)
		
		while( toPut.size >0 ) {
		  var minimumDistance = Double.MaxValue
	      var quoteToPut = toPut.first
		  var placeToPut = edge.first
		  
		  var maxRelationsWeight = 0
		  for(quote <- toPut) {
		    var relWeight = quote.relations.filter(_.quoteNode.coord != None).map(rel => rel.weight).sum
		    if(relWeight > maxRelationsWeight) {
		      quoteToPut = quote
		      maxRelationsWeight = relWeight
		    }
		  }
		  
		  for(place <- edge) {
		    val distance = quoteToPut.relations.filter(_.quoteNode.coord != None).map(rel => d(place, rel.quoteNode.coord.get)*rel.weight).sum
		    if(distance < minimumDistance || (distance == minimumDistance && Math.random < 0.3) ) {
		      minimumDistance = distance
		      placeToPut = place	        
		    }
		  }
		  
		  field.put(quoteToPut, placeToPut)
		  quoteToPut.coord = placeToPut
		  
		  toPut.remove(quoteToPut)
		  for(newToPut <- quoteToPut.relations.filter(_.quoteNode.coord == None)map(_.quoteNode)) toPut.add(newToPut)
		  
		  edge.remove(placeToPut)
		  for(newPlace <- field.getEmptyNeighbors(placeToPut)) edge.add(newPlace)
		}
		field
	}
} 