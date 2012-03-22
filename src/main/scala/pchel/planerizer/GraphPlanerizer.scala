package pchel.planerizer
import scala.collection.mutable.HashSet
import pchel.planerizer.InfiniteField._

object GraphPlanerizer {
  
	def planerize(g : QuoteGraph) : InfiniteField[QuoteNode] = {
		val field = new InfiniteField[QuoteNode]
		val edge = new HashSet[Coord]
		edge.add((0,0))
		
		val toPut = new HashSet[QuoteNode]
		toPut.add(g.getWithMaxRelations)
		
		while( toPut.size >0 ) {
		  var minimumDistance = Int.MaxValue
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
		    var distance = quoteToPut.relations.filter(_.quoteNode.coord != None).map(rel => ((place.x - rel.quoteNode.coord.get.x)^2 + (place.y - rel.quoteNode.coord.get.y)^2)*rel.weight).sum
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