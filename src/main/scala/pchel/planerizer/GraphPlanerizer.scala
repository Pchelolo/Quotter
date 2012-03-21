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
	      var quoteToPut : Option[QuoteNode] = None
		  var placeToPut : Option[Coord] = None
		  
		  for(quote <- toPut) {
		    for(place <- edge) {
		      var distance = quote.relations.filter(_.quoteNode.coord != None).map(rel => ((place.x - rel.quoteNode.coord.get.x)^2 + (place.y - rel.quoteNode.coord.get.y)^2)*rel.weight).sum
		      if(distance < minimumDistance) {
		        minimumDistance = distance
		        quoteToPut = new Some(quote)
		        placeToPut = new Some(place)		        
		      }
		    }
		  }
		  
		  field.put(quoteToPut.get, placeToPut.get)
		  quoteToPut.get.coord = placeToPut.get
		  
		  //println(placeToPut.get + "\n" + quoteToPut.get)
		  
		  toPut.remove(quoteToPut.get)
		  for(newToPut <- quoteToPut.get.relations.filter(_.quoteNode.coord == None)map(_.quoteNode)) toPut.add(newToPut)
		  
		  edge.remove(placeToPut.get)
		  for(newPlace <- field.getEmptyNeighbors(placeToPut.get)) edge.add(newPlace)
		}
		field
	}
} 