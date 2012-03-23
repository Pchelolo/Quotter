package pchel.quotesparser {
  import scala.collection.mutable.Queue
import scala.xml._
import java.net.{ URLConnection, URL }
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import scala.actors.Actor
import scala.util.matching.Regex
import pchel.planerizer.QuoteGraph
import pchel.planerizer.GraphPlanerizer
import scala.collection.mutable.ListBuffer
import pchel.planerizer.InfiniteField.Coord

  abstract class ParserMessage
  case class ParseTask(data: Task) extends ParserMessage
  case class ParserResult(quotesList: Seq[(String, String, Seq[String])]) extends ParserMessage
  case class ParserError(message: String) extends ParserMessage
  
  class Task(_url : String, _getBaseNode : NodeSeq => NodeSeq, _getAuthor : Node => String, _getQoute : Node => String, _getTags : Node => Seq[String]) {
    def url = _url
    def getBaseNode = _getBaseNode
    def getAuthor = _getAuthor
    def getQuote = _getQoute
    def getTags = _getTags
  }

  class ParserActor extends Actor {
    def act {
      receive {
        case ParserResult => error("Result message to ParseActor")
        case ParserError => error("Error message to ParseActor")
        case ParseTask(task) => OutputActor ! filterQuotes(task)
      }
    }

    private def getDocument(sURL: String): Elem = {
      var url = new URL(sURL);
      var connection = new URL(sURL).openConnection
      XML.withSAXParser(new SAXFactoryImpl().newSAXParser()).load(connection.getInputStream)
    }

    private def filterQuotes(task: Task): ParserMessage = {
       try {
          val document = getDocument(task.url)
          val resultsList = ListBuffer.empty[(String, String, Seq[String])]
          for (element <- task.getBaseNode(document)) {
            try {
            	resultsList.append((task.getAuthor(element), task.getQuote(element), task.getTags(element)))
            } catch {
              case e : NoSuchElementException => //IGNORE
            }
          }
          new ParserResult(resultsList)
        } catch {
          case e: java.net.UnknownHostException => new ParserError("Can`t reach host " + task.url)
        }
    }
  }

  object OutputActor extends Actor {

    private object MongoSerializer {
      import com.mongodb.casbah.Imports._
      import com.mongodb.casbah.MongoConnection

      private val conn = MongoConnection()
      private val storage = conn("quotter")("quotes")

      def save(author: String, quote: String, tags : Seq[String], coord : Coord) {
    	  storage.insert(MongoDBObject("author" -> author, "quote" -> quote, "tags" -> tags, "coord" -> MongoDBObject("x" -> coord.x, "y" -> coord.y) ))	
      }
      
      def close {
        conn.close()
      }
    }

    private def clear(elem: (String, String, Seq[String])): (String, String, Seq[String]) = {
      elem match {
        case (author, quote, tags) =>
          val noBraces = """\(.*?\)""".r
          val noEnters = """\n""".r
          (noBraces.replaceAllIn(author, "").trim(), noEnters.replaceAllIn(quote, " "), tags)
      }
    }

    def act {
      val g = new QuoteGraph
      (1 to Parser.tasksNumber) foreach (_ =>
        receive {
          case ParserResult(quotesList) => for ( (author, quote, tags) <- quotesList.map(clear) if author.length + quote.length <= 137) g.addNode(author, quote, tags)
          case ParserError(message) => error(message)
       })
       
       try {
    	   for( (coord, quote) <- GraphPlanerizer.planerize(g).allPositions) MongoSerializer.save(quote.author, quote.quote, quote.nouns, coord)
       } finally {
         MongoSerializer.close
       }
       
    }
      
  }

  object Parser extends Actor {
    private object filters {
      def unite(restrictions: Node => Boolean*)(node: NodeSeq): NodeSeq = {
        var result: NodeSeq = node;
        for (r <- restrictions) result = (result \\ "_" filter r)
        result
      }

      def hasClass(value: String)(node: Node): Boolean = (node \ "@class").toString.contains(value)
      def hasRel(value: String)(node: Node): Boolean = (node \ "@rel").toString.contains(value)
    }

    private val tasks: List[Task] = createTasks
    private def createTasks() : List[Task]= {
      import filters._
      var temp_tasks = List.empty[Task]
      val url = "http://citaty.info/book?page="
      val restriction = unite(hasClass("node-inner-3")_)_
      val author = (n: Node) => (n \\ "a").first.text
      val content = (n: Node) => (n \\ "p").first.text
      val tags = (n: Node) => for(tag <- (n \\ "a" filter hasRel("tag")) ) yield tag.text 
      (1 to 100) foreach (i => {
          val task = new Task(url + i, restriction, author, content, tags) 
          temp_tasks ::= task
      })
      temp_tasks
    }

    def tasksNumber = tasks.length

    def act {
      for (task <- tasks) {
        val parseActor = new ParserActor
        parseActor.start
        parseActor ! new ParseTask(task)
      }
    }

    def main(args: Array[String]) {
      OutputActor.start
      this.start
    }
  }
}
