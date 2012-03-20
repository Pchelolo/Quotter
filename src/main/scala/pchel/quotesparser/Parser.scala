package pchel.quotesparser {
  import scala.collection.mutable.Queue
  import scala.xml._
  import java.net.{ URLConnection, URL }
  import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
  import scala.actors.Actor
  import scala.util.matching.Regex
  import pchel.planerizer.QuoteGraph

  abstract class ParserMessage
  case class ParseTask(data: (String, NodeSeq => NodeSeq, Node => String, Node => String)) extends ParserMessage
  case class ParserResult(quotesList: Seq[(String, String)]) extends ParserMessage
  case class ParserError(message: String) extends ParserMessage

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

    private def filterQuotes(task: (String, NodeSeq => NodeSeq, Node => String, Node => String)): ParserMessage = task match {
      case (url, filtering, getAuthor, getQuote) =>
        try {
          val document = getDocument(url)
          new ParserResult(for (element <- filtering(document)) yield (getAuthor(element), getQuote(element)))
        } catch {
          case e: java.net.UnknownHostException => new ParserError("Can`t reach host " + url)
        }
    }
  }

  object OutputActor extends Actor {

    private object MongoSerializer {
      //import com.mongodb.casbah.Imports._
      //import com.mongodb.casbah.MongoConnection

      //private var storage = MongoConnection()("quotter")("quotes")

      def save(author: String, quote: String) {
    	  //storage.insert(MongoDBObject("author" -> author, "quote" -> quote))	
      }

    }

    private def clear(elem: (String, String)): (String, String) = {
      elem match {
        case (author, quote) =>
          val noBraces = """\(.*?\)""".r
          val noEnters = """\n""".r
          (noBraces.replaceAllIn(author, "").trim(), noEnters.replaceAllIn(quote, " "))
      }
    }

    def act {
      val g = new QuoteGraph
      (1 to Parser.tasksNumber) foreach (_ =>
        receive {
          case ParserResult(quotesList) => for ((author, quote) <- quotesList.map(clear) if author.length + quote.length <= 137) g.addNode(author, quote)
          case ParserError(message) => error(message)
        })
        println(g)
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
    }

    private var tasks: List[(String, NodeSeq => NodeSeq, Node => String, Node => String)] = List.empty
    private def createTasks() = {
      import filters._

      val url = "http://citaty.info/rating/best?page="
      val restriction = unite(hasClass("node-inner-3")_)_
      val author = (n: Node) => (n \\ "a").first.text
      val content = (n: Node) => (n \\ "p").first.text
      (1 to 5) foreach (i => tasks ::= (url + i, restriction, author, content))
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
      createTasks()
      OutputActor.start
      this.start
    }
  }
}
