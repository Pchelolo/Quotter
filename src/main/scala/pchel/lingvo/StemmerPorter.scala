package pchel.lingvo {
import scala.util.matching.Regex
import scala.MatchError
import scala.io.Source

object PartOfSpeech extends Enumeration {
  type PartOfSpeech = Value
  val Noun, Adjective, Verb, Unknown = Value
}

object StemmerPorter {
  private object RuleKind extends Enumeration {
    type RuleKind = Value
    val ReflectiveGround, Reflective, Adjective, Participle, Verb, Noun, Derivational, DerivationalToDel = Value
  }
  import RuleKind._
  
  private class StemmedWord(var _word : String) {
    private val rules = Map(ReflectiveGround	-> """((ив|ивши|ившись|ыв|ывши|ывшись)|((?<=[ая])(в|вши|вшись)))$""".r,
    						Reflective			-> """(с[яь])$""".r,
    						Adjective			-> """(ее|ие|ые|ое|ими|ыми|ей|ий|ый|ой|ем|им|ым|ом|его|ого|еых|ую|юю|ая|яя|ою|ею)$""".r,
    						Participle			-> """((ивш|ывш|ующ)|((?<=[ая])(ем|нн|вш|ющ|щ)))$""".r,
    						Verb				-> """((ила|ыла|ена|ейте|уйте|ите|или|ыли|ей|уй|ил|ыл|им|ым|ены|ить|ыть|ишь|ую|ю)|((?<=[ая])(ла|на|ете|йте|ли|й|л|ем|н|ло|но|ет|ют|ны|ть|ешь|нно)))$""".r,
    						Noun				-> """(а|ев|ов|ие|ье|е|иями|ями|ами|еи|ии|и|ией|ей|ой|ий|й|и|ы|ь|ию|ью|ю|ия|ья|я|ах|ых)$""".r,
    						Derivational		-> """[^аеиоуыэюя][аеиоуыэюя]+[^аеиоуыэюя]+[аеиоуыэюя].*(?<=о)сть?$""".r,
    						DerivationalToDel 	-> """ость?$""".r)
    def word = _word   
    def tryRule(rule : RuleKind) : Boolean = {
      val old_word = _word
      _word = rules(rule).replaceAllIn(_word, "")
      old_word != _word
    }  
    def replaceByRegex(regex : Regex, to : String) = {
      regex.replaceAllIn(_word, to)
      this
    }
    def deleteByRegex(regex : Regex) = replaceByRegex(regex, "")
    def ifMatches(rule : RuleKind) = rules(rule).findAllIn(_word).length != 0
  }
  
  private val Rvre = """^(.*?[аеиоуыэюя])(.*)$""".r
  
  def stemWord(word : String) : (String, PartOfSpeech.Value) = {
    var stem = word.toLowerCase.replace('ё', 'е')
    var partOfSpeech = PartOfSpeech.Unknown
    
    try {
      val Rvre(start, rv) = stem
      val stemmed = new StemmedWord(rv)
      if ( !stemmed.tryRule(ReflectiveGround) ) {
        stemmed.tryRule(Reflective)
        if ( stemmed.tryRule(Adjective) ) {
          partOfSpeech = PartOfSpeech.Adjective
          stemmed.tryRule(Participle)
        } 
        else if( stemmed.tryRule(Verb) )  partOfSpeech = PartOfSpeech.Verb
        else if( stemmed.tryRule(Noun) )  partOfSpeech = PartOfSpeech.Noun
      }
      
      stemmed.deleteByRegex("""и$""".r)
      if(stemmed.ifMatches(Derivational)) stemmed.tryRule(DerivationalToDel)
      stemmed.deleteByRegex("""ь$""".r).deleteByRegex("""ейш?""".r).replaceByRegex("""нн$""".r, "н")

      stem = start + stemmed.word
    } catch {
      case e : MatchError => //Nothing to do
    }
    (stem, partOfSpeech)
  }
  
  def stemText(text : String) : Seq[(String, PartOfSpeech.Value)] = for(word <- text.toLowerCase().split("""(?u)[^a-zа-я]+""")) yield stemWord(word)
}
}