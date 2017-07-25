package example

object Attempt1 {
  import fastparse._

  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace( (" " | "\r\n").rep )
  }

  import fastparse.noApi._
  import White._

  sealed trait Ty
  case class AInt(name: String, length: Int)
  case class AString(name: String, length: Int)
  case class Grammar(rows : Seq[Ty])

  val identifier = P (CharIn('A' to 'Z'))
  val digit = P( CharIn('0' to '9').rep(1) ).!.map(_.toInt)
  val intRow: P[AInt] = P( "INT(" ~ digit ~ ")" ~ identifier.!).map{case (num,ident) => AInt(ident, num)}
  val stringRow: P[AString] = P( "STRING(" ~ digit ~ ")" ~ identifier.!).map{case (num,ident) => AString(ident,num)}
  val row = intRow | stringRow
  val grammar = P("START" ~ row.rep(sep=";") ~ ";".rep(1) ~ "END".rep(1))
}

object Hello extends App {
  val fullEx = """START
INT(3) X;
STRING(3) X;
END
"""
  println(Attempt1.row.parse("INT(3) X"))
  println(Attempt1.row.parse("STRING(3) X"))
  println(Attempt1.grammar.parse(fullEx))
}


/*
I have a fastparse parser 'P( "INT(" ~ digit ~ ")" ~ identifier).map{case num => AInt("",num)}', I expected both the capture of digit and identifier to be mapped to where num is, but it seems only num is captured. How can I capture both?


 I have a parser 'P( "INT(" ~ digit ~ ")" ~ identifier).map{case num => AInt("",num)}', I expected both the capture of digit and identifier to be mapped to where num is, but it seems only num is captured. How can I capture both?
 
 */

/*
 Grammar:

 START
 INT(3) X
 STRING(5) Y
 END

 AST (suggestions welcome if it could be better):

 Grammar(Seq[AInt("X",3), AString("Y",5)])

 Case classes to get that AST:

 sealed trait Ty
 case class AInt(name: String, length: Int)
 case class AString(name: String, length: Int)
 case class Grammar(rows : Seq[Ty])

 I have experience using Haskell's Parsec library so I initially tried using Scala Parser Combinators, however I couldn't figure out how to replicate "between". I then moved to trying fastparse.

 With fastparse, I could easily parse 'INT(3) X' using:

 val digit = ...
 val intRow: P[Int] = P( "INT(" ~ digit ~ ")" ~ identifier)

 Similarly 'STRING(5) Y' using:

 val stringRow: P[Int] = P( "STRING(" ~ digit ~ ")" )
 */

// object Foo {
//   import fastparse.all._
//   val plus = P( "+" )
//   val num = P( CharIn('0' to '9').rep(1) ).!.map(_.toInt)
//   val side = P( "(" ~ expr ~ ")"| num )
//   val expr : P[Int] = P(side ~ plus ~ side).map{case (l,r) => l + r}
// }


