package com.yuroyoro

case class Morpheme( word:String, pos:String, reading:String )
case class Chunk( morphemes : Seq[Morpheme] ){
  def toToken = morphemes map{ case Morpheme(w, _, _) => w } mkString
  def toTokenExceptSpecialChars = morphemes collect {
    case Morpheme( w, "特殊", _) => ""
    case Morpheme( w, _, _ ) => w
  } mkString
}

/**
 * Yahoo形態素解析APIを利用して形態素解析を行うObject
 */
object MorphologicalAnalyser {
  import java.net.{URL, URLEncoder}
  import scala.xml._
  import scala.io.Source

  val apikey = "E5JE6uuxg64mzybiFvlbeXLmDyw3K1f.Kpj0D.W5JMQdXdMB98muWuy9PUqGOLiFIRmuplc-"
  val url = "http://jlp.yahooapis.jp/DAService/V1/parse?appid=%s&sentence=%s"

  /**
   * 形態素解析を行い、品詞分割されたListを返します。
   */
  def apply( s:String ) = XML.loadString(
    Source.fromURL( new URL(
      url.format( apikey, URLEncoder.encode(s , "utf-8")))
    ).mkString) \\ "Chunk" map { c => Chunk( c \\ "Morphem" map{ m =>
      Morpheme( m \\ "Surface"  text, m \\ "POS" text, m \\ "Reading" text) }) }

}

