package com.yuroyoro

// 1階マルコフ連鎖で奇声を発するジェネレータ
// こんなのが出ます。おまえなにやってんの?ぎょぱーーーっ!!
//   ほにゃあ！
//   ぃっーー！！！
//   よぅ…くんくんくんどるぷぇー
//   ほにゃっぱ
//   ぃぱ
//   よぅぅ…くんくんっ
//   ひぃいやぁぁ……あひぎょーっうふふぬふぅ！！
//   にゃあぃっきゅいっ…
//   う！
//   んっはーっ…くんんはにゅわぁあふぅ……きゅい！！！
//   ぬいっはにゃう！！！！！
//   へぬふぇぁんどるぁっあ…くん
//   へぺ
//   モフ！！
//   ぃぱ
//   っうわふぅ！
//   あ！！
//   わーーーっ…ん
//   にゃあんきぇぁ…くんくんきぇぇ
//   わぁ……

import scala.util.Random

class GyopaaaMarkov( sentences:Seq[String] ) {

  sealed abstract class Token
  case object Head extends Token
  case object Tail extends Token
  case class Word( c:Char ) extends Token

  case class Node( token:Token, next:Seq[Token] ){
    def choice = next( Random.nextInt( next.size ))
  }

  val sp = "?!。、！？…"
  val em = "・.,?!。、！？‥…"
  val emr = (".*[" + em + "]$").r

  val tokens = sentences.flatMap{ s =>
    val tokens =  Head :: s.map{ c => Word( c ) }.toList ::: Tail :: Nil
    tokens.sliding(2).toList.collect { case Seq(c1, c2) => (c1, c2) }
  }.distinct
  val dict = tokens.groupBy{ case( t, n ) => t }.map{ case (t ,s) =>
    Node( t, s.map{ case( _, n ) => n } )}.toList

  def choiceNode( word:Token ):Node = {
    val nodes = dict.collect{ case n @ Node(`word`, _) => n }
    nodes( Random.nextInt( nodes.size ) )
  }

  def generate = {
    def chain( n:Node ):List[Char]= n.choice match {
      case t:Word=> t.c :: chain( choiceNode( t ) )
      case _ => Nil
    }
    val rv = chain( choiceNode( Head ) ).mkString
    rv match{
      case emr() => rv
      case _ => rv + sp( Random.nextInt( sp.size  ))
    }
  }
}

object Gyopaaa {
  val seq = Seq(
     "ふぬいっ", "ふぬるぷ" , "ふにょー", "ふぬわーっ",
     "ぎょぱー", "ぎゃっぱぎゃっぱ", "ぬふふ", "ほにゅわーっ",
     "ぎゃっぱぎゃっぱ", "へぺっ", "もるぁー", "もるすぁー",
     "ひゃっはーーーっ", "きゃっきゃうふふ", "ひゃぴーっ", "へぬぇ",
     "きぇぇぇ", "はにゃー", "へぷぇ", "ぬるぽっ", "わふーっ", "あぃっ",
     "おんどるぁ", "あひぃ", "ひぎぃ", "らめぇー", "おるぁ", "ごるぁ",
     "おぺぺぺぺ", "ごぶぁ", "らぬぃぱぁ", "あっぁぁあああ",
     "ぅぅうううわぁああああん！！！", "っぁっ………",
     "あふぅ……んっ", "ぃぃいやぁん", "ふぇぁあっ",
     "あぁああああ…ああ…あっあっー！", "あぁああああああ！！！",
     "あぁぁ…くんくん",
     "んはぁっ！", "モフモフ！", "…きゅんきゅんきゅい！！",
     "よぅ！！", "あぁぁああ…あああ…あっあぁああ！", "ふぁぁああんんっ！！",
     "いやぁああ！！", "にゃああん！！", "ぎゃあああ！！",
     "いやっほぉおお！！！", "ううっうぅうう！"
  )
  val markov = new GyopaaaMarkov( seq )
  val mg = new MorphologicalGyoppa( markov )

  def run( args:Array[String] ) = {
    args.headOption match {
      case Some( word ) => ( 1 to 10 ) foreach { n =>
        println( mg( word ) ) }
      case None =>
        ( 1 to 10 ) foreach { n =>
          println( ( 1 to ( 1 + Random.nextInt(3))) map{ m => markov.generate } mkString) }
    }
  }
}

