package com.yuroyoro

class MorphologicalGyoppa( markov:GyopaaaMarkov ) {
  def apply( s:String ) = MorphologicalAnalyser( s ) map{ c =>
      c.toTokenExceptSpecialChars + markov.generate
  } mkString
}

