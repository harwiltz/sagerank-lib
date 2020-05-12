package io.github.harwiltz.sagerank

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

import io.github.harwiltz.sagerank._

class SageRanker(articles: Iterable[ArticleBibliography], p: Float = 0.15f) {
  type SageRankGraph = Graph[ArticleMetadata, UnDiEdge]
  val graph = initGraph(articles)

  def initGraph(articles: Iterable[ArticleBibliography]): SageRankGraph = addArticleReferences(articles)

  def addArticleReferences(artbibs: Iterable[ArticleBibliography]): SageRankGraph =
    (Graph[ArticleMetadata, UnDiEdge]() /: artbibs) { (acc, artbib) =>
      (acc /: artbib.references) { (g, ref) => g + artbib.article~ref }
    }
}
