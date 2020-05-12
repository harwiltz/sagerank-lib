package io.github.harwiltz.sagerank

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

import io.github.harwiltz.sagerank._

import SageRanker._

object SageRanker {
  type SageRankNode = ArticleMetadata
  type SageRankGraph = Graph[SageRankNode, UnDiEdge]

  def articleGraph(artbibs: Iterable[ArticleBibliography]): SageRankGraph =
    (Graph[SageRankNode, UnDiEdge]() /: artbibs) { (acc, artbib) =>
      (acc /: artbib.references) { (g, ref) => g + artbib.article~ref }
    }
}

class SageRanker(graph: SageRankGraph = Graph[SageRankNode, UnDiEdge](),
                 p: Float = 0.15f) {

  def withArticleGraph(artbib: ArticleBibliography): SageRanker =
    new SageRanker(this.graph union articleGraph(Array(artbib)), this.p)

  def withArticleGraphs(artbibs: Iterable[ArticleBibliography]): SageRanker =
    new SageRanker(this.graph union articleGraph(artbibs), this.p)
}
