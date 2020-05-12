package io.github.harwiltz.sagerank

import scala.math

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

import io.github.harwiltz.sagerank._
import io.github.harwiltz.sagerank.random.RandomSampler
import io.github.harwiltz.sagerank.random.EmpiricalCategoricalDistribution

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
                 p: Double = 0.15) {

  val rnd = new RandomSampler

  def withArticleGraph(artbib: ArticleBibliography): SageRanker =
    new SageRanker(this.graph union articleGraph(Array(artbib)), this.p)

  def withArticleGraphs(artbibs: Iterable[ArticleBibliography]): SageRanker =
    new SageRanker(this.graph union articleGraph(artbibs), this.p)

  def sample: SageRankNode = {
    val nodes = this.graph.nodes
    val firstNode = nodes.toVector(this.rnd.nextInt(nodes.size))
    val walkLength = math.round(this.rnd.sampleGeometric(this.p).toFloat)
    (firstNode /: (0 to walkLength)) { (node, _) =>
      val neighbors = this.graph.get(node).neighbors.toVector
      neighbors(this.rnd.nextInt(neighbors.size))
    }
  }

  def stationaryDistribution(numSamples: Int = 10000): EmpiricalCategoricalDistribution[SageRankNode] = {
    val initialMap = (Map[SageRankNode, Int]() /: this.graph.nodes) { (acc, node) => acc + (node.toOuter -> 0) }
    //TODO: Parallelize this
    val counts = (0 to numSamples).map(_ => this.sample)
                                  .foldLeft(initialMap) { (acc, node) => acc + (node -> (acc.apply(node) + 1)) }
    new EmpiricalCategoricalDistribution(counts)
  }
}
