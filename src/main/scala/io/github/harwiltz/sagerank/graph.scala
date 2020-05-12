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
  type SageRankNode = String
  type SageRankGraph = Graph[SageRankNode, UnDiEdge]
  type SageRankType = ArticleMetadata
}

class SageRanker(graph: SageRankGraph = Graph[SageRankNode, UnDiEdge](),
                 articleMap: Map[SageRankNode, ArticleMetadata] = Map[SageRankNode, ArticleMetadata](),
                 p: Double = 0.15) {

  val rnd = new RandomSampler

  def withArticleGraph(artbib: ArticleBibliography): SageRanker =
    this.withArticleGraphs(Array(artbib))

  def withArticleGraphs(artbibs: Iterable[ArticleBibliography]): SageRanker = {
    val newGraph = this.graph union articleGraph(artbibs)
    val newArticleMap = (this.articleMap /: artbibs) { (acc, artbib) =>
      acc.get(makeNode(artbib.article)) match {
        case Some(article) => {
          article.status match {
            case UnreadArticle => acc + (makeNode(article) -> artbib.article)
            case _ => artbib.article.status match {
              case ReadArticle => acc + (makeNode(artbib.article) -> artbib.article)
              case _ => acc
            }
          }
        }
        case None => acc + (makeNode(artbib.article) -> artbib.article)
      }
    }
    new SageRanker(newGraph, articleMap = newArticleMap, p = this.p)
  }

  def withChangedStatus(status: ArticleStatus)(item: SageRankType): SageRanker = this.articleMap.get(this.makeNode(item)) match {
    case Some(article) => {
      val newArticle = article.copy(status = status)
      val newArticleMap = this.articleMap + (this.makeNode(newArticle) -> newArticle)
      new SageRanker(this.graph, articleMap = newArticleMap, p = this.p)
    }
    case None => this
  }

  def withMarkedRead = withChangedStatus(ReadArticle)_
  def withMarkedUnread = withChangedStatus(UnreadArticle)_
  def withMarkedInterested = withChangedStatus(InterestedInArticle)_

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

  def suggestUnread: SageRankType = {
    val _sample = this.sample
    val result = for {
      articleSampled <- this.articleMap.get(_sample)
      article <- articleSampled.status match {
        case UnreadArticle => Some(articleSampled)
        case _ => None
      }
    } yield article
    result.getOrElse(this.suggestUnread)
  }

  def makeNode(item: SageRankType): SageRankNode = item.id

  def articleGraph(artbibs: Iterable[ArticleBibliography]): SageRankGraph =
    (Graph[SageRankNode, UnDiEdge]() /: artbibs) { (acc, artbib) =>
      (acc /: artbib.references) { (g, ref) => g + this.makeNode(artbib.article)~this.makeNode(ref) }
    }
}
