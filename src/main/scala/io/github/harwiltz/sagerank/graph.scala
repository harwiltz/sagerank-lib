package io.github.harwiltz.sagerank

import java.io.File
import java.io.PrintWriter

import scala.io.Source
import scala.math

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.io.json._
import scalax.collection.io.json.descriptor.StringNodeDescriptor
import scalax.collection.io.json.descriptor.predefined._
import spray.json._
import DefaultJsonProtocol._

import io.github.harwiltz.sagerank._
import io.github.harwiltz.sagerank.random.RandomSampler
import io.github.harwiltz.sagerank.random.EmpiricalCategoricalDistribution

import SageRanker.SageRankNode
import SageRanker.SageRankGraph
import SageRanker.SageRankType

object SageRanker {
  type SageRankNode = String
  type SageRankGraph = Graph[SageRankNode, UnDiEdge]
  type SageRankType = ArticleBibliography
}

object SageRankerFactory {
  import SageRankerJsonProtocol._
  import SageRankerJsonFormat._

  def fromFile(filepath: String): Option[SageRanker] = {
    if(new java.io.File(filepath).exists) {
      val str = Source.fromFile(filepath).mkString
      Some(str.parseJson.convertTo[SageRanker])
    } else {
      None
    }
  }

  def save(sageranker: SageRanker, filepath: String) {
    val writer = new PrintWriter(new File(filepath))
    writer.write(sageranker.toJson.toString)
    writer.close()
  }
}

class SageRanker(val graph: SageRankGraph = Graph[SageRankNode, UnDiEdge](),
                 val articleMap: Map[SageRankNode, ArticleBibliography] = Map[SageRankNode, ArticleBibliography](),
                 val p: Double = 0.15) {

  lazy val rnd = new RandomSampler

  def withArticleGraph(artbib: ArticleBibliography): SageRanker =
    this.withArticleGraphs(Array(artbib))

  def withArticleGraphs(artbibs: Iterable[ArticleBibliography]): SageRanker = {
    val newGraph = this.graph union articleGraph(artbibs)
    val newArtBibs = artbibs ++ artbibs.flatMap(x => x.references)
    val newArticleMap = (this.articleMap /: newArtBibs) { (acc, artbib) =>
      acc.get(makeNode(artbib)) match {
        case Some(_artbib) => {
          _artbib.article.status match {
            case UnreadArticle => acc + (makeNode(artbib) -> artbib)
            case _ => artbib.article.status match {
              case ReadArticle => acc + (makeNode(artbib) -> artbib)
              case _ => acc
            }
          }
        }
        case None => acc + (makeNode(artbib) -> artbib)
      }
    }
    new SageRanker(newGraph, articleMap = newArticleMap, p = this.p)
  }

  def withChangedStatus(status: ArticleStatus, justOnce: Boolean = false)(item: SageRankType): SageRanker =
    this.articleMap.get(this.makeNode(item)) match {
      case Some(artbib) => {
        val modifiedArtBib = artbib.copy(article = artbib.article.copy(status = status))
        if(justOnce) {
          new SageRanker(this.graph, this.articleMap + (this.makeNode(item) -> modifiedArtBib), this.p)
        } else {
          val newArtBib = status match {
            case UnreadArticle => modifiedArtBib
            case _ => Article.attachReferences(modifiedArtBib)
          }
          this.withArticleGraph(newArtBib)
        }
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
      article <- articleSampled.article.status match {
        case UnreadArticle => Some(articleSampled)
        case _ => None
      }
    } yield article
    result.getOrElse(this.suggestUnread)
  }

  def makeNode(item: SageRankType): SageRankNode = item.article.id

  def articleGraph(artbibs: Iterable[ArticleBibliography]): SageRankGraph =
    (Graph[SageRankNode, UnDiEdge]() /: artbibs) { (acc, artbib) =>
      (acc /: artbib.references) { (g, ref) => g + this.makeNode(artbib)~this.makeNode(ref) }
    }
}

object SageRankerJsonProtocol extends DefaultJsonProtocol {
  val descriptor = new Descriptor[String](defaultNodeDescriptor = StringNodeDescriptor,
                                          defaultEdgeDescriptor = UnDi.descriptor[String]())

  val readCode = 0
  val interestedInCode = 1
  val unreadCode = 2

  def formatStatus(status: ArticleStatus): Int = status match {
    case ReadArticle => readCode
    case InterestedInArticle => interestedInCode
    case UnreadArticle => unreadCode
  }

  def readStatus(code: Int): ArticleStatus = code match {
    case `readCode` => ReadArticle
    case `interestedInCode` => InterestedInArticle
    case `unreadCode` => UnreadArticle
  }

  implicit object ArticleBibliographyFormat extends RootJsonFormat[ArticleBibliography] {
    def write(artbib: ArticleBibliography): JsObject = JsObject(
      "id" -> JsString(artbib.article.id),
      "title" -> JsString(artbib.article.title),
      "authors" -> JsArray(artbib.article.authors.map(x => JsString(x))),
      "abs" -> JsString(artbib.article.abs),
      "year" -> JsString(artbib.article.year),
      "url" -> JsString(artbib.article.url),
      "status" -> JsNumber(formatStatus(artbib.article.status)),
      "references" -> JsArray(artbib.references.map(_.toJson))
    )

    def read(value: JsValue): ArticleBibliography = {
      val am = value.asJsObject.getFields("id", "title", "authors", "abs", "year", "url", "status") match {
        case Seq(JsString(id), JsString(title), JsArray(authors), JsString(abs), JsString(year), JsString(url), JsNumber(status)) =>
          ArticleMetadata(id,
                          title,
                          authors.map(_.convertTo[String]),
                          abs,
                          year,
                          url,
                          readStatus(status.toInt))
        case _ => throw new DeserializationException("ArticleBibliography serialization does not have proper ArticleMetadata")
      }
      value.asJsObject.getFields("references") match {
        case Seq(JsArray(references)) => ArticleBibliography(am, references.map(_.convertTo[ArticleBibliography]))
        case _ => throw new DeserializationException("Invalid references in ArticleBibliography serialization")
      }
    }
  }

  implicit object SageRankerJsonFormat extends RootJsonFormat[SageRanker] {
    def write(sageranker: SageRanker): JsObject = JsObject(
      "graph" -> JsString(sageranker.graph.toJson(descriptor)),
      "articleMap" -> sageranker.articleMap.toJson,
      "p" -> JsNumber(sageranker.p)
    )

    def read(value: JsValue): SageRanker = {
      value.asJsObject.getFields("graph", "articleMap", "p") match {
        case Seq(JsString(graph), JsObject(articleMap), JsNumber(p)) =>
          new SageRanker(Graph.fromJson[SageRankNode, UnDiEdge](graph, descriptor),
                         articleMap.map { case (k, v) => (k -> v.convertTo[ArticleBibliography]) },
                         p.toDouble)
        case _ => throw new DeserializationException("Invalid SageRanker!")
      }
    }
  }
}
