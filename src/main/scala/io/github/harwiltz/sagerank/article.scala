package io.github.harwiltz.sagerank

import scala.util.matching.Regex

import scalaj.http.Http
import spray.json._

import DefaultJsonProtocol._

abstract class SemanticArticleReference
case class ArticleReference(id: String) extends SemanticArticleReference
case class ArxivArticleReference(id: String) extends SemanticArticleReference
case class DOIArticleReference(id: String) extends SemanticArticleReference
case class MAGArticleReference(id: String) extends SemanticArticleReference
case class ACLArticleReference(id: String) extends SemanticArticleReference
case class PubMedArticleReference(id: String) extends SemanticArticleReference
case class CorpusArticleReference(id: String) extends SemanticArticleReference

abstract class ArticleStatus
case object ReadArticle extends ArticleStatus
case object UnreadArticle extends ArticleStatus
case object InterestedInArticle extends ArticleStatus

case class ArticleMetadata(id: String, title: String, authors: Vector[String], abs: String, year: String, status: ArticleStatus)
case class ArticleBibliography(article: ArticleMetadata, references: Vector[ArticleBibliography])

object Article {
  val apiPrefix = "https://api.semanticscholar.org/v1/paper/"

  val missingAuthorName = "Unkown Author"
  val missingAbstract = "No Abstract"
  val missingYear = "Unknown Year"

  val arxivMatcher = """arxiv.org/(pdf|abs)/(\d+\.\d+)$""".r.unanchored
  val semanticIdMatcher = """^([a-z0-9]{40})$""".r.unanchored
  val doiMatcher = """(doi.org|doi/full)/(.+)""".r.unanchored
  val pubmedMatcher = """/pubmed/(\d+)$""".r.unanchored

  def fromURL(url: String,
              getReferences: Boolean = false,
              status: ArticleStatus = InterestedInArticle): Option[ArticleBibliography] = {
    val articleId = url match {
      case arxivMatcher(_, id) => Some(ArxivArticleReference(id))
      case semanticIdMatcher(id) => Some(ArticleReference(id))
      case doiMatcher(_, id) => Some(DOIArticleReference(id))
      case pubmedMatcher(id) => Some(PubMedArticleReference(id))
      case _ => None
    }
    articleId.flatMap(id => fromPaperId(id, getReferences, status))
  }

  def fromPaperId(ref: SemanticArticleReference,
                  getReferences: Boolean = false,
                  status: ArticleStatus = InterestedInArticle): Option[ArticleBibliography] = {
    val inputId = paperId(ref)
    val responseJson = responseBodyAst(inputId)
    val article = extractArticleMetadata(responseJson, status)
    val references = if(getReferences) {
                       extractArticleReferences(responseJson)
                     } else {
                       Vector[ArticleBibliography]()
                     }
    article.map { a => ArticleBibliography(a, references) }
  }

  def attachReferences(artbib: ArticleBibliography): ArticleBibliography =
    if(artbib.references.isEmpty) {
      val responseJson = responseBodyAst(artbib.article.id)
      val references = extractArticleReferences(responseJson)
      artbib.copy(references = references)
    } else {
      artbib
    }

  private def extractArticleMetadata(json: Map[String, JsValue], status: ArticleStatus = UnreadArticle): Option[ArticleMetadata] = {
    val authors = json.get("authors")
                      .map(x => x.convertTo[Vector[JsValue]])
                      .getOrElse(Vector[JsValue]())
                      .map(a => getAuthorFromJson(a))
    val abs = json.get("abs").map(a => a.convertTo[String])
    val year = json.get("year").map(y => y.convertTo[String])
    for {
      id <- json.get("paperId")
      title <- json.get("title")
    } yield ArticleMetadata(id.convertTo[String],
                            title.convertTo[String],
                            authors,
                            abs.getOrElse(missingAbstract),
                            year.getOrElse(missingYear),
                            status)
  }

  private def extractArticleReferences(json: Map[String, JsValue]): Vector[ArticleBibliography] = {
    val referencesJsValue = json.get("references").map(x => x.convertTo[Vector[JsValue]])
                                                  .getOrElse(Vector[JsValue]())
    referencesJsValue.map(x => x.asJsObject.fields)
                     .map(x => extractArticleMetadata(x))
                     .flatten
                     .map(x => ArticleBibliography(x, Vector[ArticleBibliography]()))
  }

  private def getAuthorFromJson(json: JsValue): String = {
    val nameJson = json.asJsObject.fields.get("name")
    nameJson.map(n => n.convertTo[String]).getOrElse(missingAuthorName)
  }

  private def responseBodyAst(id: String): Map[String, JsValue] = httpResponse(id).asJsObject
                                                                                  .fields

  private def httpResponse(id: String): JsValue = Http(s"${apiPrefix}${id}").asString
                                                                            .body
                                                                            .parseJson

  private def paperId(ref: SemanticArticleReference): String = ref match {
    case ArxivArticleReference(id) => s"arxiv:${id}"
    case ArticleReference(id) => id
    case DOIArticleReference(id) => id
    case MAGArticleReference(id) => s"MAG:${id}"
    case ACLArticleReference(id) => s"ACL:${id}"
    case PubMedArticleReference(id) => s"PMID:${id}"
    case CorpusArticleReference(id) => s"CorpusID:${id}"
  }
}
