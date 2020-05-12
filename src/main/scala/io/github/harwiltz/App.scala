package io.github.harwiltz

import io.github.harwiltz.sagerank.Article
import io.github.harwiltz.sagerank.ArxivArticleReference

/**
 * @author ${user.name}
 */
object App {
  def foo(x : Array[String]) = x.foldLeft("")((a,b) => a + b)

  def main(args : Array[String]) {
    println( "Hello World!" )
    val id = ArxivArticleReference("1911.08265")
    val articleMetadata = Article.fromPaperId(id, true)
    articleMetadata match {
      case None => println("No article retrieved")
      case Some(artbib) => { 
        val article = artbib.article
        val references = artbib.references
        println(s"Title: ${article.title}")
        println(s"Authors: ${article.authors.mkString(",")}")
        println(s"Id: ${article.id}")
        println("References:")
        references foreach { r =>
          println(r.title)
        }
      }
    }
  }

}
