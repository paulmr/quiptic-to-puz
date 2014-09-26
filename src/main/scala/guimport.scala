package org.stelo.cw.guimport

import java.io.File
import java.io.{InputStream, FileInputStream}
import java.net.URL

import scala.collection.JavaConversions._

import org.joda.time.DateTime

import org.jsoup.Jsoup
import org.jsoup.select.Elements
import org.jsoup.nodes.Element
import com.adamrosenfield.wordswithcrosses.puz._
import com.adamrosenfield.wordswithcrosses.io._

object GuImport {
  type Errors = List[String]
  type Boxes = Array[Array[Box]]
  type Clues = List[Clue]
  type BoardInput = Either[Errors, Boxes]
  type CluesInput = Either[Errors, Clues]

  val timeout = 20000

  val baseURI = ""

  sealed trait Clue {
    val text: String
    val num:  Int
    def isAcross = this match {
      case Across(_, _) => true
      case _ => false
    }
  }
  case class Across(text: String, num: Int) extends Clue
  case class Down(  text: String, num: Int) extends Clue

  def parseUrl(url: URL): Either[Errors, Puzzle] = {
    val doc = Jsoup.parse(url, timeout)
    val cw = doc.select(".crossword").first
    val meta = parseMeta(doc)
    val title = meta.getOrElse("title", "Unknown")
    val author = meta.getOrElse("author", "Unknown")
    val copyright = parseCopyright(doc).getOrElse("Unknown")
    val date = meta.get("article:published_time").map(new DateTime(_)).getOrElse(DateTime.now)
    for {
      board <- parseBoard(cw.select("table").first).right
      clues <- parseClues(cw.select(".clues-col")).right
    } yield makePuzzle(board, clues, title, author, copyright, date)
  }

  def makePuzzle(board: Boxes, cluesList: Clues, title: String, author: String,
		  copyright: String, date: DateTime): Puzzle = {
    val puz = new Puzzle
    val clues = cluesList.toArray
    puz.setBoxes(board)
    puz.setRawClues(clues sortWith { (a, b) => if(a.num == b.num) a.isAcross else a.num < b.num } map (_.text))
    puz.setNumberOfClues(clues.length)
    puz.setHeight(board.length)
    puz.setWidth(board.head.length)
    puz.setTitle(title)
    puz.setAuthor(author)
    puz.setCopyright(copyright)
    puz.setDate(date.toCalendar(null))
    puz
  }

  def parseCopyright(root: Element): Option[String] = {
    val elm = root.select("#copyright-links li")
    if(elm.length < 1) None else Some(elm.first.text)
  }

  def parseMeta(root: Element): Map[String, String] = {
    val elms = root.select("meta[property],meta[name]").toList
    elms.foldLeft(Map[String, String]()) { (map, elm) =>
      val key = if(elm.hasAttr("property")) elm.attr("property").replaceFirst("og:", "") else elm.attr("name")
      val value = elm.attr("content")
      map + (key -> value)
    }
  }

  def parseClues(es: Elements): CluesInput = {
    val clues = es.select("li label").toList.map(lbl => parseClue(lbl.id, lbl.ownText))
    val errors = clues.filter(_.isLeft)
    if(errors.isEmpty)
      Right(clues.map(_.right.get))
    else
      Left(errors.map(_.left.get))
  }

  def parseClue(id: String, text: String): Either[String, Clue] = {
    id.split("-").toList match {
      case num :: "across" :: _ => Right(Across(text, num.toInt))
      case num :: "down"   :: _ => Right(Down  (text, num.toInt))
      case _ => Left(s"could not parse clue $id ($text)")
    }
  }

  def parseBoard(es: Element): BoardInput = {
    val board = es.select("tr").toList.map { row =>
      row.select("td").toList.map { col =>
	if(col hasClass "blank") Right(null)
	else parseBoxId(col.id)
      }
    }
    val errors = board.flatten.filter(_.isLeft)
    if(errors.isEmpty)
      Right(board.map(row => row.map(col => col.right.get).toArray).toArray)
    else 
      Left(errors.map(_.left.get))
  }

  def parseBoxId(s: String): Either[String, Box] = {
    var b = new Box
    s.split("-").toList match {
      case num :: dir :: _ => 
	b.setClueNumber(num.toInt)
	b.setAcross(dir == "across")
	b.setDown(dir == "down")
	b.setCheated(false)
	b.setCircled(false)
	b.setResponse(' ')
	Right(b)
      case _ =>
	Left(s"couldn't parse col-id $s")
    }
  }
}

object GuPuz extends App {
  import GuImport._
  val url = args.headOption
  val outname = if(args.length > 1) args(1) else "out.puz"

  def readPuzFromUrl(url: String, outname: String) =
    parseUrl(new URL(url)) match {
      case Left(errors) =>
	println("Error processing input:\n" + errors.mkString("\n"))
	
      case Right(puz)   => IO.save(puz, new File(outname))
    }

  url.map(readPuzFromUrl(_, outname)).getOrElse(println("No url provided"))
}
