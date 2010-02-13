package org.uwh.osgi.parsers

import scala.util.parsing.combinator._
import org.osgi.framework._

trait Parameter {
	def attr : String
	def value : String
}

case class Attribute(val attr : String, val value : String) extends Parameter
case class Directive(val attr : String, val value : String) extends Parameter

case class VersionRange(val lowerBound : String, val upperBound : String, val lowerExclusive : Boolean, val upperExclusive : Boolean)

class OSGiHeaderParser extends JavaTokenParsers {
	override val whiteSpace = "".r	
	
	def identifier = new Parser[String] {
		def apply(in : Input) = {
			val cs : java.lang.CharSequence = in.source.subSequence(in.offset, in.source.length)
			
			if (java.lang.Character.isJavaIdentifierStart(cs.charAt(0))) {
				var i = 1
				while (i < cs.length && java.lang.Character.isJavaIdentifierPart(cs.charAt(i))) { i+=1 }
				
				Success(cs.subSequence(0,i).toString, in.drop(i))
			} else {
				Failure("Expected jLetter but got `"+cs.charAt(0)+"'",in.drop(in.offset))
			}
		}
	}
	
	def uniqueName = seq(identifier, optOrEmpty(rep(seq(".", identifier)) ^^ (_.mkString)))
	def packageName = uniqueName
	
	def token = "[0-9a-zA-Z_-]+".r
	def quotedString = "\"" ~>
		rep("[^\"\u000D\u000A\u0000\\\\]+".r | "\\\"" ^^ (_ => "\"") | "\\\\" ^^ (_ => "\\")) <~ "\"" ^^ (_.mkString)
	
	def argument = token | quotedString

	def attribute : Parser[Parameter] = token ~ "=" ~ argument ^^ { case u~"="~v => Attribute(u,v) }
	def directive : Parser[Parameter] = token ~ ":=" ~ argument ^^ { case u~":="~v => Directive(u,v) }
	def parameter = attribute | directive
	
	def pathElement = "[^/\"\u000D\u000A\u0000\\\\]+".r
	def pathUnquoted = opt("/") ~ token ~ rep("/"~token ^^ { case u~v => u+v }) ^^
		{ 
			case Some(s)~e~l => s+e+l.mkString
			case None~e~l => e+l.mkString 
		} | "/"
	
	def path = pathUnquoted | quotedString
	
	def header = repsep(clause, ",")
	def clause = withParameters(path)
		
	private def withParameters[T](p : Parser[T]) : Parser[(List[T], List[Parameter])] = 
		p~opt(";"~> (parameter~opt(rep(";"~>parameter)) ^^ {
			case p~Some(ps) => List() -> (List(p) ++ ps)
			case p~None => List() -> List(p)
		} | withParameters(p))) ^^ {
			case p~Some((ps,attrs)) => (List(p) ++ ps) -> attrs
			case p~None => List(p) -> List()
		}
						
	def number = "[0-9]+".r
	def version = seq(number,optOrEmpty(seq(".",seq(number,optOrEmpty(seq(".", seq(number,optOrEmpty(seq(".", token)))))))))
	
	def versionRange = version ^^ { v => VersionRange(v,null,false,false) } | 
		("[" ^^ { _ => false } | "(" ^^ { _ => true } ) ~ version ~ "," ~ version ~ ("]" ^^ { _ => false } | ")" ^^ { _ => true } ) ^^ {
			case b1~lower~_~upper~b2 => VersionRange(lower, upper, b1, b2)
		}
		
	def importPackage = withParameters(packageName)
	def importPackages = repsep(importPackage, ",")		
		
	def parseBundleVersion(v : String) = parseOrNone(version, v) map { new Version(_) }	
	def parseImportPackages(ps : String) = parseOrNone(importPackages, ps)
	
	private def parseOrNone[T](p : Parser[T], s : String) = parseAll(p, s) match {
		case Success(res,_) => Some(res)
		case _ => None
	}
	
	private def optOrEmpty(p : Parser[String]) = 
		opt(p) ^^ {
			case Some(s) => s
			case None => ""
		}
		
	private def seq(p : Parser[String], q : Parser[String]) = p~q ^^ { case x~y => x+y }
}

object OSGiHeaderParser extends OSGiHeaderParser {
	def main(args : Array[String]) {
		System.out.println(parseAll(header, args(0)))
	}
}

