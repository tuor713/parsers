package org.uwh.osgi.parsers

import org.scalatest.junit._
import org.junit._
import Assert._
import org.osgi.framework._

class ParserTest extends OSGiHeaderParser {
	
	@Test def testTokens {
		assertEquals("alphaOnly", parseAll(token, "alphaOnly").get)
		assertEquals("8472", parseAll(token, "8472").get)
		assertFalse(parseAll(token,"\"tuhe").successful)
	}
	
	@Test def testQuotedStrings {
		assertEquals("hello", parseAll(quotedString, "\"hello\"").get)
		assertEquals("\"", parseAll(quotedString, "\"\\\"\"").get)
		assertEquals("hello \"", parseAll(quotedString, "\"hello \\\"\"").get)
	}
	
	@Test def testArguments {
		assertEquals("a-B_43", parseAll(argument, "a-B_43").get)
		assertEquals("hello \" world", parseAll(argument, "\"hello \\\" world\"").get)
	}
	
	@Test def testAttribute {
		assertEquals(Attribute("attr","val"), parseAll(attribute,"attr=val").get)
		assertEquals(Attribute("attr","strange, characters"), parseAll(attribute,"attr=\"strange, characters\"").get)
		assertFalse(parseAll(attribute,"attr=not,valid").successful)
	}
	
	@Test def testDirective {
		assertEquals(Directive("dir","value"), parseAll(directive,"dir:=value").get)
		assertEquals(Directive("dir",", #"), parseAll(directive,"dir:=\", #\"").get)
		assertFalse(parseAll(directive, "dir:=,").successful)
	}
	
	@Test def testParameter {
		assertEquals(Attribute("attr",":="), parseAll(parameter, "attr=\":=\"").get)
		assertEquals(Directive("dir",""), parseAll(parameter,"dir:=\"\"").get)
	}
	
	@Test def testPathElement {
		assertEquals("path", parseAll(pathElement, "path").get)
		assertFalse(parseAll(pathElement,"some/root").successful)
	}
	
	@Test def testPath {
		assertEquals("/", parseAll(path, "/").get)
		assertEquals("/", parseAll(path, "\"/\"").get)
		assertEquals("/some/unix/path", parseAll(path, "/some/unix/path").get)
		assertEquals("some/nonunix/path", parseAll(path, "\"some/nonunix/path\"").get)
	}
	
	@Test def testClause {
		assertEquals(List("/one", "/two", "/", "three") -> List(), parseAll(clause,"/one;/two;/;three").get)
		assertEquals(List("path") -> List(Attribute("attr","val")), parseAll(clause,"path;attr=val").get)
		System.out.println(parseAll(clause, "one;two;attr=val;dir:=\"weird;shit\""))
		assertEquals(List("one","two") -> List(Attribute("attr","val"), Directive("dir","weird;shit")),
			parseAll(clause, "one;two;attr=val;dir:=\"weird;shit\"").get)
		assertFalse(parseAll(clause, "path;attr=val;another/path").successful)
		assertFalse(parseAll(clause, "").successful)
	}
	
	@Test def testHeader {
		assertEquals(List(List("path") -> List(Attribute("attr","val"))), parseAll(header, "path;attr=val").get)
		assertEquals(
			List(List("one","two") -> List(Attribute("attr","val")), 
				List("path") -> List(Directive("dir","\",;"))),
			parseAll(header, "one;two;attr=val,path;dir:=\"\\\",;\"").get)
	}
	
	@Test def testVersion {
		assertEquals("1", parseAll(version, "1").get)
		assertEquals("0.0.0", parseAll(version, "0.0.0").get)
		assertEquals("1.0.0.qua_li-fier", parseAll(version, "1.0.0.qua_li-fier").get)
		assertFalse(parseAll(version, "1.0.qualifier").successful)
		assertFalse(parseAll(version, "1.0.0-qualifier").successful)
		assertFalse(parseAll(version, "1.0.0.wrong qualifier").successful)
	}
	
	@Test def testVersionRange {
		assertEquals(VersionRange("1.0.0",null,false,false), parseAll(versionRange, "1.0.0").get)
		assertEquals(VersionRange("1","2",false,true), parseAll(versionRange, "[1,2)").get)
		assertEquals(VersionRange("1","2",true,false), parseAll(versionRange, "(1,2]").get)
		assertEquals(VersionRange("1.0.0.qualifier","2.0.0", false, false), parseAll(versionRange, "[1.0.0.qualifier,2.0.0]").get)
	}
	
	@Test def testParseBundleVersion {
		assertEquals(Some(new Version("1.0.0")), parseBundleVersion("1.0.0"))
		assertEquals(None, parseBundleVersion("1.0.a"))
		assertEquals(Some(new Version(1,2,3,"qual")), parseBundleVersion("1.2.3.qual"))
	}

	@Test def testImportPackages {
		assertEquals(List(List("org.uwh","org.uwh.util") -> List(Attribute("version","1.0.0")), 
			List("java.util") -> List(Directive("resolution","jdk"))), 
			parseAll(importPackages, "org.uwh;org.uwh.util;version=\"1.0.0\",java.util;resolution:=jdk").get)
	}



	
}