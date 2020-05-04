package org.dice_research.rdfdetector

import collection.JavaConverters._
import java.io._
import java.nio.file.{Files, Paths}
/*
import org.apache.jena.rdf.model.ModelFactory
*/
import org.apache.jena.riot.Lang
import org.apache.jena.riot.Lang._
import org.scalatest._

class Test extends FunSuite with Matchers with OptionValues with Inside with Inspectors {
  val parameters: Seq[(String, Seq[Lang])] = Seq(
    ("n3-whitespace-in-url", Seq(N3)),
    ("n3-newline-in-url", Seq(N3, NQUADS, TRIG)), // IRI automaton is not strict enough
    ("nquads", Seq(NQUADS)),
    ("nquads-tricky", Seq(N3, NQUADS, TRIG)), // file is too long to find the first quad
    ("turtle-keyword", Seq(N3, TRIG)),
    ("hdt-lookalike", Seq(RdfSerializationDetector.HDT)),
    ("json-empty", Seq(RDFJSON, JSONLD, TRIG)),
    ("jsonld1", Seq(JSONLD)),
    ("jsonld2", Seq(JSONLD)),
    ("jsonld3", Seq(JSONLD)),
    // ("rdfjson", Seq(RDFJSON)),
    ("rdfxml-cp1251", Seq(RDFXML)),
    ("rdfxml-xml-1.0", Seq(RDFXML)),
    ("trix-cp1251", Seq(TRIX)),
    ("trix-tricky-n3", Seq(TRIX)),
    ("trix-tricky-nquads", Seq(TRIX)),
    ("trix-utf8-bom", Seq(TRIX)),
    ("trix-utf16be", Seq(TRIX)),
    ("trix-utf16be-bom", Seq(TRIX)),
    ("trix-utf16le", Seq(TRIX)),
    ("trix-utf16le-bom", Seq(TRIX)),
    ("trix-utf32be", Seq(TRIX)),
    ("trix-utf32be-bom", Seq(TRIX)),
    ("trix-utf32le", Seq(TRIX)),
    ("trix-utf32le-bom", Seq(TRIX)),
    ("trix-xml-1.0", Seq(TRIX)),
    ("xml-long", Seq(RDFXML, TRIX)),
    // Files from Squirrel tests.
    ("genders_en_turtle", Seq(N3, TRIG)),
    ("new_york_n3", Seq(N3, TRIG)),
  )

  val instance = new RdfSerializationDetector

  test("detect") {
    forAll(parameters) {
      case (file: String, languages: Seq[Lang]) => assertResult(languages, (file, languages))(instance._detect(resourceStream(file)))
    }
  }

  /*
  test("read") {
    forAll(parameters) {
      case (file: String, _) => instance.read(ModelFactory.createDefaultModel, resourceStream(file))
    }
  }
  */

  def resourceStream(name: String) = new BufferedInputStream(getClass getResourceAsStream ("/" + name))

  def dotPath(l: Lang) = "docs/figures/" + l.toString.replaceAll("[^a-zA-Z0-9]", "") + ".dot"

  // Save the dot (graphviz) representations of all automata.
  RdfSerializationDetector.automata foreach {
    case (lang, a) => Files.write(Paths.get(dotPath(lang)), a.toDot().getBytes)
  }
}
