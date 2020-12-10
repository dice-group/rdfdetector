package org.dice_research.rdfdetector

import collection.JavaConverters._
import java.io._
import org.apache.jena.rdf.model.{Model, ModelFactory}
import org.apache.jena.riot._
import org.apache.jena.riot.Lang._
import org.apache.lucene.util.automaton._
import org.rdfhdt.hdt.hdt.HDTManager
import org.rdfhdt.hdtjena.HDTGraph
import scala.collection.mutable

object RdfSerializationDetector {
  /**
   * HDT Lang definition.
   */
  val HDT = LangBuilder.create.langName("HDT").contentType("").build

  val automata = Seq(
    (RdfSerializationDetector.HDT, (new RegExp("$HDT.*")).toAutomaton),
    (N3, buildNotation3),
    (NQUADS, buildNQuads),
    (RDFJSON, buildRDFJSON),
    (JSONLD, buildJSONLD),
    (RDFXML, buildRDFXML),
    (TRIG, buildTriG),
    (TRIX, buildTriX),
  )

  object State {
    val maxLabel = 0x10ffff
    val newLines = Set[Int]('\n', '\r')
    val whitespace = Set[Int](' ', '\t')
    val plusMinus = Set[Int]('+', '-')
    val decimals = Set[Int]('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
    val letters = Set[Int] ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')
  }

  case class State()(implicit val builder: Automaton.Builder) {
    val id = builder.createState
    builder setAccept (id, true)
    val labels = mutable.SortedSet[Int]()

    def addTransition(label: Int, newId: Int) {
      labels += label
      builder addTransition (id, newId, label)
    }

    def ->(labels: Set[Int], other: State) = {
      labels foreach (addTransition(_, other.id))
      other
    }
    def ->(labels: Set[Int]): State = ->(labels, State())
    def ->(label: Int, other: State = State()): State = ->(Set(label), other)
    def ->(str: String): State = str.foldLeft(this)((state, char) => state -> char)
    def ->(fun: (State, State) => State, other: State) = fun(this, other)
    def ->(fun: (State, State) => State): State = ->(fun, State())

    def labelPairs = labels + -1 zip labels + (State.maxLabel + 1)
    def otherwise(other: State) = labelPairs foreach { case(min, max) => builder addTransition (id, other.id, min+1, max-1) }
  }

  def buildXmlWithRoot(tagNameBuilder: State => State) = {
    implicit val builder = new Automaton.Builder
    val initial = State()
    initial -> (State.whitespace ++ State.newLines, initial)
    val openAngular = initial -> '<'

    // XML directives
    val directive = openAngular -> '?'
    directive -> ('>', initial)
    directive otherwise directive

    // XML comments
    val comment = openAngular -> "!--"
    val firstMinus = comment -> '-'
    comment otherwise comment
    val secondMinus = firstMinus -> '-'
    firstMinus otherwise comment
    secondMinus -> ('>', initial)
    secondMinus -> ('-', secondMinus)
    secondMinus otherwise comment

    val good = tagNameBuilder(openAngular)
    good otherwise good

    builder.finish
  }

  def buildRDFXML = buildXmlWithRoot(tagName => tagName -> "rdf:")
  def buildTriX = buildXmlWithRoot(tagName => tagName -> "TriX")

  def prependAny(labelsToPrepend: Set[Int], label: Int) = (before: State, after: State) => before -> (State.whitespace ++ State.newLines, before) -> (label, after)

  def jsonControlCharacter(label: Int) = prependAny(State.whitespace ++ State.newLines, label)

  def jsonString = (state: State, fin: State) => {
    state -> (State.whitespace ++ State.newLines, state)
    val inString = state -> '"'
    val inStringEscaped = inString -> '\\'
    inStringEscaped otherwise inString
    inString -> ('"', fin)
    inString otherwise inString
    fin
  }

  def buildJSONLD = {
    implicit val builder = new Automaton.Builder

    val openBrace: State = State() -> jsonControlCharacter('{')
    val fin: State = openBrace -> jsonControlCharacter('}')
    fin -> (State.whitespace ++ State.newLines) -> fin

    val good: State = openBrace -> jsonString -> jsonControlCharacter(':')

    good otherwise good
    builder.finish
  }

  def buildRDFJSON = {
    implicit val builder = new Automaton.Builder

    val openBrace = State() -> jsonControlCharacter('{')
    val fin = openBrace -> jsonControlCharacter('}')
    fin -> (State.whitespace ++ State.newLines) -> fin

    val good = openBrace -> jsonString -> jsonControlCharacter(':') -> jsonControlCharacter('{') ->
      jsonString -> jsonControlCharacter(':') -> jsonControlCharacter('[')

    good otherwise good
    builder.finish
  }

  def sharpComment = (before: State, after: State) => {
    val comment = before -> '#'
    comment -> (State.newLines, after)
    comment otherwise comment
    after
  }

  def ntTerm(
    blankNode: Boolean = false,
    rdfLiteral: Boolean = false,
    numericLiteral: Boolean = false,
    booleanLiteral: Boolean = false,
    turtle: Boolean = false,
    n3: Boolean = false,
  )(before: State, after: State = null): State = {
    implicit val builder = before.builder
    val _after = if (after != null) after else State()

    var firstChar = before -> '<'

    if (n3) {
      firstChar -> (State.whitespace ++ State.newLines, firstChar)
    }

    // Scheme is always non-empty and begins with a letter.
    val inIRI = firstChar -> State.letters

    inIRI -> ('>', _after)
    inIRI otherwise inIRI

    if (turtle) {
      before -> ('a', _after)
    }

    if (blankNode) {
      val inBlank = before -> '_' -> ':'
      inBlank -> (State.whitespace, _after)
      inBlank otherwise inBlank
    }

    if (rdfLiteral) {
      val inLiteral = before -> '"'
      val inLiteralEscape = inLiteral -> '\\'
      inLiteralEscape otherwise inLiteral
      val afterLiteral = inLiteral -> '"'
      inLiteral otherwise inLiteral

      afterLiteral -> (State.whitespace, _after)

      val inLang = afterLiteral -> '@'
      inLang -> (State.whitespace, _after)
      inLang otherwise inLang

      val inType = afterLiteral -> '^' -> '^' -> '<'
      inType -> ('>', _after)
      inType otherwise inType
    }

    if (numericLiteral || booleanLiteral) {
      val good = State()
      good otherwise good

      if (numericLiteral) {
        before -> (State.plusMinus ++ State.decimals, good)
        before -> ('.', good)
      }

      if (booleanLiteral) {
        before -> 't' -> 'r' -> 'u' -> ('e', good)
        before -> 'f' -> 'a' -> 'l' -> 's' -> ('e', good)
      }
    }

    _after -> (State.whitespace, _after)
    if (turtle) {
      _after -> (State.newLines, _after)
    }
    _after
  }

  def buildTriG = {
    implicit val builder = new Automaton.Builder
    val initial = State()
    initial -> (State.whitespace ++ State.newLines, initial)

    sharpComment(initial, initial)

    val afterSubject = initial -> (ntTerm(blankNode = true, turtle = true): (State, State) => State)
    val afterPredicate = afterSubject -> (ntTerm(turtle = true): (State, State) => State)
    val afterObject = afterPredicate -> (ntTerm(blankNode = true, rdfLiteral = true, numericLiteral = true, booleanLiteral = true, turtle = true): (State, State) => State)
    afterObject -> (';', afterSubject)
    afterObject -> (',', afterSubject)
    val afterDot = afterObject -> '.'
    afterDot -> (State.whitespace, afterDot)
    sharpComment(afterDot, initial)
    afterDot -> (State.newLines, initial)

    val directive = initial -> '@'
    val good = directive -> "prefix" // prefixID
    directive -> 'b' -> 'a' -> 's' -> ('e', good) // base
    initial -> 'P' -> 'R' -> 'E' -> 'F' -> 'I' -> ('X', good) // sparqlPrefix
    initial -> 'B' -> 'A' -> 'S' -> ('E', good) // sparqlBase
    initial -> ('[', good) // blankNodePropertyList
    initial -> ('(', good) // collection
    initial -> 'G' -> 'R' -> 'A' -> 'P' -> ('H', good)

    val wrappedGraph = initial -> '{'
    wrappedGraph -> (State.whitespace ++ State.newLines, wrappedGraph)
    wrappedGraph -> (ntTerm(blankNode = true, turtle = true): (State, State) => State, afterSubject)
    wrappedGraph -> ('}', initial)

    good otherwise good
    builder.finish
  }

  def buildNotation3 = {
    implicit val builder = new Automaton.Builder
    val initial = State()
    initial -> (State.whitespace ++ State.newLines, initial)

    sharpComment(initial, initial)

    val afterSubject = initial -> (ntTerm(blankNode = true, turtle = true, n3 = true): (State, State) => State)
    val afterPredicate = afterSubject -> (ntTerm(turtle = true, n3 = true): (State, State) => State)
    val afterObject = afterPredicate -> (ntTerm(blankNode = true, rdfLiteral = true, numericLiteral = true, booleanLiteral = true, turtle = true, n3 = true): (State, State) => State)
    afterObject -> (';', afterSubject)
    afterObject -> (',', afterSubject)
    val afterDot = afterObject -> '.'
    afterDot -> (State.whitespace, afterDot)
    sharpComment(afterDot, initial)
    afterDot -> (State.newLines, initial)

    val directive = initial -> '@'
    val good = directive -> "prefix"
    directive -> 'b' -> 'a' -> 's' -> ('e', good)
    directive -> 'f' -> 'o' -> 'r' -> 'S' -> 'o' -> 'm' -> ('e', good)
    directive -> 'f' -> 'o' -> 'r' -> 'A' -> 'l' -> ('l', good)
    directive -> 'k' -> 'e' -> 'y' -> 'w' -> 'o' -> 'r' -> 'd' -> ('s', good)
    initial -> ('[', good)
    initial -> ('(', good)

    good otherwise good
    builder.finish
  }

  def buildNQuads = {
    implicit val builder = new Automaton.Builder
    val initial = State()
    initial -> (State.whitespace ++ State.newLines, initial)

    sharpComment(initial, initial)

    val afterThreeTerms = initial ->
      (ntTerm(blankNode = true): (State, State) => State) ->
      (ntTerm(): (State, State) => State) ->
      (ntTerm(blankNode = true, rdfLiteral = true): (State, State) => State)
    val afterFourTerms = afterThreeTerms -> (ntTerm(blankNode = true): (State, State) => State)
    afterThreeTerms -> (State.whitespace, afterThreeTerms)
    afterFourTerms -> (State.whitespace, afterFourTerms)
    val afterDot = afterThreeTerms -> '.'
    afterFourTerms -> ('.', afterDot)
    afterDot -> (State.whitespace, afterDot)
    sharpComment(afterDot, initial)
    afterDot -> (State.newLines, initial)

    val noDot = afterThreeTerms -> State.newLines
    afterFourTerms -> (State.newLines, noDot)
    sharpComment(afterThreeTerms, noDot)
    sharpComment(afterFourTerms, noDot)

    noDot -> (State.whitespace ++ State.newLines, noDot)
    sharpComment(noDot, noDot)

    builder.finish
  }
}

/**
 * Class which can detect RDF serialization used on a given InputStream.
 */
class RdfSerializationDetector(readLimit: Int = 300) {
  private[rdfdetector] def _detect(input: BufferedInputStream): Seq[Lang] = try {
    // Set initial states (0) for all automata.
    var runs = RdfSerializationDetector.automata map { case (l, a) => (l, a, 0) }

    input mark readLimit

    val reader = new InputStreamReader(input, RdfEncodingDetector.detect(input))

    // Continue until there is only one automaton left or the read limit is reached.
    var readAmount = 0
    while (runs.length > 1 && readAmount < readLimit) {
      val label: Int = reader.read()

      if (readAmount == 0 && label == 0xfeff) {
        // Skip Unicode byte order mark
      } else {
        if (label != -1) {
          // Move all automata to the next state and filter out ones without defined transitions.
          runs = runs map { case (l, a, s) => (l, a, a step (s, label)) } filter { case (_, a, s) => s != -1 }
          readAmount += 1
        } else {
          readAmount = readLimit
        }
      }
    }

    runs map { _._1 }
  } finally {
    // Reset the given InputStream to its original position.
    input.reset
  }

  /**
   * Detect possible RDF formats for a given InputStream.
   *
   * Returns an array of format strings which can be used with Jena.
   */
  def detect(input: BufferedInputStream): java.util.Collection[Lang] = _detect(input).asJava

  /**
   * Read the given InputStream into a Jena Model (with format detection).
   */
  def readModel(input: BufferedInputStream): Model = _detect(input).head match {
    case RdfSerializationDetector.HDT => ModelFactory.createModelForGraph(new HDTGraph(HDTManager.loadHDT(input), true))
    case lang => {
      val model = ModelFactory.createDefaultModel
      RDFParser.create.source(input).lang(lang) parse model.getGraph
      model
    }
  }
}
