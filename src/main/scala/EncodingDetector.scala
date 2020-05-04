package org.dice_research.rdfdetector

/**
 * Class which can detect encoding of RDF serialization used on a given InputStream.
 */
object RdfEncodingDetector {
  /**
   * Detect possible encoding of RDF serialization for a given InputStream.
   *
   * Returns a string name of encoding.
   */
  def detect(input: java.io.InputStream): String = try {
    input mark 4
    detect(input.read(), input.read(), input.read(), input.read())
  } finally {
    // Reset the given InputStream to its original position.
    input.reset
  }

  def detect(bytes: Int*) = bytes match {
    case Seq(0x00, 0x00, 0xfe, 0xff) => "UTF-32BE"
    case Seq(0xff, 0xfe, 0x00, 0x00) => "UTF-32LE"
    case Seq(0x00, 0x00, 0x00, _)    => "UTF-32BE"
    case Seq(_,    0x00, 0x00, 0x00) => "UTF-32LE"
    case Seq(0xfe, 0xff, _,    _)    => "UTF-16BE"
    case Seq(0xff, 0xfe, _,    _)    => "UTF-16LE"
    case Seq(0x00, _,    0x00, _)    => "UTF-16BE"
    case Seq(_,    0x00, _,    0x00) => "UTF-16LE"
    case _ => "UTF-8"
  }
}
