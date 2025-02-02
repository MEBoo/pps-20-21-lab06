package u06lab.solution

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class TryParsers {
  def parser = new BasicParser(Set('a', 'b', 'c'))
  // Note NonEmpty being "stacked" on to a concrete class
  // Bottom-up decorations: NonEmptyParser -> NonEmpty -> BasicParser -> Parser
  def parserNE = new NonEmptyParser(Set('0', '1'))
  def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
  // note we do not need a class name here, we use the structural type
  def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]
  // Bottom-up decorations: BasicParser -> NotTwoConsecutive -> NonEmpty ->  Parser

  import BasicParser._ // si può evitare l'import in questo caso? non ho trovato il modo...
  def sparser: Parser[Char] = "abc".charParser()

  @Test
  def testBasicParser = {
    assertTrue(parser.parseAll("aabc".toList))
    assertFalse(parser.parseAll("aabcdc".toList))
    assertTrue(parser.parseAll("".toList))
  }

  @Test
  def testNotEmptyParser = {
    assertTrue(parserNE.parseAll("0101".toList))
    assertFalse(parserNE.parseAll("0123".toList))
    assertFalse(parserNE.parseAll(List()))
  }

  @Test
  def testNotTwoConsecutiveParser = {
    assertTrue(parserNTC.parseAll("XYZ".toList))
    assertFalse(parserNTC.parseAll("XYYZ".toList))
    assertTrue(parserNTC.parseAll("".toList))
  }

  @Test
  def testNotEmptyAndNotTwoConsecutiveParser = {
    assertTrue(parserNTCNE.parseAll("XYZ".toList))
    assertFalse(parserNTCNE.parseAll("XYYZ".toList))
    assertFalse(parserNTCNE.parseAll("".toList))
  }

  @Test
  def testStringParser = {
    assertTrue(sparser.parseAll("aabc".toList))
    assertFalse(sparser.parseAll("aabcdc".toList))
    assertTrue(sparser.parseAll("".toList))
  }
}


