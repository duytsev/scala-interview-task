import org.scalatest.FlatSpec

class ProcessorSpec extends FlatSpec {

  final val BasicInput: String = "сапог сарай арбуз болт бокс биржа"
  final val CapitalizedInput: String = "сапог Сарай арбуз болт БоКс биржа"
  final val SpacesInput: String = " сапог       сарай арбуз болт    бокс биржа "
  final val DuplicateWordsInput = "арбуз сапог сарай арбуз болт бокс биржа"
  final val LatinWordsInput = "арбуз Apple сапог сарай acid болт бокс биржа арбуз"

  val proc = new Processor

  "A processor" should "process basic string" in {
    val res: String = proc.process(BasicInput)
    assert(res == "[б=[биржа, бокс, болт], с=[сапог, сарай]]")
  }

  it should "process string with Capitalized words" in {
    val res: String = proc.process(CapitalizedInput)
    assert(res == "[б=[биржа, бокс, болт], с=[сапог, сарай]]")
  }

  it should "process string with leading, trailing and multiple spaces" in {
    val res: String = proc.process(SpacesInput)
    assert(res == "[б=[биржа, бокс, болт], с=[сапог, сарай]]")
  }

  it should "process string with duplicate words" in {
    val res: String = proc.process(DuplicateWordsInput)
    assert(res == "[а=[арбуз, арбуз], б=[биржа, бокс, болт], с=[сапог, сарай]]")
  }

  it should "process string with latin words" in {
    val res: String = proc.process(LatinWordsInput)
    assert(res == "[a=[apple, acid], а=[арбуз, арбуз], б=[биржа, бокс, болт], с=[сапог, сарай]]")
  }

  it should "return empty group ('[]') if no groups found" in {
    val res1: String = proc.process("машина")
    assert(res1 == "[]")

    val res2: String = proc.process("машина сапог")
    assert(res2 == "[]")
  }

  it should "throw IllegalArgumentException when input is empty" in {
    assertThrows[IllegalArgumentException] {
      proc.process("")
    }

    assertThrows[IllegalArgumentException] {
      proc.process(null)
    }
  }
}