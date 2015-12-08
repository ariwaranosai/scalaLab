package parsercombinators

import org.scalatest.{Matchers, FlatSpec}


/**
  * Created by ariwaranosai on 15/12/7.
  */
class OXOParserTest extends FlatSpec with Matchers {
    import parsercombinators.OXOParser.{Success, Failure}
    val oxo = OXOParser.oxo
    val oxos = OXOParser.oxos

    "oxo parser" should "match oxo right" in {
        oxo("oxo") should matchPattern { case Success((('o', 'x'), 'o'), "") => }
        oxo("ooxo") should matchPattern { case Failure(_, _) => }
    }

    "oxos parser" should "match oxos right" in {
        oxos("oxo") should matchPattern {case Success(_, _) => }
        oxos("oxo oxo oxo") should matchPattern {case Success(_, _) => }
        oxos("o") should matchPattern {case Failure(_, _) => }
    }

}
