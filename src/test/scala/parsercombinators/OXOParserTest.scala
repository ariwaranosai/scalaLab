package parsercombinators

import org.scalatest.{Matchers, FlatSpec}


/**
  * Created by ariwaranosai on 15/12/7.
  */
class OXOParserTest extends FlatSpec with Matchers {
    import parsercombinators.OXOParser.{Success, Failure}
    val oxo = OXOParser.oxo
    "oxo parser" should "match oxo right" in {
        oxo("oxo") should matchPattern { case Success((('o', 'x'), 'o'), "") => }
        oxo("ooxo") should matchPattern { case Failure(_, _) => }
    }

}
