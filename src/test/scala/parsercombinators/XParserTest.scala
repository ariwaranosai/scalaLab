package parsercombinators

import org.scalatest.{WordSpecLike, Matchers, FlatSpec}

/**
  * Created by ariwaranosai on 15/12/7.
  */

class XParserTest extends FlatSpec with Matchers {
    "XParser" should "resolve empty right" in {
        assert(XParser.acceptX("") == XParser.Failure("expected an x", ""))
    }

    it should "resolve string contain x right" in {
        assert(XParser.acceptX("xabcd") == XParser.Success('x', "abcd"))
    }

}
