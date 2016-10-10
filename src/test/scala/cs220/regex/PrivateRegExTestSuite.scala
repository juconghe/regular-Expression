package cs220.evaluator

import cs220.regex._
import org.scalatest.FunSuite

class PrivateRegExTestSuite extends FunSuite {
  test("A regular expression /a*c*/ should match \"\"") {
    val S  = Factory.re
    val re = S.seq(S.closure(S.char('a')), S.closure(S.char('c')))
    assert((re matches "") == true)
  }

  test("A regular expression /a*c*/ should match c") {
    val S  = Factory.re
    val re = S.seq(S.closure(S.char('a')), S.closure(S.char('c')))
    assert((re matches "c") == true)
  }

  test("A regular expression /a*c*/ should match a") {
    val S  = Factory.re
    val re = S.seq(S.closure(S.char('a')), S.closure(S.char('c')))
    assert((re matches "a") == true)
  }

  test("A regular expression /cc*/ should match c") {
    val S  = Factory.re
    val re = S.seq(S.closure(S.char('a')), S.closure(S.char('c')))
    assert((re matches "c") == true)
  }

  test("A regular expression /cc*/ should match cc") {
    val S  = Factory.re
    val re = S.seq(S.closure(S.char('a')), S.closure(S.char('c')))
    assert((re matches "cc") == true)
  }

  test("A regular expression /cc*/ should match cccccc") {
    val S  = Factory.re
    val re = S.seq(S.closure(S.char('a')), S.closure(S.char('c')))
    assert((re matches "cccccc") == true)
  }

  test("A regular expression /(a|b)(c|d)x*(y|z)/ should match acy") {
    val S  = Factory.re
    val re = S.seq(S.alt(S.char('a'), S.char('b')),
                   S.seq(S.alt(S.char('c'), S.char('d')),
                         S.seq(S.closure(S.char('x')),
                               S.seq(S.alt(S.char('y'), S.char('z')),
                                     S.empty))))
    assert((re matches "acy") == true)
  }

  test("A regular expression /(a|b)(c|d)x*(y|z)/ should match ady") {
    val S  = Factory.re
    val re = S.seq(S.alt(S.char('a'), S.char('b')),
                   S.seq(S.alt(S.char('c'), S.char('d')),
                         S.seq(S.closure(S.char('x')),
                               S.seq(S.alt(S.char('y'), S.char('z')),
                                     S.empty))))
    assert((re matches "ady") == true)
  }

  test("A regular expression /(a|b)(c|d)x*(y|z)/ should match bcy") {
    val S  = Factory.re
    val re = S.seq(S.alt(S.char('a'), S.char('b')),
                   S.seq(S.alt(S.char('c'), S.char('d')),
                         S.seq(S.closure(S.char('x')),
                               S.seq(S.alt(S.char('y'), S.char('z')),
                                     S.empty))))
    assert((re matches "bcy") == true)
  }

  test("A regular expression /(a|b)(c|d)x*(y|z)/ should match bdy") {
    val S  = Factory.re
    val re = S.seq(S.alt(S.char('a'), S.char('b')),
                   S.seq(S.alt(S.char('c'), S.char('d')),
                         S.seq(S.closure(S.char('x')),
                               S.seq(S.alt(S.char('y'), S.char('z')),
                                     S.empty))))
    assert((re matches "bdy") == true)
  }

  test("A regular expression /(a|b)(c|d)x*(y|z)/ should match abxxxxxz") {
    val S  = Factory.re
    val re = S.seq(S.alt(S.char('a'), S.char('b')),
                   S.seq(S.alt(S.char('c'), S.char('d')),
                         S.seq(S.closure(S.char('x')),
                               S.seq(S.alt(S.char('y'), S.char('z')),
                                     S.empty))))
    assert((re matches "acxxxxxz") == true)
  }

  test("A regular expression /(a|b)(c|d)x*(y|z)/ should match acxz") {
    val S  = Factory.re
    val re = S.seq(S.alt(S.char('a'), S.char('b')),
                   S.seq(S.alt(S.char('c'), S.char('d')),
                         S.seq(S.closure(S.char('x')),
                               S.seq(S.alt(S.char('y'), S.char('z')),
                                     S.empty))))
    assert((re matches "acxz") == true)
  }

  test("A regular expression /(a|b)(c|d)x*(y|z)/ should not match abxz") {
    val S  = Factory.re
    val re = S.seq(S.alt(S.char('a'), S.char('b')),
                   S.seq(S.alt(S.char('c'), S.char('d')),
                         S.seq(S.closure(S.char('x')),
                               S.seq(S.alt(S.char('y'), S.char('z')),
                                     S.empty))))
    assert((re matches "abxz") == false)
  }

  test("A regular expression /(a|b)(c|d)x*(y|z)/ should not match abcdxyz") {
    val S  = Factory.re
    val re = S.seq(S.alt(S.char('a'), S.char('b')),
                   S.seq(S.alt(S.char('c'), S.char('d')),
                         S.seq(S.closure(S.char('x')),
                               S.seq(S.alt(S.char('y'), S.char('z')),
                                     S.empty))))
    assert((re matches "abcdxyz") == false)
  }

  test("A regular expression /x*x*x/ should not match ''") {
    val S  = Factory.re
    val re = S.seq(S.closure(S.char('x')),
                   S.seq(S.closure(S.char('x')),
                         S.seq(S.char('x'), S.empty)))
    assert((re matches "") == false)
  }

  test("A regular expression /x*x*x/ should match 'x'") {
    val S  = Factory.re
    val re = S.seq(S.closure(S.char('x')),
                   S.seq(S.closure(S.char('x')),
                         S.seq(S.char('x'), S.empty)))
    assert((re matches "x") == true)
  }

  test("A regular expression /x*x*x/ should match 'xx'") {
    val S  = Factory.re
    val re = S.seq(S.closure(S.char('x')),
                   S.seq(S.closure(S.char('x')),
                         S.seq(S.char('x'), S.empty)))
    assert((re matches "xx") == true)
  }

  test("A regular expression /x*x*x/ should match 'xxx'") {
    val S  = Factory.re
    val re = S.seq(S.closure(S.char('x')),
                   S.seq(S.closure(S.char('x')),
                         S.seq(S.char('x'), S.empty)))
    assert((re matches "xxx") == true)
  }

  test("A regular expression /x*x*x/ should match 'xxxx'") {
    val S  = Factory.re
    val re = S.seq(S.closure(S.char('x')),
                   S.seq(S.closure(S.char('x')),
                         S.seq(S.char('x'), S.empty)))
    assert((re matches "xxxx") == true)
  }

  test("A regular expression /I rock/ should match 'I rock'") {
    val S  = Factory.re
    val re = S.str("I rock")
    assert((re matches "I rock") == true)
  }

  test("A regular expression /I rock/ should not match 'Irock'") {
    val S  = Factory.re
    val re = S.str("I rock")
    assert((re matches "Irock") == false)
  }

  test("A regular expression /I (rock|suck)/ should match 'I rock'") {
    val S  = Factory.re
    val re = S.seq(S.str("I "),
                   S.alt(S.str("rock"),
                         S.str("suck")))
    assert((re matches "I rock") == true)
  }

  test("A regular expression /I (rock|suck)/ should match 'I suck'") {
    val S  = Factory.re
    val re = S.seq(S.str("I "),
                   S.alt(S.str("rock"),
                         S.str("suck")))
    assert((re matches "I suck") == true)
  }

  test("A regular expression /I (rock|suck)/ should not match 'I '") {
    val S  = Factory.re
    val re = S.seq(S.str("I "),
                   S.alt(S.str("rock"),
                         S.str("suck")))
    assert((re matches "I ") == false)
  }

  test("A regular expression /I (rock|suck)/ should not match 'suck'") {
    val S  = Factory.re
    val re = S.seq(S.str("I "),
                   S.alt(S.str("rock"),
                         S.str("suck")))
    assert((re matches "suck") == false)
  }
}