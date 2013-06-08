package logic.resolution
//moved all the test classes into a separate file

import scala.collection._
import scala.util.matching.Regex
import scala.util.control.Exception._

object RunTests extends App {
  val tests = List(
    //new TestTakeLiteralApart,
    //new TestVariableFindAndReplace,
    new TestClause,
    new TestCNF,
    new TestInferenceRule,
    new ResolutionTest
  )

  tests foreach { _.testme }
}

trait TestMe {
  def testme:Unit
}

class TestTakeLiteralApart extends TakeLiteralApart with TestMe {
  def testme {
    List("p(f(a), g(g(X)), Y)", "p", "q(f(a, b), X, Y,   f(X, Y))").foreach { literal =>
      println("Taking apart " + literal + " : ")
      val (pred, args) = separatePredArg(literal)
      println("P=" + pred.toString + ".")
      args.foreach ( a => println("a: " + a))
    }

    println
    println("testing separating literal and negation: -p(a) : " + separateLiteralAndNegation("-p(a)"))
    println("testing separating literal and negation: q(X,Y,Z) : " + separateLiteralAndNegation("q(X,Y,Z)"))
    println("testing separating literal and negation: X : " + separateLiteralAndNegation("X"))
  }
}

class TestVariableFindAndReplace extends VariableFindAndReplace with TestMe {
  def testme {
    println("testing isVariable on X:" + isVariable("X"))
    println("testing isVariable on p(X):" + isVariable("p(X)"))
    println("testing isVariable on aX:" + isVariable("aX"))
    println

    println("testing takeTermApart on p(f(a, XX),   g(g(g(a_23Q))), Y,Z):")
    takeTermApart("p(f(a, XX),   g(g(g(a_23Q))), Y,Z)").foreach (piece =>
      println("--" + piece.toString + "--"))
    println

    println("testing occursCheck on X and p(f(g(g(Y,aX)))): " + occursCheck("X", "p(f(g(g(Y,aX))))"))
    println("testing occursCheck on X and p(f(g(g(Y,X)))): " +  occursCheck("X", "p(f(g(g(Y,X))))"))
    println

    println("testing applySubstitution on p(g(g(Y,X, aX)), Z,bY) and X->c,Y->d : " + 
	    applySubstitution("p(g(g(Y,X, aX)), Z,bY)", newSubstitutionInit("X" -> "c", "Y" -> "d")))
    
    println("testing applySubstitution on U and U->a,Y->d : " + 
	    applySubstitution("U", newSubstitutionInit("U" -> "a", "Y" -> "d")))
    
  }
}

class TestUnification extends Unification with TestMe {
  def testme = {
    val subs = addSubstitution("X", "a", newSubstitution)
    println("testing addSubstitution: adding X->a to an empty substitution : "+  subs)
    val subs2 = addSubstitution("Y", "Z", subs)
    println("testing addSubstitution: adding Y->Z to the previous substitution : " +  subs2)
    val subs3 = addSubstitution("Z", "f(U)", subs2)
    println("testing addSubstitution: adding Z->f(U) to the previous  substitution : " +  subs3)
    val subs4 = addSubstitution("U", "b", subs3)
    println("testing addSubstitution: adding U->b to the previous  substitution  : " +  subs4)

    println

    println("testing unification: Y=X,a=X,Z=f(f(Y)) : " + computeSubstitution(List("Y", "a", "Z"), List("X", "X", "f(f(Y))")))     
    println("testing unification: X=Y,Y=Z,Z=X : " + computeSubstitution(List("X", "Y", "Z"), List("Y", "Z", "X")))
    println("testing unification: X=X : " + computeSubstitution(List("X"), List("X")))
    println("testing unification: X=Y,Y=Z,Z=f(X) : " + computeSubstitution(List("X", "Y", "Z"), List("Y", "Z", "f(X)")))
    println("testing unification: g(g(a))=g(g(a)),f(X)=f(h(b)) : " + computeSubstitution(List("g(g(a))", "f(X)"), List("g(g(a))", "f(h(b))")))
  }
}

class TestLiteral extends TestMe {
  def testme { 
    val litlist = List("p(a, f(X), g(g(g(Y,X))))", "-X", "-g(Y)")
    litlist.foreach { ls =>
      val l = Literal(ls)
      println("posLiteral, isNegated, predicate for " + ls + " : " + l.posLiteralString + " " + l.isNegated + 
	      " " + l.predSymbol)
      l.argList.foreach (a => println("argument " + a))
      println("arity: " + l.arity)
      println("isContent: " + l.isContent)
    }
    println

    val litpairs = List(("p(a)", "-p(b)"), ("p(a)", "-p(X)"), ("p(X)", "-p(p(X))"), ("p(X)", "-p(a, b)"), 
			("p(a, b, c)", "p(a, b, c)"), ("p(a, b, g(c))", "p(a, b,  g( c ))"),
			("p(a, b, c)", "-p(  a,b,   c )"), ("X", "-X"), ("p(X, Y)", "-p(Y, Z)"))

    litpairs.foreach { case (l1, l2) =>
		       val l1L = Literal(l1)
		       val l2L = Literal(l2)
		       println("Comparing: " + l1L.toString + " || " + l2L.toString)
		       println("possible match: " + l1L.possibleMatch(l2L))
		       println("unifiableAndNegated: " + l1L.unifiableAndNegated(l2L))
		       println("equals: " + l1L.equals(l2L))
		       println("isNegationOf: " + l1L.isNegationOf(l2L))
		     }
    
    println
    var l1 = Literal("p(X, g(g(Y)), ZZ, f(Z))")
    val subst = newSubstitutionInit("X"->"a", "Y"->"b", "Z"->"c")
    println("applying substitution to " + l1 + " : " + subst)
    val l1p = l1.applySubstitution(subst)
    println("result: " + l1p)

    println
    val l2 = Literal("-p(f(a, b, c, g(g(X))))")
    println("is -p(f(a, b, c, g(g(X)))) grounded? " + l2.isGrounded)
    val l2p = l2.applySubstitution(newSubstitutionInit("X"->"f(c)"))
    println("after applying substitution X->f(c) : " + l2p.isGrounded)
    println("is -a grounded? " + Literal("-a").isGrounded)
    println("is -X grounded? " + Literal("-X").isGrounded)
  }
}

class TestClause extends TestMe {
  def testme {
    println("test clause")
    val litlist = List("p(a)", "-p(a)", "p(a,a)", "p(a)")
    var cl1 = new Clause(litlist)
    println("clause from literal list " + litlist)
    println(cl1)
    println
    println("is singleton: " + cl1.isSingleton)
    println("is empty: " + cl1.isEmpty)
    println("size: " + cl1.size)
    println

    val litlist2 = List("p(a)", "p(X)")
    var cl2 = new Clause(litlist2)
    println(cl2)
    cl2 = cl2.applySubstitution(newSubstitutionInit("X"->"a"))
    println(cl2)

    val litlist3 = List("p(a)", "-p(X)")
    var cl3 = new Clause(litlist3)
    println(cl3)
    try { 
      cl3.applySubstitution(newSubstitutionInit("X"->"a"))
    } catch {
      case e: EmptyClauseException => println("got an empty clause exception")
    } 

    println(cl2.toList)

  }

}

class TestCNF extends TestMe {
  def testme {
    println
    println("test cnf")
    println
    val cnfList1 = List(List("-q(a)"), List("p(a)", "p(X)"), List("p(a)", "-p(a)", "p(g(X))"))
    var cnf1 = new CNF(cnfList1)
    println(cnf1)
    println("clause indices " + cnf1.clauseIndices)
    println("doing ResolveAsSingleton on 0 with substitution X->b")
    cnf1 = cnf1.resolveAsSingleton(0, newSubstitutionInit("X" -> "b"))
    println(cnf1)
    println("trying ResolveAsSingleton with nonexistent clause")
    try {
      cnf1 = cnf1.resolveAsSingleton(3, newSubstitution)
    } catch {
      case e: Exception => println("caught an exception")
    }

    println("clause indices " + cnf1.clauseIndices)
    println("doing ResolveWithSingleton on 1 with empty substitution")
    cnf1 = cnf1.resolveWithSingleton(1, Literal("p(b)"), newSubstitution)
    println(cnf1)
    println("doing ResolveWithSingleton with nonexistent literal")
    try {
      cnf1 = cnf1.resolveWithSingleton(1, Literal("d"), newSubstitution)
    } catch {
      case e:Exception => println("caught an exception")
    }
    println

    val cnfList2 = List(List("p(a)", "p(X)"))
    var cnf2 = new CNF(cnfList2)
    println(cnf2)
    println("applying substitution: X->a")
    cnf2 = cnf2.applySubstitution(newSubstitutionInit("X"->"a"))
    println(cnf2)
    println

    println(cnf2.toListList)
    println

    val cnfList3 = List(List("p(f(X))", "-q(Y)"), List("p(a, a)", "-q(Z)"))
    var cnf3 = new CNF(cnfList3)
    println(cnf3)
    println("applying substitution: X->Z, Y->c")
    cnf3 = cnf3.applySubstitution(newSubstitutionInit("X" -> "Z", "Y"->"c"))
    println(cnf3)
    println

    // uniqueUnificationMatch
    println("uniqueUnificationMatch for q(X): " + cnf3.uniqueUnificationMatch(Literal("q(X)")))
    println("uniqueUnificationMatch for q(a): " + cnf3.uniqueUnificationMatch(Literal("q(a)")))

    println(cnf3.toListList)

    println
    println("grounded negation match for q(X) in cnf3: " + cnf3.groundedNegationMatch(Literal("q(X)")))
    println("grounded negation match for p(a, a) in cnf3: " + cnf3.groundedNegationMatch(Literal("p(a,a)")))
    println("grounded negation match for -p(a,a) in cnf3: " + cnf3.groundedNegationMatch(Literal("-p( a,   a)")))
  }
}

class TestInferenceRule extends TestMe { 
  def testme {
    println("inference rule test")
    val cnf1 = new CNF(List(List("p(a)"), List("q(b)"), List("-p(a)"), List("r(b, b)")))
    val cnf2 = new CNF(List(List("q(a)"), List("-o(b)"), List("p(X)"), List("p(Y)"), List("r(X)")))

    testPair(cnf1, cnf2)

    val cnf3 = new CNF(List(List("a(X)"), List("d(Y)"), List("c(X, X)")))
    val cnf4 = new CNF(List(List("b(Y, X)"), List("-a(X, Y)")))

    testPair(cnf3, cnf4)
  }

  def testPair(cnf1:CNF, cnf2:CNF) :Unit = {
    val ir = new InferenceRule(cnf1, cnf2)

    println("CNF1: " + cnf1)
    println("CNF2 : " + cnf2)
    println
    println("lhsOld " + ir.lhsOld)
    println("rhsOld " + ir.rhsOld)

    println
    println("lhs argument map: " + ir.lhsArgumentMap)
    println("rhs argument map: " + ir.rhsArgumentMap)
    println("combined map: " + ir.combinedMap)
    println

    println("quantifiers: " + ir.quantifiers)
    println("lhs: " + ir.lhs)
    println("rhs: " + ir.rhs)
    println
    println(ir)
  }
}

class ResolutionTest extends TestMe {
  import Resolution._
  def testme { 
    println
    println("resolution test")
    println
    testThisPair(List(List("solve(a)"), List("event(a)"), List("agent(a, b)"), List("person(b)"), 
		      List("patient(a, c)"), List("problem(c)")),
		 List(List("-find(E)", "-event(E)", "-agent(E, X)", "-person(X)", 
			   "-patient(E, Y)", "-solution(Y)", "-to(Y, Z)", "-problem(Z)")))
    println

    testThisPair(List(List("p(a)")), List(List("-p(a)")))
    println

testSentpair("""    In Beijing, officials said Thursday that visit would take place on May 26  to 29. In Moscow the premier's office confirmed that Chernomyrdin would visit  at the end of the month but did not give precise dates.""", """    In Beijing, officials said Thursday that visit would take be on May 26  to 29. In Moscow the premier's office confirmed that Chernomyrdin would visit  at the end of the month but did not give precise dates.""", """place on@R@->be on@R@""", 
List(List("tim1moscow(a1)"), List("n1office(c1)"), List("r1of(c1, b1)"), List("n1premierC39s(b1)"), List("tim1moscow(d1)"), List("n1month(e1)"), List("n1end(f1)"), List("per1chernomyrdin(g1)"), List("n1office(i1)"), List("r1of(i1, h1)"), List("n1premierC39s(h1)"), List("r1in(n1, m1)"), List("r1on(k1, j1)"), List("n1thursday(j1)"), List("r1theme(k1, l1)"), List("r1agent(k1, m1)"), List("v1say(k1)"), List("r1in(r1, d1)"), List("r1to(r1, p1)"), List("n129C46(p1)"), List("r1on(r1, q1)"), List("t_XXXX0526(q1)"), List("r1patient(r1, s1)"), List("r1agent(r1, t1)"), List("v1take(r1)"), List("n1place(s1)"), List("n1visit(t1)"), List("n1official(m1)"), List("org1beijingC44(m1)"), List("r1theme(n1, o1)"), List("r1agent(n1, i1)"), List("v1confirm(n1)"), List("r1at(u1, f1)"), List("r1of(f1, e1)"), List("r1agent(u1, g1)"), List("v1visit(u1)")), List(List("-org1beijingC44(E2)", "-tim1moscow(F2)", "-per1chernomyrdin(A2)", "-n1month(B2)", "-n1end(C2)", "-per1chernomyrdin(D2)", "-n1office(H2)", "-r1of(H2, G2)", "-n1premierC39s(G2)", "-r1in(J2, E2)", "-r1in(J2, F2)", "-r1to(J2, I2)", "-n129C46(I2)", "-r1on(N2, Q2)", "-t_XXXX0526(Q2)", "-r1on(L2, K2)", "-n1thursday(K2)", "-r1theme(L2, M2)", "-r1agent(L2, N2)", "-v1say(L2)", "-r1agent(R2, S2)", "-v1take(R2)", "-n1visit(S2)", "-n1official(N2)", "-r1theme(O2, P2)", "-r1agent(O2, H2)", "-v1confirm(O2)", "-r1at(T2, C2)", "-r1of(C2, B2)", "-r1agent(T2, D2)", "-v1visit(T2)")))

testSentpair("""    It was Feyenoord's third win in four years and ninth all-told.""", """    It was Feyenoord's third fight in four years and ninth all-told.""", """win in->fight in""", 
List(List("a1neuter(a1)"), List("eq(a1, f1)"), List("r1subset_of(c1, f1)"), List("n1allC45toldC46(c1)"), List("a1ninth(c1)"), List("r1subset_of(e1, f1)"), List("r1in(e1, d1)"), List("n1year(d1)"), List("card(d1, g1)"), List("c4number(g1)"), List("n1numeral(g1)"), List("n1win(e1)"), List("a1third(e1)"), List("nam1feyenoordC39s(e1)")), List(List("-a1neuter(A2)", "-eq(A2, F2)", "-r1subset_of(C2, F2)", "-n1allC45toldC46(C2)", "-a1ninth(C2)", "-r1subset_of(E2, F2)", "-r1in(E2, D2)", "-n1year(D2)", "-card(D2, G2)", "-c4number(G2)", "-n1numeral(G2)", "-n1fight(E2)", "-a1third(E2)", "-nam1feyenoordC39s(E2)")))

testSentpair("""    It was Feyenoord's third win in four years and ninth all-told.""", """    It was Feyenoord's third fight in four years and ninth all-told.""", """win in@R@->fight in@R@""", 
List(List("a1neuter(a1)"), List("eq(a1, f1)"), List("r1subset_of(c1, f1)"), List("n1allC45toldC46(c1)"), List("a1ninth(c1)"), List("r1subset_of(e1, f1)"), List("r1in(e1, d1)"), List("n1year(d1)"), List("card(d1, g1)"), List("c4number(g1)"), List("n1numeral(g1)"), List("n1win(e1)"), List("a1third(e1)"), List("nam1feyenoordC39s(e1)")), List(List("-a1neuter(A2)", "-eq(A2, F2)", "-r1subset_of(C2, F2)", "-n1allC45toldC46(C2)", "-a1ninth(C2)", "-r1subset_of(E2, F2)", "-r1in(E2, D2)", "-n1year(D2)", "-card(D2, G2)", "-c4number(G2)", "-n1numeral(G2)", "-n1fight(E2)", "-a1third(E2)", "-nam1feyenoordC39s(E2)")))

testSentpair("""    The 73-year-old Indian leader, who embarks on the visit Saturday, is under  tremendous domestic pressure to stand up to persistent US demands to cap the  country's nuclear regime and freeze its missile defence programme.""", """    The 73-year-old Indian leader, who embarks on the visit Saturday, is under  tremendous domestic pressure to stand up to persistent US demands to cap the  country's nuclear regime and wake its missile defence programme.""", """freeze->wake""", 
List(List("n1programmeC46(c1)"), List("r1of(c1, a1)"), List("n1defence(a1)"), List("r1of(c1, b1)"), List("n1missile(b1)"), List("r1of(c1, d1)"), List("a1neuter(d1)"), List("n1regime(c1)"), List("a1nuclear(c1)"), List("n1countryC39s(d1)"), List("nam1saturdayC44(e1)"), List("n1visit(f1)"), List("n1leaderC44(g1)"), List("a1indian(g1)"), List("a173C45yearC45old(g1)"), List("a1topic(g1)"), List("r1rel(h1, f1)"), List("r1under(e1, k1)"), List("n1pressure(k1)"), List("r1patient(o1, c1)"), List("r1agent(o1, k1)"), List("v1freeze(o1)"), List("r1patient(p1, c1)"), List("r1agent(p1, k1)"), List("v1cap(p1)"), List("r1to(n1, m1)"), List("n1demand(m1)"), List("loc1us(m1)"), List("a1persistent(m1)"), List("a1up(n1)"), List("r1agent(n1, k1)"), List("v1stand(n1)"), List("a1domestic(k1)"), List("a1tremendous(k1)"), List("r1on(i1, f1)"), List("r1agent(i1, g1)"), List("v1embark(i1)")), List(List("-n1wake(A2)", "-n1regime(B2)", "-r1subset_of(A2, D2)", "-r1subset_of(B2, D2)", "-a1nuclear(D2)", "-r1of(D2, C2)", "-n1countryC39s(C2)", "-n1programmeC46(D2)", "-r1of(D2, E2)", "-n1defence(E2)", "-r1of(D2, F2)", "-n1missile(F2)", "-a1neuter(C2)", "-nam1saturdayC44(G2)", "-n1visit(C2)", "-n1leaderC44(H2)", "-a1indian(H2)", "-a173C45yearC45old(H2)", "-a1topic(H2)", "-r1rel(K2, C2)", "-r1under(K2, J2)", "-n1pressure(J2)", "-r1patient(P2, D2)", "-r1agent(P2, J2)", "-v1cap(P2)", "-r1to(O2, N2)", "-n1demand(N2)", "-loc1us(N2)", "-a1persistent(N2)", "-a1up(O2)", "-r1agent(O2, J2)", "-v1stand(O2)", "-a1domestic(J2)", "-a1tremendous(J2)", "-eq(G2, D2)", "-r1on(L2, C2)", "-r1agent(L2, H2)", "-v1embark(L2)")))

testSentpair("""    The 73-year-old Indian leader, who embarks on the visit Saturday, is under  tremendous domestic pressure to stand up to persistent US demands to cap the  country's nuclear regime and freeze its missile defence programme.""", """    The 73-year-old Indian leader, who embarks on the visit Saturday, is under  tremendous domestic pressure to stand up to persistent US demands to cap the  country's nuclear regime and wake its missile defence programme.""", """freeze@R@->wake@R@""", 
List(List("n1programmeC46(c1)"), List("r1of(c1, a1)"), List("n1defence(a1)"), List("r1of(c1, b1)"), List("n1missile(b1)"), List("r1of(c1, d1)"), List("a1neuter(d1)"), List("n1regime(c1)"), List("a1nuclear(c1)"), List("n1countryC39s(d1)"), List("nam1saturdayC44(e1)"), List("n1visit(f1)"), List("n1leaderC44(g1)"), List("a1indian(g1)"), List("a173C45yearC45old(g1)"), List("a1topic(g1)"), List("r1rel(h1, f1)"), List("r1under(e1, k1)"), List("n1pressure(k1)"), List("r1patient(o1, c1)"), List("r1agent(o1, k1)"), List("v1freeze(o1)"), List("r1patient(p1, c1)"), List("r1agent(p1, k1)"), List("v1cap(p1)"), List("r1to(n1, m1)"), List("n1demand(m1)"), List("loc1us(m1)"), List("a1persistent(m1)"), List("a1up(n1)"), List("r1agent(n1, k1)"), List("v1stand(n1)"), List("a1domestic(k1)"), List("a1tremendous(k1)"), List("r1on(i1, f1)"), List("r1agent(i1, g1)"), List("v1embark(i1)")), List(List("-n1wake(A2)", "-n1regime(B2)", "-r1subset_of(A2, D2)", "-r1subset_of(B2, D2)", "-a1nuclear(D2)", "-r1of(D2, C2)", "-n1countryC39s(C2)", "-n1programmeC46(D2)", "-r1of(D2, E2)", "-n1defence(E2)", "-r1of(D2, F2)", "-n1missile(F2)", "-a1neuter(C2)", "-nam1saturdayC44(G2)", "-n1visit(C2)", "-n1leaderC44(H2)", "-a1indian(H2)", "-a173C45yearC45old(H2)", "-a1topic(H2)", "-r1rel(K2, C2)", "-r1under(K2, J2)", "-n1pressure(J2)", "-r1patient(P2, D2)", "-r1agent(P2, J2)", "-v1cap(P2)", "-r1to(O2, N2)", "-n1demand(N2)", "-loc1us(N2)", "-a1persistent(N2)", "-a1up(O2)", "-r1agent(O2, J2)", "-v1stand(O2)", "-a1domestic(J2)", "-a1tremendous(J2)", "-eq(G2, D2)", "-r1on(L2, C2)", "-r1agent(L2, H2)", "-v1embark(L2)")))

testSentpair("""Russian ultranationalist Vladimir Zhirinovsky said  Thursday he had approached Prime Minister Viktor Chernomyrdin with a view to  his party being given places in the government, Interfax said.""", """Russian ultranationalist Vladimir Zhirinovsky said  Thursday he had approached Prime Minister Viktor Chernomyrdin with a span to  his party being given places in the government, Interfax said.""", """view to->span to""", 
List(List("n1saidC46(c1)"), List("r1of(c1, a1)"), List("n1interfax(a1)"), List("r1of(c1, b1)"), List("n1governmentC44(b1)"), List("n1party(c1)"), List("per1prime_minister_viktor_chern(d1)"), List("per1vladimir_zhirinovsky(c1)"), List("n1ultranationalist(b1)"), List("a1russian(c1)"), List("r1on(f1, b1)"), List("n1thursday(b1)"), List("r1with(j1, i1)"), List("r1to(i1, c1)"), List("r1theme(g1, h1)"), List("r1recipient(g1, c1)"), List("v1give(g1)"), List("r1in(h1, c1)"), List("n1place(h1)"), List("n1view(i1)"), List("r1patient(j1, d1)"), List("r1agent(j1, b1)"), List("v1approach(j1)"), List("a1male(b1)"), List("r1theme(f1, e1)"), List("r1agent(f1, c1)"), List("v1say(f1)")), List(List("-n1saidC46(C2)", "-r1of(C2, A2)", "-n1interfax(A2)", "-r1of(C2, B2)", "-n1governmentC44(B2)", "-n1party(C2)", "-per1prime_minister_viktor_chern(D2)", "-per1vladimir_zhirinovsky(C2)", "-n1ultranationalist(B2)", "-a1russian(C2)", "-r1on(F2, B2)", "-n1thursday(B2)", "-r1with(J2, I2)", "-r1to(I2, C2)", "-r1theme(G2, H2)", "-r1recipient(G2, C2)", "-v1give(G2)", "-r1in(H2, C2)", "-n1place(H2)", "-n1span(I2)", "-r1patient(J2, D2)", "-r1agent(J2, B2)", "-v1approach(J2)", "-a1male(B2)", "-r1theme(F2, E2)", "-r1agent(F2, C2)", "-v1say(F2)")))

testSentpair("""Russian ultranationalist Vladimir Zhirinovsky said  Thursday he had approached Prime Minister Viktor Chernomyrdin with a view to  his party being given places in the government, Interfax said.""", """Russian ultranationalist Vladimir Zhirinovsky said  Thursday he had approached Prime Minister Viktor Chernomyrdin with a span to  his party being given places in the government, Interfax said.""", """view to@R@->span to@R@""", 
List(List("n1saidC46(c1)"), List("r1of(c1, a1)"), List("n1interfax(a1)"), List("r1of(c1, b1)"), List("n1governmentC44(b1)"), List("n1party(c1)"), List("per1prime_minister_viktor_chern(d1)"), List("per1vladimir_zhirinovsky(c1)"), List("n1ultranationalist(b1)"), List("a1russian(c1)"), List("r1on(f1, b1)"), List("n1thursday(b1)"), List("r1with(j1, i1)"), List("r1to(i1, c1)"), List("r1theme(g1, h1)"), List("r1recipient(g1, c1)"), List("v1give(g1)"), List("r1in(h1, c1)"), List("n1place(h1)"), List("n1view(i1)"), List("r1patient(j1, d1)"), List("r1agent(j1, b1)"), List("v1approach(j1)"), List("a1male(b1)"), List("r1theme(f1, e1)"), List("r1agent(f1, c1)"), List("v1say(f1)")), List(List("-n1saidC46(C2)", "-r1of(C2, A2)", "-n1interfax(A2)", "-r1of(C2, B2)", "-n1governmentC44(B2)", "-n1party(C2)", "-per1prime_minister_viktor_chern(D2)", "-per1vladimir_zhirinovsky(C2)", "-n1ultranationalist(B2)", "-a1russian(C2)", "-r1on(F2, B2)", "-n1thursday(B2)", "-r1with(J2, I2)", "-r1to(I2, C2)", "-r1theme(G2, H2)", "-r1recipient(G2, C2)", "-v1give(G2)", "-r1in(H2, C2)", "-n1place(H2)", "-n1span(I2)", "-r1patient(J2, D2)", "-r1agent(J2, B2)", "-v1approach(J2)", "-a1male(B2)", "-r1theme(F2, E2)", "-r1agent(F2, C2)", "-v1say(F2)")))

testSentpair("""    Juppe said France had let it be known it would not accept this "parody of  justice" and would not bargain for their freedom.""", """    Juppe said France had let it be known it would not accept this "impersonate of  justice" and would not bargain for their freedom.""", """parody->impersonate""", 
List(List("n1freedomC46(a1)"), List("r1of(a1, b1)"), List("n12thing(b1)"), List("loc1france(c1)"), List("n1C34parody(d1)"), List("a1neuter(e1)"), List("loc1france(f1)"), List("per1juppe(g1)"), List("-r1patient(N1, d1)", "-r1agent(N1, e1)", "-v1accept(N1)", "-r1of(d1, O1)", "-n1justiceC34(O1)"), List("r1theme(m1, l1)"), List("r1agent(m1, e1)"), List("v1know(m1)"), List("r1theme(k1, j1)"), List("r1agent(k1, f1)"), List("r1patient(k1, e1)"), List("v1let(k1)"), List("r1theme(i1, h1)"), List("r1agent(i1, g1)"), List("v1say(i1)")), List(List("r1patient(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2), D2)", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34impersonate(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("r1agent(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2), E2)", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34impersonate(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("v1accept(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34impersonate(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("r1of(D2, o2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34impersonate(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("n1justiceC34(o2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34impersonate(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)")))

testSentpair("""    Juppe said France had let it be known it would not accept this "parody of  justice" and would not bargain for their freedom.""", """    Juppe said France had let it be known it would not accept this "impersonate of  justice" and would not bargain for their freedom.""", """parody@R@->impersonate@R@""", 
List(List("n1freedomC46(a1)"), List("r1of(a1, b1)"), List("n12thing(b1)"), List("loc1france(c1)"), List("n1C34parody(d1)"), List("a1neuter(e1)"), List("loc1france(f1)"), List("per1juppe(g1)"), List("-r1patient(N1, d1)", "-r1agent(N1, e1)", "-v1accept(N1)", "-r1of(d1, O1)", "-n1justiceC34(O1)"), List("r1theme(m1, l1)"), List("r1agent(m1, e1)"), List("v1know(m1)"), List("r1theme(k1, j1)"), List("r1agent(k1, f1)"), List("r1patient(k1, e1)"), List("v1let(k1)"), List("r1theme(i1, h1)"), List("r1agent(i1, g1)"), List("v1say(i1)")), List(List("r1patient(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2), D2)", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34impersonate(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("r1agent(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2), E2)", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34impersonate(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("v1accept(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34impersonate(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("r1of(D2, o2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34impersonate(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("n1justiceC34(o2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34impersonate(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)")))

testSentpair("""    The event reopens debate on the issue. National talk show host Phil  Donohue has joined Lawson in his request to the North Carolina Supreme Court  to overturn the ban.""", """    The event reopens be divided on the issue. National talk show host Phil  Donohue has joined Lawson in his request to the North Carolina Supreme Court  to overturn the ban.""", """debate on->be divided on""", 
List(List("n1banC46(a1)"), List("org1north_carolina_supreme_cour(b1)"), List("n1request(c1)"), List("r1of(c1, d1)"), List("a1male(d1)"), List("per1lawson(d1)"), List("per1phil_donohue(c1)"), List("r1of(c1, e1)"), List("n1host(e1)"), List("r1of(c1, f1)"), List("n1show(f1)"), List("n1talk(d1)"), List("org1national(c1)"), List("org1issueC46(c1)"), List("n1event(g1)"), List("r1patient(l1, a1)"), List("r1agent(l1, g1)"), List("v1overturn(l1)"), List("r1in(i1, c1)"), List("r1to(c1, b1)"), List("r1patient(i1, d1)"), List("r1agent(i1, g1)"), List("v1join(i1)"), List("r1patient(j1, k1)"), List("r1agent(j1, g1)"), List("v1reopen(j1)"), List("r1on(k1, c1)"), List("n1debate(k1)")), List(List("-n1banC46(A2)", "-org1north_carolina_supreme_cour(B2)", "-n1request(C2)", "-r1of(C2, D2)", "-a1male(D2)", "-per1lawson(D2)", "-per1phil_donohue(C2)", "-r1of(C2, E2)", "-n1host(E2)", "-r1of(C2, F2)", "-n1show(F2)", "-n1talk(D2)", "-org1national(C2)", "-org1issueC46(C2)", "-n1event(G2)", "-r1patient(K2, A2)", "-r1agent(K2, G2)", "-v1overturn(K2)", "-r1in(I2, C2)", "-r1to(C2, B2)", "-r1patient(I2, D2)", "-r1agent(I2, G2)", "-v1join(I2)", "-r1on(J2, C2)", "-r1patient(J2, G2)", "-v1divide(J2)")))

testSentpair("""    The event reopens debate on the issue. National talk show host Phil  Donohue has joined Lawson in his request to the North Carolina Supreme Court  to overturn the ban.""", """    The event reopens be divided on the issue. National talk show host Phil  Donohue has joined Lawson in his request to the North Carolina Supreme Court  to overturn the ban.""", """debate on@R@->be divided on@R@""", 
List(List("n1banC46(a1)"), List("org1north_carolina_supreme_cour(b1)"), List("n1request(c1)"), List("r1of(c1, d1)"), List("a1male(d1)"), List("per1lawson(d1)"), List("per1phil_donohue(c1)"), List("r1of(c1, e1)"), List("n1host(e1)"), List("r1of(c1, f1)"), List("n1show(f1)"), List("n1talk(d1)"), List("org1national(c1)"), List("org1issueC46(c1)"), List("n1event(g1)"), List("r1patient(l1, a1)"), List("r1agent(l1, g1)"), List("v1overturn(l1)"), List("r1in(i1, c1)"), List("r1to(c1, b1)"), List("r1patient(i1, d1)"), List("r1agent(i1, g1)"), List("v1join(i1)"), List("r1patient(j1, k1)"), List("r1agent(j1, g1)"), List("v1reopen(j1)"), List("r1on(k1, c1)"), List("n1debate(k1)")), List(List("-n1banC46(A2)", "-org1north_carolina_supreme_cour(B2)", "-n1request(C2)", "-r1of(C2, D2)", "-a1male(D2)", "-per1lawson(D2)", "-per1phil_donohue(C2)", "-r1of(C2, E2)", "-n1host(E2)", "-r1of(C2, F2)", "-n1show(F2)", "-n1talk(D2)", "-org1national(C2)", "-org1issueC46(C2)", "-n1event(G2)", "-r1patient(K2, A2)", "-r1agent(K2, G2)", "-v1overturn(K2)", "-r1in(I2, C2)", "-r1to(C2, B2)", "-r1patient(I2, D2)", "-r1agent(I2, G2)", "-v1join(I2)", "-r1on(J2, C2)", "-r1patient(J2, G2)", "-v1divide(J2)")))

testSentpair("""A 26-year-old anorexic's fight for  survival hung in the balance Thursday as the British woman awaited the outcome  of a media bidding battle for her exclusive story.""", """A 26-year-old anorexic's fight for  survival hung in the balance Thursday as the British woman awaited the outcome  of a media bidding battle for her live chat story.""", """exclusive->live chat""", 
List(List("n1storyC46(a1)"), List("a1exclusive(a1)"), List("r1of(a1, b1)"), List("a1female(b1)"), List("n1outcome(b1)"), List("n1woman(c1)"), List("a1british(c1)"), List("tim1thursday(a1)"), List("n1balance(b1)"), List("r1patient(d1, b1)"), List("r1agent(d1, k1)"), List("v1await(d1)"), List("r1of(b1, g1)"), List("r1for(g1, a1)"), List("n1battle(g1)"), List("r1of(g1, e1)"), List("n1bidding(e1)"), List("r1of(g1, f1)"), List("n1media(f1)"), List("r1as(k1, c1)"), List("r1in(k1, a1)"), List("r1for(k1, i1)"), List("n1hung(i1)"), List("r1of(i1, h1)"), List("n1survival(h1)"), List("n1fight(k1)"), List("r1of(k1, j1)"), List("n1anorexicC39s(j1)"), List("a126C45yearC45old(k1)")), List(List("-n1storyC46(B2)", "-r1of(B2, A2)", "-n1chat(A2)", "-a1live(B2)", "-r1of(B2, C2)", "-a1female(C2)", "-n1outcome(C2)", "-n1woman(D2)", "-a1british(D2)", "-tim1thursday(B2)", "-n1balance(C2)", "-r1patient(E2, C2)", "-r1agent(E2, L2)", "-v1await(E2)", "-r1of(C2, H2)", "-r1for(H2, B2)", "-n1battle(H2)", "-r1of(H2, F2)", "-n1bidding(F2)", "-r1of(H2, G2)", "-n1media(G2)", "-r1as(L2, D2)", "-r1in(L2, B2)", "-r1for(L2, J2)", "-n1hung(J2)", "-r1of(J2, I2)", "-n1survival(I2)", "-n1fight(L2)", "-r1of(L2, K2)", "-n1anorexicC39s(K2)", "-a126C45yearC45old(L2)")))

testSentpair("""    The woman, whose twin sister has already dieted herself to death, was  relying on the outcome of a battle between such British media giants as  Granada Television and the Daily Mirror newspaper to pay her medical bills at  an exclusive clinic which specializes in bringing victims of anorexia and  bulimia back to the world of the living.""", """    The woman, whose twin sister has already dieted herself to death, was  relying on the outcome of a battle between such British media giants as  Granada Television and the Daily Mirror newspaper to pay her medical bills at  an live chat clinic which specializes in bringing victims of anorexia and  bulimia back to the world of the living.""", """exclusive->live chat""", 
List(List("n1livingC46(a1)"), List("n1world(b1)"), List("n1newspaper(c1)"), List("org1mirror(c1)"), List("org1daily(c1)"), List("org1granada_television(d1)"), List("n1outcome(e1)"), List("a1female(f1)"), List("n1womanC44(f1)"), List("r1on(l1, e1)"), List("r1of(e1, k1)"), List("r1patient(o1, g1)"), List("r1agent(o1, k1)"), List("v1pay(o1)"), List("r1at(g1, w1)"), List("a1back(p1)"), List("r1to(p1, b1)"), List("r1of(b1, a1)"), List("r1patient(p1, t1)"), List("r1agent(p1, u1)"), List("v1bring(p1)"), List("r1of(t1, s1)"), List("n1bulimium(q1)"), List("n1anorexia(r1)"), List("r1subset_of(q1, s1)"), List("r1subset_of(r1, s1)"), List("n1victim(t1)"), List("r1in(v1, u1)"), List("r1agent(v1, w1)"), List("v1specialize(v1)"), List("n1clinic(w1)"), List("a1exclusive(w1)"), List("n1bill(g1)"), List("a1medical(g1)"), List("r1of(g1, h1)"), List("a1female(h1)"), List("r1between(k1, j1)"), List("a1such(j1)"), List("r1subset_of(c1, j1)"), List("r1subset_of(g1, j1)"), List("r1as(g1, d1)"), List("n1giant(g1)"), List("n1media(h1)"), List("a1british(g1)"), List("n1battle(k1)"), List("r1agent(l1, f1)"), List("v1rely(l1)"), List("a1already(m1)"), List("r1patient(m1, n1)"), List("r1agent(m1, h1)"), List("v1diet(m1)"), List("r1agent(x1, f1)"), List("v1deathC44(x1)"), List("n1sister(h1)"), List("a1twin(h1)"), List("r1of(h1, f1)")), List(List("-n1livingC46(A2)", "-n1world(B2)", "-n1newspaper(C2)", "-org1mirror(C2)", "-org1daily(C2)", "-org1granada_television(D2)", "-n1outcome(E2)", "-a1female(F2)", "-n1womanC44(F2)", "-r1on(L2, E2)", "-r1of(E2, K2)", "-r1patient(O2, G2)", "-r1agent(O2, K2)", "-v1pay(O2)", "-r1at(G2, X2)", "-a1back(P2)", "-r1to(P2, B2)", "-r1of(B2, A2)", "-r1patient(P2, T2)", "-r1agent(P2, U2)", "-v1bring(P2)", "-r1of(T2, S2)", "-n1bulimium(Q2)", "-n1anorexia(R2)", "-r1subset_of(Q2, S2)", "-r1subset_of(R2, S2)", "-n1victim(T2)", "-r1in(V2, U2)", "-r1agent(V2, X2)", "-v1specialize(V2)", "-n1clinic(X2)", "-r1of(X2, W2)", "-n1chat(W2)", "-a1live(X2)", "-n1bill(G2)", "-a1medical(G2)", "-r1of(G2, H2)", "-a1female(H2)", "-r1between(K2, J2)", "-a1such(J2)", "-r1subset_of(C2, J2)", "-r1subset_of(G2, J2)", "-r1as(G2, D2)", "-n1giant(G2)", "-n1media(H2)", "-a1british(G2)", "-n1battle(K2)", "-r1agent(L2, F2)", "-v1rely(L2)", "-a1already(M2)", "-r1patient(M2, N2)", "-r1agent(M2, H2)", "-v1diet(M2)", "-r1agent(Y2, F2)", "-v1deathC44(Y2)", "-n1sister(H2)", "-a1twin(H2)", "-r1of(H2, F2)")))

testSentpair("""    The woman, whose twin sister has already dieted herself to death, was  relying on the outcome of a battle between such British media giants as  Granada Television and the Daily Mirror newspaper to pay her medical bills at  an exclusive clinic which specializes in bringing victims of anorexia and  bulimia back to the world of the living.""", """    The woman, whose twin sister has already dieted herself to death, was  relying on the outcome of a battle between such British media giants as  Granada Television and the Daily Mirror newspaper to pay her medical bills at  an live chat clinic which specializes in bringing victims of anorexia and  bulimia back to the world of the living.""", """exclusive@R@->live chat@R@""", 
List(List("n1livingC46(a1)"), List("n1world(b1)"), List("n1newspaper(c1)"), List("org1mirror(c1)"), List("org1daily(c1)"), List("org1granada_television(d1)"), List("n1outcome(e1)"), List("a1female(f1)"), List("n1womanC44(f1)"), List("r1on(l1, e1)"), List("r1of(e1, k1)"), List("r1patient(o1, g1)"), List("r1agent(o1, k1)"), List("v1pay(o1)"), List("r1at(g1, w1)"), List("a1back(p1)"), List("r1to(p1, b1)"), List("r1of(b1, a1)"), List("r1patient(p1, t1)"), List("r1agent(p1, u1)"), List("v1bring(p1)"), List("r1of(t1, s1)"), List("n1bulimium(q1)"), List("n1anorexia(r1)"), List("r1subset_of(q1, s1)"), List("r1subset_of(r1, s1)"), List("n1victim(t1)"), List("r1in(v1, u1)"), List("r1agent(v1, w1)"), List("v1specialize(v1)"), List("n1clinic(w1)"), List("a1exclusive(w1)"), List("n1bill(g1)"), List("a1medical(g1)"), List("r1of(g1, h1)"), List("a1female(h1)"), List("r1between(k1, j1)"), List("a1such(j1)"), List("r1subset_of(c1, j1)"), List("r1subset_of(g1, j1)"), List("r1as(g1, d1)"), List("n1giant(g1)"), List("n1media(h1)"), List("a1british(g1)"), List("n1battle(k1)"), List("r1agent(l1, f1)"), List("v1rely(l1)"), List("a1already(m1)"), List("r1patient(m1, n1)"), List("r1agent(m1, h1)"), List("v1diet(m1)"), List("r1agent(x1, f1)"), List("v1deathC44(x1)"), List("n1sister(h1)"), List("a1twin(h1)"), List("r1of(h1, f1)")), List(List("-n1livingC46(A2)", "-n1world(B2)", "-n1newspaper(C2)", "-org1mirror(C2)", "-org1daily(C2)", "-org1granada_television(D2)", "-n1outcome(E2)", "-a1female(F2)", "-n1womanC44(F2)", "-r1on(L2, E2)", "-r1of(E2, K2)", "-r1patient(O2, G2)", "-r1agent(O2, K2)", "-v1pay(O2)", "-r1at(G2, X2)", "-a1back(P2)", "-r1to(P2, B2)", "-r1of(B2, A2)", "-r1patient(P2, T2)", "-r1agent(P2, U2)", "-v1bring(P2)", "-r1of(T2, S2)", "-n1bulimium(Q2)", "-n1anorexia(R2)", "-r1subset_of(Q2, S2)", "-r1subset_of(R2, S2)", "-n1victim(T2)", "-r1in(V2, U2)", "-r1agent(V2, X2)", "-v1specialize(V2)", "-n1clinic(X2)", "-r1of(X2, W2)", "-n1chat(W2)", "-a1live(X2)", "-n1bill(G2)", "-a1medical(G2)", "-r1of(G2, H2)", "-a1female(H2)", "-r1between(K2, J2)", "-a1such(J2)", "-r1subset_of(C2, J2)", "-r1subset_of(G2, J2)", "-r1as(G2, D2)", "-n1giant(G2)", "-n1media(H2)", "-a1british(G2)", "-n1battle(K2)", "-r1agent(L2, F2)", "-v1rely(L2)", "-a1already(M2)", "-r1patient(M2, N2)", "-r1agent(M2, H2)", "-v1diet(M2)", "-r1agent(Y2, F2)", "-v1deathC44(Y2)", "-n1sister(H2)", "-a1twin(H2)", "-r1of(H2, F2)")))

testSentpair("""A 26-year-old anorexic's fight for  survival hung in the balance Thursday as the British woman awaited the outcome  of a media bidding battle for her exclusive story.""", """A 26-year-old anorexic's fight for  survival hung in the balance Thursday as the British woman awaited the outcome  of a media bidding battle for her live chat story.""", """exclusive@R@->live chat@R@""", 
List(List("n1storyC46(a1)"), List("a1exclusive(a1)"), List("r1of(a1, b1)"), List("a1female(b1)"), List("n1outcome(b1)"), List("n1woman(c1)"), List("a1british(c1)"), List("tim1thursday(a1)"), List("n1balance(b1)"), List("r1patient(d1, b1)"), List("r1agent(d1, k1)"), List("v1await(d1)"), List("r1of(b1, g1)"), List("r1for(g1, a1)"), List("n1battle(g1)"), List("r1of(g1, e1)"), List("n1bidding(e1)"), List("r1of(g1, f1)"), List("n1media(f1)"), List("r1as(k1, c1)"), List("r1in(k1, a1)"), List("r1for(k1, i1)"), List("n1hung(i1)"), List("r1of(i1, h1)"), List("n1survival(h1)"), List("n1fight(k1)"), List("r1of(k1, j1)"), List("n1anorexicC39s(j1)"), List("a126C45yearC45old(k1)")), List(List("-n1storyC46(B2)", "-r1of(B2, A2)", "-n1chat(A2)", "-a1live(B2)", "-r1of(B2, C2)", "-a1female(C2)", "-n1outcome(C2)", "-n1woman(D2)", "-a1british(D2)", "-tim1thursday(B2)", "-n1balance(C2)", "-r1patient(E2, C2)", "-r1agent(E2, L2)", "-v1await(E2)", "-r1of(C2, H2)", "-r1for(H2, B2)", "-n1battle(H2)", "-r1of(H2, F2)", "-n1bidding(F2)", "-r1of(H2, G2)", "-n1media(G2)", "-r1as(L2, D2)", "-r1in(L2, B2)", "-r1for(L2, J2)", "-n1hung(J2)", "-r1of(J2, I2)", "-n1survival(I2)", "-n1fight(L2)", "-r1of(L2, K2)", "-n1anorexicC39s(K2)", "-a126C45yearC45old(L2)")))

testSentpair("""    Juppe said France had let it be known it would not accept this "parody of  justice" and would not bargain for their freedom.""", """    Juppe said France had let it be known it would not accept this "satirize of  justice" and would not bargain for their freedom.""", """parody->satirize""", 
List(List("n1freedomC46(a1)"), List("r1of(a1, b1)"), List("n12thing(b1)"), List("loc1france(c1)"), List("n1C34parody(d1)"), List("a1neuter(e1)"), List("loc1france(f1)"), List("per1juppe(g1)"), List("-r1patient(N1, d1)", "-r1agent(N1, e1)", "-v1accept(N1)", "-r1of(d1, O1)", "-n1justiceC34(O1)"), List("r1theme(m1, l1)"), List("r1agent(m1, e1)"), List("v1know(m1)"), List("r1theme(k1, j1)"), List("r1agent(k1, f1)"), List("r1patient(k1, e1)"), List("v1let(k1)"), List("r1theme(i1, h1)"), List("r1agent(i1, g1)"), List("v1say(i1)")), List(List("r1patient(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2), D2)", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34satirize(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("r1agent(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2), E2)", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34satirize(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("v1accept(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34satirize(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("r1of(D2, o2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34satirize(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("n1justiceC34(o2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34satirize(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)")))

testSentpair("""    Juppe said France had let it be known it would not accept this "parody of  justice" and would not bargain for their freedom.""", """    Juppe said France had let it be known it would not accept this "satirize of  justice" and would not bargain for their freedom.""", """parody@R@->satirize@R@""", 
List(List("n1freedomC46(a1)"), List("r1of(a1, b1)"), List("n12thing(b1)"), List("loc1france(c1)"), List("n1C34parody(d1)"), List("a1neuter(e1)"), List("loc1france(f1)"), List("per1juppe(g1)"), List("-r1patient(N1, d1)", "-r1agent(N1, e1)", "-v1accept(N1)", "-r1of(d1, O1)", "-n1justiceC34(O1)"), List("r1theme(m1, l1)"), List("r1agent(m1, e1)"), List("v1know(m1)"), List("r1theme(k1, j1)"), List("r1agent(k1, f1)"), List("r1patient(k1, e1)"), List("v1let(k1)"), List("r1theme(i1, h1)"), List("r1agent(i1, g1)"), List("v1say(i1)")), List(List("r1patient(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2), D2)", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34satirize(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("r1agent(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2), E2)", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34satirize(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("v1accept(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34satirize(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("r1of(D2, o2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34satirize(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("n1justiceC34(o2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34satirize(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)")))

testSentpair("""    Both Indian officials and US diplomats admit that the two countries have  never been so far apart on crucial policy issues, but emphasise that the trip  is nevertheless important to set the course for future ties.""", """    Both Indian officials and US diplomats admit that the two countries have  never been so far apart on crucial policy issues, but emphasise that the trip  is nevertheless important to be decisive for future ties.""", """set the course for->be decisive for""", 
List(List("n1course(a1)"), List("n1trip(b1)"), List("n1country(c1)"), List("card(c1, e1)"), List("c2number(e1)"), List("n1numeral(e1)"), List("n1official(d1)"), List("a1indian(d1)"), List("r1theme(f1, g1)"), List("r1agent(f1, i1)"), List("v1admit(f1)"), List("a1never(o1)"), List("r1on(o1, n1)"), List("r1subset_of(k1, n1)"), List("r1patient(s1, a1)"), List("r1agent(s1, b1)"), List("v1set(s1)"), List("r1for(a1, t1)"), List("n1tiesC46(t1)"), List("a1future(t1)"), List("a1nevertheless(b1)"), List("a1important(b1)"), List("r1theme(k1, j1)"), List("n1emphasise(k1)"), List("r1subset_of(m1, n1)"), List("n1issuesC44(m1)"), List("r1of(m1, l1)"), List("n1policy(l1)"), List("a1crucial(m1)"), List("a1so(c1)"), List("a1far(c1)"), List("a1apart(c1)"), List("r1subset_of(h1, i1)"), List("n1diplomat(h1)"), List("loc1us(h1)"), List("r1subset_of(d1, i1)")), List(List("-n1trip(A2)", "-n1country(B2)", "-card(B2, D2)", "-c2number(D2)", "-n1numeral(D2)", "-n1official(C2)", "-a1indian(C2)", "-r1theme(E2, F2)", "-r1agent(E2, H2)", "-v1admit(E2)", "-a1never(N2)", "-r1on(N2, M2)", "-r1subset_of(J2, M2)", "-r1for(S2, R2)", "-n1tiesC46(R2)", "-a1future(R2)", "-a1decisive(A2)", "-a1nevertheless(A2)", "-a1important(A2)", "-r1theme(J2, I2)", "-n1emphasise(J2)", "-r1subset_of(L2, M2)", "-n1issuesC44(L2)", "-r1of(L2, K2)", "-n1policy(K2)", "-a1crucial(L2)", "-a1so(B2)", "-a1far(B2)", "-a1apart(B2)", "-r1subset_of(G2, H2)", "-n1diplomat(G2)", "-loc1us(G2)", "-r1subset_of(C2, H2)")))

testSentpair("""    Both Indian officials and US diplomats admit that the two countries have  never been so far apart on crucial policy issues, but emphasise that the trip  is nevertheless important to set the course for future ties.""", """    Both Indian officials and US diplomats admit that the two countries have  never been so far apart on crucial policy issues, but emphasise that the trip  is nevertheless important to be decisive for future ties.""", """set the course for@R@->be decisive for@R@""", 
List(List("n1course(a1)"), List("n1trip(b1)"), List("n1country(c1)"), List("card(c1, e1)"), List("c2number(e1)"), List("n1numeral(e1)"), List("n1official(d1)"), List("a1indian(d1)"), List("r1theme(f1, g1)"), List("r1agent(f1, i1)"), List("v1admit(f1)"), List("a1never(o1)"), List("r1on(o1, n1)"), List("r1subset_of(k1, n1)"), List("r1patient(s1, a1)"), List("r1agent(s1, b1)"), List("v1set(s1)"), List("r1for(a1, t1)"), List("n1tiesC46(t1)"), List("a1future(t1)"), List("a1nevertheless(b1)"), List("a1important(b1)"), List("r1theme(k1, j1)"), List("n1emphasise(k1)"), List("r1subset_of(m1, n1)"), List("n1issuesC44(m1)"), List("r1of(m1, l1)"), List("n1policy(l1)"), List("a1crucial(m1)"), List("a1so(c1)"), List("a1far(c1)"), List("a1apart(c1)"), List("r1subset_of(h1, i1)"), List("n1diplomat(h1)"), List("loc1us(h1)"), List("r1subset_of(d1, i1)")), List(List("-n1trip(A2)", "-n1country(B2)", "-card(B2, D2)", "-c2number(D2)", "-n1numeral(D2)", "-n1official(C2)", "-a1indian(C2)", "-r1theme(E2, F2)", "-r1agent(E2, H2)", "-v1admit(E2)", "-a1never(N2)", "-r1on(N2, M2)", "-r1subset_of(J2, M2)", "-r1for(S2, R2)", "-n1tiesC46(R2)", "-a1future(R2)", "-a1decisive(A2)", "-a1nevertheless(A2)", "-a1important(A2)", "-r1theme(J2, I2)", "-n1emphasise(J2)", "-r1subset_of(L2, M2)", "-n1issuesC44(L2)", "-r1of(L2, K2)", "-n1policy(K2)", "-a1crucial(L2)", "-a1so(B2)", "-a1far(B2)", "-a1apart(B2)", "-r1subset_of(G2, H2)", "-n1diplomat(G2)", "-loc1us(G2)", "-r1subset_of(C2, H2)")))

testSentpair("""    The cause of the fire, which broke out at around 1:30 a.m. and raged for  over an hour, was under investigation, police said.""", """    The cause of the fire, which broke out at around 1:30 a.m. and raged for  over an hour, was since investigation, police said.""", """under->since""", 
List(List("n1fireC44(a1)"), List("n1cause(b1)"), List("r1under(b1, i1)"), List("n1saidC46(i1)"), List("r1of(i1, h1)"), List("n1police(h1)"), List("a1investigationC44(i1)"), List("r1of(b1, a1)"), List("r1for(e1, d1)"), List("a1over(d1)"), List("n1hourC44(d1)"), List("r1agent(e1, a1)"), List("v1rage(e1)"), List("r1at(g1, f1)"), List("n1aC46mC46(f1)"), List("a11C5830(f1)"), List("a1around(f1)"), List("a1out(g1)"), List("r1agent(g1, a1)"), List("v1break(g1)")), List(List("-n1fireC44(A2)", "-n1cause(B2)", "-r1since(B2, I2)", "-n1saidC46(I2)", "-r1of(I2, H2)", "-n1police(H2)", "-a1investigationC44(I2)", "-r1of(B2, A2)", "-r1for(E2, D2)", "-a1over(D2)", "-n1hourC44(D2)", "-r1agent(E2, A2)", "-v1rage(E2)", "-r1at(G2, F2)", "-n1aC46mC46(F2)", "-a11C5830(F2)", "-a1around(F2)", "-a1out(G2)", "-r1agent(G2, A2)", "-v1break(G2)")))

testSentpair("""    UN Secretary-general Boutros Boutros-Ghali called Thursday in a statement  published in New York for the boundaries of the safe areas, where Moslems are  under siege from Serbs, to be clearly defined.""", """    UN Secretary-general Boutros Boutros-Ghali called Thursday in a statement  published in New York for the boundaries of the safe areas, where Moslems are  since siege from Serbs, to be clearly defined.""", """under->since""", 
List(List("nam1serbsC44(a1)"), List("n1areasC44(b1)"), List("a1safe(b1)"), List("n1boundary(c1)"), List("loc1new_york(d1)"), List("tim1thursday(e1)"), List("org1un_secretaryC45general_bout(f1)"), List("r1in(j1, i1)"), List("r1for(h1, c1)"), List("r1of(c1, b1)"), List("n1moslem(l1)"), List("r1under(l1, n1)"), List("a1clearly(n1)"), List("a1definedC46(n1)"), List("n1siege(n1)"), List("r1from(o1, a1)"), List("n1siege(o1)"), List("r1where(b1, g1)"), List("r1in(h1, d1)"), List("r1patient(h1, i1)"), List("v1publish(h1)"), List("n1statement(i1)"), List("r1patient(j1, e1)"), List("r1agent(j1, f1)"), List("v1call(j1)")), List(List("-nam1serbsC44(A2)", "-n1areasC44(B2)", "-a1safe(B2)", "-n1boundary(C2)", "-loc1new_york(D2)", "-tim1thursday(E2)", "-org1un_secretaryC45general_bout(F2)", "-r1in(J2, I2)", "-r1for(H2, C2)", "-r1of(C2, B2)", "-n1moslem(L2)", "-r1since(L2, N2)", "-a1clearly(N2)", "-a1definedC46(N2)", "-n1siege(N2)", "-r1from(O2, A2)", "-n1siege(O2)", "-r1where(B2, G2)", "-r1in(H2, D2)", "-r1patient(H2, I2)", "-v1publish(H2)", "-n1statement(I2)", "-r1patient(J2, E2)", "-r1agent(J2, F2)", "-v1call(J2)")))
}

  def testThisPair(f1:List[List[String]], f2:List[List[String]]) :Unit = {
    val formula1 = new CNF(f1)
    val formula2 = new CNF(f2)

    println("F1: " + formula1)
    println("F2: " + formula2)
    println
    val result  = resolveToFindDifference(f1, f2)
    result match {
      case Some((ir1, ir2)) =>
	println(ir1)
	println
	println(ir2)
      case None => println("failed.")
    }
  }

  def testSentpair(sent1Text:String, sent2Text:String, ruleText:String, f1:List[List[String]], f2:List[List[String]]) :Unit = {
    println("---------------------------------")
    println
    println("S1: " + sent1Text)
    println
    println("S2 : " + sent2Text)
    println
    println("Rule : " + ruleText)
    println
    testThisPair(f1, f2)
  }
}



