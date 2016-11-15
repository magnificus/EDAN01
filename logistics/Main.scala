import org.jacop.scala._

object Main extends App with jacop {
  val s = new IntVar("s", 0, 9)
  val e = new IntVar("e", 0, 9)
  val n = new IntVar("n", 0, 9)
  val d = new IntVar("d", 0, 9)
  val m = new IntVar("m", 0, 9)
  val o = new IntVar("o", 0, 9)
  val r = new IntVar("r", 0, 9)
  val y = new IntVar("y", 0, 9)

  val fd = Array(s,e,n,d,m,o,r,y)

  alldifferent(fd)

  1000*s + 100*e + 10*n + d + 1000*m + 100*o + 10*r + e #=
    10000*m + 1000*o + 100*n + 10*e + y

  s #> 0
  m #> 0

  val result = satisfy(search(fd, input_order, indomain_min),
      printSol)

  statistics

  def printSol(): Unit = {
    print("Solution: ")
      for (v <- fd)
        print(v + " ")
      println
  }
}