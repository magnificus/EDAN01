import org.jacop.scala._
import scala.collection.mutable

object Logistics extends App with jacop {
  val graph_size = 6
  val start = 1
  val dest = Array(6)
  val n_edges = 7
  val input_from =  Array(1,1,2,2,3,4,4)
  val input_to =    Array(2,3,3,4,5,5,6)
  val from = input_from ++ input_to
  val to = input_to ++ input_from
  val input_cost = Array(4,2,5,10,3,4,11)
  val cost = input_cost ++ input_cost

  val vars: Array[IntVar] = Array.fill(graph_size)(new IntVar())
  val domains = Array.fill(graph_size)(new IntSet())


  // Build graph
  from.zipWithIndex.foreach{ case (v, i) => {
    domains(v - 1) += to(i)
    domains(v - 1) += v
    vars(v - 1) = new IntVar(s"x${v.toString}", domains(v - 1))
  }}

  // Build cost map
  val map: mutable.Map[(Int, Int), Int] = mutable.Map()

  from.zipWithIndex.foreach{ case (v, i) => {
    map += (v, to(i)) -> cost(i)
  }}

  (1 to graph_size).foreach(v => {
    map += (v, v) -> 0
  })

  // Add start to domain of dests
  dest.foreach{ d =>
    domains(d - 1) += start
    vars(d - 1) = new IntVar(s"x${d.toString}", domains(d - 1))
    map += (d, start) -> 0
  }


  println(vars.mkString(" "))
  subcircuit(vars)

  // Dest must be in the solution
  dest.foreach(d =>
    vars(d - 1) #\= d
  )

  // Start must be in the solution
  vars(start - 1) #\= start
    // totalCost

  val min = new IntVar("min-cost", -10000, 10000)
  val intVector = Array.fill(graph_size)(new IntVar())
  vars.zipWithIndex.foreach{ case (v, i) => {
    val theCost = (map get (i + 1, v.value())).get
    intVector(i) #= theCost
  }}
  println(intVector.mkString(" "))
  //min #= sum(vars, intVector)

  val result = satisfyAll(search(vars, input_order, indomain_min), printSol)

  statistics

  def printSol(): Unit = {
    print("Solution: ")
      for (v <- vars)
        print(v + " ")
      println
    println("min: " + min.value())
  }

}
