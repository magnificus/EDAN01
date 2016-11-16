import org.jacop.scala._
import scala.collection.mutable

object Logistics extends App with jacop {
  val graph_size = 6
  val start = 1
  val dest = Array(5,6)
  val n_edges = 9
  val input_from =  Array(1,1,1,2,2,3,3,3,4)
  val input_to =    Array(2,3,4,3,5,4,5,6,6)
  val from = input_from ++ input_to
  val to = input_to ++ input_from
  val input_cost = Array(6,1,5,5,3,5,6,4,2)
  val cost = input_cost ++ input_cost
  val edges_used = Array.fill(n_edges*2)(new IntVar("e", 0, 1))
  //val vars: Array[IntVar] = Array.fill(graph_size)(new IntVar())
  val vars = Array.ofDim[IntVar](dest.length, graph_size)
  val domains = Array.fill(graph_size)(new IntSet())


  // Build graph
    from.zipWithIndex.foreach{ case (v, i) => {
      domains(v - 1) += to(i)
      domains(v - 1) += v
      (0 to dest.length - 1).foreach(d => {
        vars(d)(v - 1) = new IntVar(s"x${v.toString}", domains(v - 1))
      })
    }}

  // Add start to domain of dests
  (0 to dest.length - 1).foreach(i => {  
    dest.foreach{ d =>
      val domainCopy = domains.clone
      domainCopy(d - 1) += start
      vars(i)(d - 1) = new IntVar(s"x${d.toString}", domainCopy(d - 1))
    }
  })

  (0 to dest.length - 1).foreach(i => {
    subcircuit(vars(i))
  })


  (0 to dest.length - 1).foreach(j => {
    edges_used.zipWithIndex.foreach{ case (v, i) => {
      val edge = new IntVar()
      edge #= abs(vars(j)(from(i)-1) - to(i))
      val temp = new IntVar("t")

      //edge #= abs(edge)
      val const = new IntVar()
      const #= 1
      val a = Array(edge, const)
      edges_used(i) >= abs(min(a)-1)
    }}
  })

  println(cost.mkString(" "))
  val reward = new IntVar("r", 0, 1000)
  //reward #= sumDom(edges_used, cost)
  
  val totCosts = edges_used.zip(cost).map{ (a) =>
   a._1 * a._2
  }
  reward #= sum[IntVar](totCosts)

  // Dest must be in the solution of corresponding vars
  (0 to dest.length -1).foreach(d => {
    vars(d)(dest(d) - 1) #\= dest(d)
    // Start must be in the solution
    vars(d)(start - 1) #\= start
  })

  val all_arrays = vars(0) ++ vars(1)

  val result = minimize(search(all_arrays, input_order, indomain_min), reward)
  //statistics

  def printSol(): Unit = {
    print("Solution: ")
      for (v <- vars)
        print(v + " ")
      println("\nedges_used: ")
      for (e <- edges_used)
        print(e + " ")
      println
    println("reward: " + reward.dom)
  }

}
