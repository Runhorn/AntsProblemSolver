import domain.Context.cities

import scopt.OParser

object AntAlgorithmSolver extends App {
  val configParser = OParser.builder[Config]
  val parser = {
    import configParser._
    OParser.sequence(
      opt[Int]("alpha")
        .action((x, c) => c.copy(alpha = x))
        .text("Współczynnik alfa"),
      opt[Int]("beta")
        .action((x, c) => c.copy(beta = x))
        .text("Współczynnik"),
      opt[Int]("ants")
        .action((x, c) => c.copy(ants = x))
        .text("Liczba mrówek"),
      opt[Int]("iterations")
        .action((x, c) => c.copy(iterations = x))
        .text("Liczba iteracji"),
      opt[Double]("vaporCoeff")
        .action((x, c) => c.copy(vaporCoeff = x))
        .text("Współczynnik parowania")
    )
  }

  OParser.parse(parser, args, Config()) match {
    case Some(config) =>
      val solver   = new Solver(cities.toSet, config)
      val bestPath = solver.solve
      println("Znaleziono najlepszą scieżkę:")
      println(bestPath.cities.map(_.id).mkString("->"))
      println(s"o długosci: ${bestPath.distance}.")
    case _ =>
      println("Podano nieprawidłowe argumenty! Użyj --help, aby zobaczyć dostępne.")
  }
}
