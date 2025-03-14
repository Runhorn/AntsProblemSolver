import domain.Context.cities
import utils.Image

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
      val bestPath = new Solver(cities.toSet, config).solve
      Image.draw(bestPath)
    case _ =>
      println("Podano nieprawidłowe argumenty! Użyj --help, aby zobaczyć dostępne.")
  }
}
