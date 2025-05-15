import domain.Context.cities
import scopt.{ OParser, Read }

object AntAlgorithmSolver extends App {
  implicit val strategyRead: Read[Strategy] = Read.reads {
    case "WeightedRandom" => WeightedRandom
    case "Naive"          => Naive
    case other            => throw new IllegalArgumentException(s"Niewspierana strategia: $other")
  }
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
        .text("Współczynnik parowania"),
      opt[Strategy]("strategy")
        .action((x, c) => c.copy(strategy = x))
        .text("Wybór strategii")
    )
  }

  OParser.parse(parser, args, Config()) match {
    case Some(config) =>
      val bestPath = new Solver(cities.toSet, config).solve
      println(bestPath.cities.map(c => c.id))
      println(bestPath.distance)
    case _ => println("Podano nieprawidłowe argumenty! Użyj --help, aby zobaczyć dostępne.")
  }
}
