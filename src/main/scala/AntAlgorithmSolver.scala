import domain.Context.cities
import scopt.{ OParser, Read }

import com.typesafe.scalalogging.LazyLogging

object AntAlgorithmSolver extends App with LazyLogging {
  implicit val strategyRead: Read[Strategy] = Read.reads {
    case "WeightedRandom" => WeightedRandom
    case "Naive"          => Naive
    case other            => throw new IllegalArgumentException(s"Unsupported strategy: $other")
  }
  val configParser = OParser.builder[Config]
  val parser = {
    import configParser._
    OParser.sequence(
      opt[Int]("alpha")
        .action((x, c) => c.copy(alpha = x))
        .text("Alpha"),
      opt[Int]("beta")
        .action((x, c) => c.copy(beta = x))
        .text("Beta"),
      opt[Int]("ants")
        .action((x, c) => c.copy(ants = x))
        .text("Ants count"),
      opt[Int]("iterations")
        .action((x, c) => c.copy(iterations = x))
        .text("Number of iterations"),
      opt[Double]("vaporCoeff")
        .action((x, c) => c.copy(vaporCoeff = x))
        .text("Vaporization coefficient"),
      opt[Strategy]("strategy")
        .action((x, c) => c.copy(strategy = x))
        .text("Strategy choice: WeightedRandom or Naive"),
    )
  }

  OParser.parse(parser, args, Config()) match {
    case Some(config) => new Solver(cities.toSet, config).solve
    case _ => logger.error("Invalid arguments! Use --help to see available options.")
  }
}
