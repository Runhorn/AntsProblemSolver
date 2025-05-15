import domain.City

import scala.util.Random

sealed trait Strategy {
  def chooseBestCity(choices: Set[(City, Double)]): City
}

case object WeightedRandom extends Strategy {
  def chooseBestCity(choices: Set[(City, Double)]): City =
    takeRandomAtChance(
      choices.map { case (city, weight) =>
        (city, weight / choices.map(_._2).sum)
      }
    )

  private def takeRandomAtChance(probabilities: Set[(City, Double)]): City = {
    val r = Random.nextDouble()
    probabilities
      .foldLeft((Option.empty[City], 0.0)) { case ((selected, cumulative), (city, probability)) =>
        val newCumulative = cumulative + probability
        if (selected.isEmpty && newCumulative >= r) (Some(city), newCumulative)
        else (selected, newCumulative)
      }
      ._1
      .getOrElse(probabilities.head._1)
  }
}

case object Naive extends Strategy {
  def chooseBestCity(choices: Set[(City, Double)]): City = choices.maxBy(_._2)._1
}
