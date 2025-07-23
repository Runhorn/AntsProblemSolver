case class Config(
    alpha: Int = 1,
    beta: Int = 3,
    ants: Int = 48,
    iterations: Int = 50,
    vaporCoeff: Double = 0.3,
    strategy: Strategy = WeightedRandom
)
