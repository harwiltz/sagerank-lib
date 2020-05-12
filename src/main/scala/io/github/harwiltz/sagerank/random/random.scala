package io.github.harwiltz.sagerank.random

import scala.math
import scala.util.Random

class RandomSampler extends Random {
  private val minLogArg = 0.000001

  def sampleGeometric(p: Double): Double = {
    val u = this.nextDouble()
    math.log(math.max(1.0 - u, minLogArg)) / math.log(1.0 - p)
  }
}
