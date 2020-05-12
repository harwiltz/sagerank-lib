package io.github.harwiltz.sagerank.random

import scala.math
import scala.util.Random

class RandomSampler extends Random {
  private val minLog = 0.000001

  def sampleGeometric(p: Double): Double = {
    val u = this.nextDouble()
    math.max(math.log(1.0 - u), minLog) / math.log(1.0 - p)
  }
}
