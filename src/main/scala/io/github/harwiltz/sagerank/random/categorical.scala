package io.github.harwiltz.sagerank.random

import scala.util.Random

class EmpiricalCategoricalDistribution[T](counts: Map[T, Int]) {
  val rnd = new Random

  def sample: T = {
    val list = this.counts.flatMap { case (k, v) => List.fill(v)(k) }.toVector
    list(this.rnd.nextInt(list.size))
  }

  def marginalized(vars: Set[T]): EmpiricalCategoricalDistribution[T] = {
    new EmpiricalCategoricalDistribution[T](this.counts.filter { case (k, v) => vars contains k })
  }
}
