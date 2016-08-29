package schedule

import scala.io.Source
import collection.mutable.ArrayBuffer

object JobScore {
  val ratioScore = (job: Job) => job.weight / job.length
}

class Job(val weight: Int, val length: Int, val scoreFunction: Job => Int = null) extends Ordered[Job] {
  val score =
    if (scoreFunction == null) (weight - length).toDouble
    else weight.toDouble / length.toDouble

  def compare(that: Job): Int = {
    if (this.score > that.score) {
      1

    } else if (this.score < that.score) {
      -1

    } else {
      if (this.weight > that.weight) 1
      else if (this.weight == that.weight) 0
      else -1
    }
  }

  override def equals(that: Any): Boolean = that match {
    case that: Job => this.score == that.score && that.weight == that.weight
    case _ => false
  }

  override def toString = weight + "#" + length + " => " + score
}

class Schedule(inputFile: String, scoreFunction: Job => Int = null) {
  val jobs = {
    val jobs = ArrayBuffer[Job]()

    for (line <- Source.fromFile(inputFile).getLines if line.split(" ").length > 1) {
      val array = line.split(" ")

      if (array.length > 1) jobs += new Job(array(0).toInt, array(1).toInt, scoreFunction)
    }

    val sortedJobs = jobs.sorted(Ordering[Job].reverse)

    checkOrder(sortedJobs)

    jobs.sorted(Ordering[Job].reverse)
  }

  def weightedSumOfCompletionTime: Long = {
    checkOrder(jobs)
    var completionTime: Long = 0
    var sum: Long = 0

    for (job <- jobs) {
      completionTime += job.length
      sum += job.weight * completionTime
    }

    println(sum)
    sum
  }

  def checkOrder(jobs: ArrayBuffer[Job]) = {
    var i = 1

    while (i < jobs.length) {
//      assert(jobs(i - 1) >= jobs(i))
      assert((jobs(i - 1) compare jobs(i)) >= 0)
      i += 1
    }
  }
}

object Main extends App {
  new Schedule("test_input.txt").weightedSumOfCompletionTime
  new Schedule("test_input.txt", JobScore.ratioScore).weightedSumOfCompletionTime
}