import scala.util.Random

/**
 * Created by vinhbachsy on 1/11/15.
 */
object BowlingScoring {
  def main(args: Array[String]) = {
    val frames: Seq[Frame] = (1 to 10).toList.map(FrameFactory.make(_))
    println(score(frames))
  }

  def score(frames: Seq[Frame]): Int = frames match {
    case Nil => 0
    case frame::Nil => frame.score
    case current::next::remains => current.score + current.bonus(next) + score(next::remains)
  }
}

trait Frame {
  def first: Int
  def second: Int
  def score: Int
  def bonus(nextFrame: Frame): Int
}

case class NormalFrame(first: Int, second: Int) extends Frame {
  def score = first + second
  def bonus(nextFrame: Frame) = 0
}

case class SpareFrame(first: Int, second: Int) extends Frame {
  def score = first + second
  def bonus(nextFrame: Frame) = nextFrame.first
}

case class StrikeFrame() extends Frame {
  def first = 10
  def second = 0
  def score = 10
  def bonus(nextFrame: Frame) = nextFrame.first + nextFrame.second
}

case class FinalFrame(first: Int, second: Int, extra: Int) extends Frame {
  def score = first + second + extra
  def bonus(nextFrame: Frame) = 0
}

case class Lane(standPins: Int = 10, score: Int = 0) {
  val PINS = 10
  def empty: Boolean = standPins == 0
  def roll: Lane = {
    if (empty) return Lane().roll
    val downPins = Random.nextInt(standPins + 1)
    Lane(standPins - downPins, downPins)
  }
}

object FrameFactory {
  def make(round: Int): Frame = round match {
    case 10 => makeFinalFrame
    case _ => makeFrame
  }

  private def makeFrame: Frame = {
    val firstLane = Lane().roll
    if (firstLane.empty) return StrikeFrame()

    val secondLane = firstLane.roll
    if (secondLane.empty) return SpareFrame(firstLane.score, secondLane.score)
    NormalFrame(firstLane.score, secondLane.score)
  }

  private def makeFinalFrame: Frame = {
    var extra = 0

    val firstLane = Lane().roll
    val secondLane = firstLane.roll
    if (firstLane.empty || secondLane.empty) {
      extra = secondLane.roll.score
    }
    FinalFrame(firstLane.score, secondLane.score, extra)
  }
}