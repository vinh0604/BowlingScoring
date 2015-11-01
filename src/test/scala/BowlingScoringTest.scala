import org.scalatest._

/**
 * Created by vinhbachsy on 1/11/15.
 */
class BowlingScoringTest extends FlatSpec with Matchers {
  "BowlingScoring.score" should "return 0 if there is no frame" in {
    BowlingScoring.score(Nil) should be (0)
  }

  "BowlingScoring.score" should "return frame score if there is one frame" in {
    BowlingScoring.score(Seq[Frame](NormalFrame(1,2))) should be (3)
    BowlingScoring.score(Seq[Frame](SpareFrame(6,4))) should be (10)
    BowlingScoring.score(Seq[Frame](StrikeFrame())) should be (10)
  }

  "BowlingScoring.score" should "add frame bonus for every frame except last frame" in {
    BowlingScoring.score(
      Seq[Frame](NormalFrame(1,2), SpareFrame(4,6))) should be (13)
    BowlingScoring.score(
      Seq[Frame](NormalFrame(1,2), StrikeFrame())) should be (13)
    BowlingScoring.score(
      Seq[Frame](SpareFrame(4,6), NormalFrame(1,2))) should be (14)
    BowlingScoring.score(
      Seq[Frame](StrikeFrame(), NormalFrame(1,2))) should be (16)
  }

  "FrameFactory" should "make FinalFrame if round is 10" in {
    FrameFactory.make(10).isInstanceOf[FinalFrame] should be (true)
  }

  "FrameFactory" should "not make FinalFrame if round is not 10" in {
    FrameFactory.make(1).isInstanceOf[FinalFrame] should be (false)
  }
}
