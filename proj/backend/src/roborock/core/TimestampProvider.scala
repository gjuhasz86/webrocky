package roborock.core
import java.time.Instant

trait TimestampProvider {
  def timestamp: Long
}

case class ExplicitTimestampProvider(timestamp: Long) extends TimestampProvider

object DefaultTimestampProvider extends TimestampProvider {
  override def timestamp: Long = Instant.now.getEpochSecond
}
