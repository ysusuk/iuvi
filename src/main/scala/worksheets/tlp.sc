trait Status
trait Open extends Status
trait Closed extends Status

trait Door[S <: Status]
object Door {
  def apply[S <: Status] = new Door[S] {}

  def open[S <: Closed](d: Door[S]) = Door[Open]
  def close[S <: Open](d: Door[S]) = Door[Closed]
}

val closedDoor = Door[Closed]
val openDoor = Door[Open]

val closedAgain = Door.close(openDoor)
val openAgain = Door.open(closedAgain)