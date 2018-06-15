import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}


class Greeter extends Actor {
  override def receive: Receive = {
    case name: String => println(s"Hello World $name")
  }
}

  object Samlpe
  {
    def main(args: Array[String]): Unit = {
      val system = ActorSystem("Greeting")
      val greeter = system.actorOf(Props[Greeter],"greeter")
      greeter ! "Hi"
    }
  }