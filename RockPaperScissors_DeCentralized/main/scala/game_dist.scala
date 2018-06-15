// Author : Karthik Vaidhyanathan
// Institute : Gran Sasso Science Institute
// Course : Core Course on Reactive Systems
// Program : To simulate a 3 player distributed rock paper scissor game using AKKA

import java.io.File


import Player.{Play1, PlayMove1}
import Player2.{Play2, PlayMove2}
import Player3.{Play3, PlayMove3}
import com.typesafe.config.{Config, ConfigFactory}
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}

import scala.collection.mutable
// First the configuration file needs to read to understand the properties
// Use a single ton to read the properties

import scala.concurrent._
import ExecutionContext.Implicits.global  // implicit execution context



object ConfigReader
{
  // To initialize the game configurations
  var rounds = ""   // Keep track of the number of rounds
  var moves = List("rock","scissor","paper")
  //////////////
  def ReadConfig(): Unit = {
    // Read the configurations from the settings.conf inside the properties folder
    val configPath = "properties/"
    val config = ConfigFactory.parseFile(new File(configPath + "settings.conf"))
    rounds = config.getString("rounds")
  }
  def generateMove(): String = {
    // Randomly generated a move when called
    var possibleMoves = ConfigReader.moves
    var randomNumber = new scala.util.Random // Generate a random Number
    // Randomly let the players select a number between 0 and 2
    // The number is then translated into a move

    var playerMove = possibleMoves(randomNumber.nextInt(3))
    playerMove  // Return a random move

  }

}



object RuleEngine
{
  def getScore(player1 : String,player2 : String, player3 : String) : Array[Int] ={
    // To compute the scores for each of the players given their moves
    var moveOrder = ConfigReader.moves // Gives the preccedance of moves

    //var scoreList = List(0, 0, 0)
    var scoreList = Array(0,0,0)
    // Get the index of the moves made by each player
    var indexPlayer1 = moveOrder.indexOf(player1)
    var indexPlayer2 = moveOrder.indexOf(player2)
    var indexPlayer3 = moveOrder.indexOf(player3)

    var ruleMap : mutable.Map[(String,String),String] = mutable.Map()
    // Defining the possible rules between combinations

    ruleMap(("rock","scissor")) = "rock"
    ruleMap(("scissor","rock")) = "rock"

    ruleMap(("rock","paper")) = "paper"
    ruleMap(("paper","rock")) = "paper"

    ruleMap(("paper","scissor")) = "scissor"
    ruleMap(("scissor","paper")) = "scissor"

    var winner = ""
    var winIndex = 0
    // Compute the scores based on the rule
    if ((indexPlayer1 == indexPlayer2) && (indexPlayer2!=indexPlayer3))
      {
        winner = ruleMap((player2,player3))
        winIndex = moveOrder.indexOf(winner)
        if (winIndex == indexPlayer1)
          {
            scoreList(0) = 1
            scoreList(1) = 1
          }
        else
          {
            scoreList(2) = 2
          }
      }
    else if ((indexPlayer2 == indexPlayer3)&& (indexPlayer1!=indexPlayer3))
      {
        winner = ruleMap((player1,player3))
        winIndex = moveOrder.indexOf(winner)
        if (winIndex == indexPlayer2)
        {
          scoreList(1) = 1
          scoreList(2) = 1
        }
        else
        {
          scoreList(0) = 2
        }

      }
    else if ((indexPlayer1 == indexPlayer3 )&&(indexPlayer1!=indexPlayer2)) {
      winner = ruleMap((player1,player2))

      winIndex = moveOrder.indexOf(winner)
      //println(winIndex)
      if (winIndex == indexPlayer1)
      {
        scoreList(0) = 1
        scoreList(2) = 1
      }
      else
      {
        scoreList(1) = 2
      }
      }
    scoreList
  }
}

object Player{
  //Companion for player 1

  final case class PlayMove1(playerMove : String, playerId : String) // For getting the moves from other players
  case object Play1 // It recieves a string for its move and sends it to others
}

class Player extends Actor
{
  // Player 1
  //import Player._



  var otherMove = ""
  var moveQueue = new mutable.Queue[String]
  var moveMap : mutable.Map[String,String] = mutable.Map() // To keep the moves made by players as key value pairs

  var scoreMap : mutable.Map[String, Int] = mutable.Map()// To keep the score of each of the players
  var roundNumber = 0

  var scoreList:Array[Int] = Array()   // To store the scores of each of players obtained from getScore of RuleEngine
  // Initialize the scores of all the players to 0
  scoreMap("player1") = 0
  scoreMap("player2") = 0
  scoreMap("player3") = 0


  def receive = {
    // Defined to perform action on receiving a message

    case PlayMove1(playerMove,playerId) => {
      // Gets the move from other players and compute the score on receiving moves from other 2 players
      otherMove = playerMove
      if (playerId.contains("player2")) {
        println("player 1 receives move :", otherMove, " from player 2 ")
        moveMap("player2") = otherMove
      }
      else if (playerId.contains("player3")) {
        println("player 1 receives move :", otherMove, " from player 3 ")
        moveMap("player3") = otherMove
      }

      if (moveMap.size == 2) {

        var player1 = moveQueue.dequeue()
        var player2 = moveMap("player2")
        var player3 = moveMap("player3")

        scoreList = RuleEngine.getScore(player1, player2, player3)

        // Update the score map continously and at the end this can be sent as an output

        scoreMap("player1") = scoreMap("player1") + scoreList(0)
        scoreMap("player2") = scoreMap("player2") + scoreList(1)
        scoreMap("player3") = scoreMap("player3") + scoreList(2)

        roundNumber = roundNumber + 1
        println("scores in Player 1 :",scoreMap)
        println("round : ", roundNumber, "complete in player 1")
        moveMap.clear() // Clear once the serves has been done

      }
    }
    case Play1 => {
      // This is to send the moves to other players
      var myMove= ConfigReader.generateMove()   // Randomly generate a move and send it to other players
      moveQueue.enqueue(myMove)       // Store the move playeed in a queue to keep track of all the moves made in order
      context.actorSelection("/user/player2") ! PlayMove2(myMove,"player1")
      context.actorSelection("/user/player3") ! PlayMove3(myMove,"player1")
    }



  }
}

object Player2{
  //Companion for player 2
  //def props(player : ActorRef,player3: ActorRef): Props = Props(new Player2(player,player3))
  final case class PlayMove2(playerMove : String,playerId : String)  // For getting the moves from other players
  case object Play2 // It recieves a string for its move and sends it to others
}

class Player2 extends Actor
{
  // Player2
  //import Player2._



  var scoreMap : mutable.Map[String, Int] = mutable.Map()// To keep the score of each of the players
  var moveMap : mutable.Map[String,String] = mutable.Map() // To keep the moves made by players as key value pairs
  var moveQueue = new mutable.Queue[String]


  var otherMove = ""
  var roundNumber = 0
  var scoreList:Array[Int] = Array() // To store the scores of each of players obtained from getScore of RuleEngine
  // Initialize the score of all the players to 0
  scoreMap("player1") = 0
  scoreMap("player2") = 0
  scoreMap("player3") = 0
  def receive = {
    case PlayMove2(playerMove,playerId) => {
      otherMove = playerMove
      if (playerId.contains("player1")) {
        println("player 2 receives move :", otherMove, " from player 1 ")
        moveMap("player1") = otherMove
      }
      else if (playerId.contains("player3")) {
        println("player 2 receives move :", otherMove, " from player 3 ")
        moveMap("player3") = otherMove
      }
      if (moveMap.size == 2) {
        var player1 = moveMap("player1")
        var player2 = moveQueue.dequeue()
        var player3 = moveMap("player3")

        scoreList = RuleEngine.getScore(player1, player2, player3)

        // Update the score map continously and at the end this can be sent as an output

        scoreMap("player1") = scoreMap("player1") + scoreList(0)
        scoreMap("player2") = scoreMap("player2") + scoreList(1)
        scoreMap("player3") = scoreMap("player3") + scoreList(2)

        roundNumber = roundNumber + 1
        println("scores in Player 2 :", scoreMap)
        println("round :", roundNumber, "comnpleted in player 2")
        moveMap.clear() // Clear once the serves has been done

      }
    }
    case Play2 => {
      // This is to send the moves to other players
      var myMove = ConfigReader.generateMove() // Randomly generate a move and send it to other players
      moveQueue.enqueue(myMove)   // Store the move playeed in a queue to keep track of all the moves made in order
      context.actorSelection("/user/player1") ! PlayMove1(myMove,"player2")
      context.actorSelection("/user/player3") ! PlayMove3(myMove,"player2")
     }

  }
}


object Player3{
  //Companion for player 2
  //def props(player : ActorRef,player2: ActorRef): Props = Props(new Player3(player,player2))
  final case class PlayMove3(playerMove : String,playerId : String)
  case object Play3// It recieves a string for its move and sends it to value
  case object Done

}

class Player3 extends Actor
{
  // Player 3
  //import Player3._
  var roundCount =0 // This is to keep track of the rounds
  var otherMove  = ""
  var scoreMap : mutable.Map[String, Int] = mutable.Map() // To keep the score of each of the players
  var moveMap : mutable.Map[String,String] = mutable.Map() // To keep the moves made by players as key value pairs

  var moveQueue = new mutable.Queue[String]
  var roundNumber = 0
  var scoreList:Array[Int] = Array() // To store the scores of each of players obtained from getScore of RuleEngine
  // Intialize the score of all the players to 0
  scoreMap("player1") = 0
  scoreMap("player2") = 0
  scoreMap("player3") = 0



def receive = {
  case PlayMove3(playerMove, playerId) => {

    otherMove = playerMove

    if (playerId.contains("player1")) {
      println("player 3 receives move :", otherMove, " from player 1 ")

      moveMap("player1") = otherMove
    }
    else if (playerId.contains("player2")) {
      println("player 3 receives move :", otherMove, " from player 2 ")
      moveMap("player2") = otherMove
    }

    if (moveMap.size == 2) {
      var player1 = moveMap("player1")
      var player2 = moveMap("player2")
      var player3 = moveQueue.dequeue()

      scoreList = RuleEngine.getScore(player1, player2, player3)

      // Update the score map continously and at the end this can be sent as an output

      scoreMap("player1") = scoreMap("player1") + scoreList(0)
      scoreMap("player2") = scoreMap("player2") + scoreList(1)
      scoreMap("player3") = scoreMap("player3") + scoreList(2)

      roundNumber = roundNumber + 1
      println("scores in Player 3 :", scoreMap)
      println("round :", roundNumber, "completed in player 3")
      moveMap.clear() // Clear once the serves has been done



    }
  }
  case Play3 => {
    // This is to send the moves to other players
    var myMove = ConfigReader.generateMove() // Randomly generate a move and send it to other players
    moveQueue.enqueue(myMove)  // Store the move playeed in a queue to keep track of all the moves made in order
    context.actorSelection("/user/player1") ! PlayMove1(myMove, "player3")
    context.actorSelection("/user/player2") ! PlayMove2(myMove, "player3")

  }


}
}



object GameDist
{
  // The executable object containing the main
  import Player._
  import Player2._
  import Player3._
  def main(args: Array[String]): Unit = {
    ConfigReader.ReadConfig() // First the config reader reads the value
    var rounds = ConfigReader.rounds
    println(rounds)
    val system: ActorSystem = ActorSystem("Game")

    // Instantiate the actors

    val player1 : ActorRef = system.actorOf(Props[Player],"player1")
    val player2 : ActorRef = system.actorOf(Props[Player2],"player2")
    val player3 : ActorRef = system.actorOf(Props[Player3],"player3")

    var index  = 0  // To keep track the number of times the loop is executed / to keep track of the rounds
    //implicit val context = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
    for (index <- 1 to rounds.toInt) {

      val finalVal: Future[Unit] = Future {
        // Let all players play parallely
        player2 ! Play2
        player1 ! Play1
        player3 ! Play3
      }

    }




  }
}