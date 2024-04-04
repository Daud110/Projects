package o1.adventure
import scala.util.Random
/** The class `Adventure` represents text adventure games. An adventure consists of a player and
  * a number of areas that make up the game world. It provides methods for playing the game one
  * turn at a time and for checking the state of the game.
  *
  * N.B. This version of the class has a lot of “hard-coded” information that pertains to a very
  * specific adventure game that involves a small trip through a twisted forest. All newly created
  * instances of class `Adventure` are identical to each other. To create other kinds of adventure
  * games, you will need to modify or replace the source code of this class. */
class Adventure:

  /** the name of the game */
  val title = "Labyrinth"


  private val entrance    = Area("Entrance" ,"" )
  private val hall        = Area("Hall of the Warriors" ,"There used to be a fearsome warrior who lived here. here are some materials you might find useful")
  private val spidernest  = Area("SpiderWeb", "It's dark and the hallway is filled with spiderweb.")
  private val wisdom      = Area("Palace of the wise man", "There is a lot of books in here.")
  private val spiderHouse = Area("Spiders house", "Welcome to the spiders house watch your step")
  private val jungle      = Area("Jungle","You are almost there, There is a vault on your east. You might need a key to get in")
  private val bossFight   = Area("Monsters house","AGH! A Monster. quickly get ready the monster is really strong! ")
  private val vault       = Area("Vault" ,"Here is the code to get you out: 1357")
  private val door        = Area( "Escape door", "Enter the code found from the Vault to get out of the labyrinth. The code is a four digit number type it as an command")
  private val workshop    = Area("Workshop", "There used to be a smith living here. Maybe you can craft something yourself")
  private val exit        = Area(" Exit","finally free")
  private val end = exit

  private val random = Random(12)

  private val areas: Vector[Area] = Vector(spiderHouse ,entrance, spidernest, wisdom, jungle, workshop)
  entrance   .setNeighbors(Vector("north" -> hall, "east" -> wisdom, "south" -> spidernest))
  hall       .setNeighbors(Vector("south" -> entrance, "east" -> workshop))
  wisdom     .setNeighbors(Vector("east" -> jungle, "west" -> entrance, "north" -> workshop))
  spidernest .setNeighbors(Vector("north" -> entrance, "south" -> spiderHouse))
  spiderHouse     .setNeighbors(Vector("north" -> spidernest))
  jungle     .setNeighbors(Vector("south" -> door,"west" -> wisdom , "north" ->  bossFight, "east" -> vault))
  exit       .setNeighbors(Vector("north" -> door))
  door.setNeighbors(Vector("south" -> exit, "north" -> jungle))
  bossFight  .setNeighbors(Vector("south"  -> jungle, "west" -> workshop))
  workshop.setNeighbors(Vector("south" -> wisdom, "west" -> hall, "east" -> bossFight))
  vault.setNeighbors(Vector("west" -> jungle))


  private val wood     =Item("wood", "A piece of wood you can make something out of it")
  private val iron     =Item("iron", "Some iron that you can use to make a nice sword")
  private val helmet   = Item("helmet","Protects your head and preciuos brain so that you can solve some intregrals later")
  private val shield   = Item("shield", "It's a golden shield. you can take some hit from the monster without dmaging your self.")
  private val diamond   = Item("diamond", "WOOOOW!!! This will sell for a fortune once you get out!!")
  private val monster  = Enemy("Monster", "Watch out. It can kill you")

  private val items: Vector[Item] = Vector(helmet, shield)
  bossFight.addEnemy(monster)


  def resetItems() =
    spidernest.disallowAccess()
    exit.disallowAccess()
    vault.disallowAccess()
    entrance.changeDescriprion("You are at the entrance of the Labyrinth. \nThere is an exit on the other side. Also you see a thick layer of spiderweb in south direction. \nUse something sharp to discover where it leads.")


    for area <- this.areas do
      area.removeAllItems()
    hall.addItem(wood)
    hall.addItem(iron)
    vault.addItem(diamond)
    for item <- this.items do
      var loc = random.nextInt(this.areas.size)
      this.areas(loc).addItem(item)




  resetItems()


  /** The character that the player controls in the game. */
  val player = Player(entrance)

  /** The number of turns that have passed since the start of the game. */
  var turnCount = 0
  /** The maximum number of turns that this adventure game allows before time runs out. */
  val timeLimit = 50



  /** Determines if the adventure is complete, that is, if the player has won. */
  def isComplete = this.player.location == this.end && this.player.has("diamond")

  /** Determines whether the player has won, lost, or quit, thereby ending the game. */
  def isOver = this.isComplete || this.player.hasQuit || this.turnCount == this.timeLimit

  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage = "Welcome to the labyrinth. Find your way out and take the diamond with you. "


  /** Returns a message that is to be displayed to the player at the end of the game. The message
    * will be different depending on whether or not the player has completed their quest. */
  def goodbyeMessage =
    if this.isComplete then
      "Nice you made it out alive with the diamond."
    else if this.turnCount == this.timeLimit then
      "Oh no! You didn´t make it out in time the labyrinth collapsed\nGame over!"
    else  // game over due to player quitting
      "Quitter!"


  /** Plays a turn by executing the given in-game command, such as “go west”. Returns a textual
    * report of what happened, or an error message if the command was unknown. In the latter
    * case, no turns elapse. */
  def playTurn(command: String) =
    val action = Action(command)
    val outcomeReport = action.execute(this.player)
    if outcomeReport.isDefined then
      this.turnCount += 1
    if this.player.hasDied() then
      resetItems()
      this.player.restart()
    outcomeReport.getOrElse(s"Unknown command: \"$command\".")

end Adventure

