package o1.adventure

import scala.collection.mutable.Map
import scala.collection.mutable.Buffer

/** A `Player` object represents a player character controlled by the real-life user
  * of the program.
  *
  * A player object’s state is mutable: the player’s location and possessions can change,
  * for instance.
  *
  * @param startingArea  the player’s initial location */
class Player(startingArea: Area):

  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false              // one-way flag
  private var currentinventory = Map[String, Item]()
  private var defeated  = false

  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven

  /** Returns the player’s current location. */
  def location = this.currentLocation
  /** Attempts to move the player in the given direction. This is successful if there
    * is an exit from the player’s current location towards the direction name. Returns
    * a description of the result: "You go DIRECTION." or "You can't go DIRECTION." */
  def go(direction: String) =
    val destination = this.location.neighbor(direction)
    if destination.isDefined && destination.exists(_.canAccess()) then
      this.currentLocation = destination.getOrElse(this.currentLocation)
      "You go " + direction + "."
    else "You can't go " + direction + "."


  /** Causes the player to rest for a short while (this has no substantial effect in game terms).
    * Returns a description of what happened. */
  def rest() =
    "You rest for a while. Better get a move on, though."

  def alive = this.location.contains("key")


  /** Signals that the player wants to quit the game. Returns a description of what happened within
    * the game as a result (which is the empty string, in this case). */
  def quit() =
    this.quitCommandGiven = true
    ""

  def get(itemName: String): String =
    val item = this.location.removeItem(itemName)
    item match
      case Some(jotakin) =>  currentinventory += itemName -> jotakin
        "You pick up the " + itemName + "."
      case None => "There is no " + itemName + " here to pick up."

  def craftSword(): String =
    if currentinventory.contains("wood") && currentinventory.contains("iron") && this.location.name == "Workshop" then
      currentinventory -= "wood"
      currentinventory -= "iron"
      this.location.addItem(Item("sword", "Now you can defend your self and deal damage. Might need some protection to "))
      "You successfully crafted a sword. You lost your materials but gained something far better."
    else
      "Are you sure you have all the elements and in the right place"

  def fight(): String =
    if currentinventory.contains("sword") && currentinventory.contains("helmet") && currentinventory.contains("shield") && this.location.name == "Monsters house" then
      this.location.removeEnemy("Monster")
      this.location.changeDescriprion("Great you defeated the Monster he dropped a key.")
      this.location.addItem(Item("key", " This is the key to get you out."))
      "Monster was killed successfully"
    else
      this.currentLocation = startingArea
      this.currentinventory = Map[String, Item]()
      this.defeated = true
      "Monster was to strong. You have to start from the beginning. All your items were lost and replaced randomly in the map. \nYou need to start collecting items again."


  def help() =
    "You are stuck in a labyrinth. You´re job is to gather items and defeat a boss. \n" +
      "When you defeat the boss the boss drops a key that can open the vault which contains a code and a diamond\n" +
      "The code can used at the escape door to open the door and taking the diamond with you to the exit wins the game.\n\n" +
      "New commands:\n\n" +
      " use: The use command can be used to use an item. A sword can cut through a spiderweb and a key can open a door. \n" +
      " Using the items in certain areas can grant you access to new areas\n\n" +
      " fight: The fight command can be used when you are in an area with an enemy. You either can win the fight or loose it. \n" +
      " If you loose you go back to the entrance and loose all your items and the items are placed back in random locations. \n" +
      " If you win a key is dropped to the area and it can be used to open the vault.\n\n" +
      " craft: This command combines iron and wood making a sword. \n" +
      " To use this command you need to be at the workshop and have wood and iron in your inventory. Then a sword appears in the workshop.\n\n" +
      " secret code: There also is a secret command that is a four digit code that can be used at the escape door to grant access out.\n" +
      " for example if the code found in vault is 0000 then just type 0000 in when at the exit door to get access out."



  def hasDied() = this.defeated

  def restart() = this.defeated = false

  def drop(itemName: String): String =
    if has(itemName) then
      val item = currentinventory(itemName)
      this.location.addItem(item)
      currentinventory -= itemName
      "You drop the " +  itemName + "."
    else "You don't have that!"

  def useItem(itemName: String): String =

    if this.currentinventory.contains(itemName) && itemName == "sword" && this.location.name == "Entrance" then
      this.location.neighbor("south").foreach(_.allowAccess())
      this.location.changeDescriprion("You are at the entrance of the Labyrinth. \nThere is an exit on the other side.")
      itemName + "used successfully and there is a new exit south of entrance"

    else if this.currentinventory.contains(itemName) && itemName == "key" && this.location.name == "Jungle" then
      this.location.neighbor("east").foreach(_.allowAccess())
      itemName + " opened the vault"
    else itemName + " cannot be used"

  def openDoor(): String =
    if this.location.name == "Escape door" then
      this.location.neighbor("south").foreach(_.allowAccess())
      "Great you have successfully found the exit to the other side"
    else
      "Great you know the code, but you are not at the door"


  def examine(itemName: String): String =
    if has(itemName) then
      val item = currentinventory(itemName)
      "You look closely at the " + itemName + ".\n" +currentinventory(itemName).description
    else
      "If you want to examine something, you need to pick it up first."



  def has(itemName: String): Boolean = currentinventory.contains(itemName)

  def inventory: String =
    if currentinventory.isEmpty then
      "You are empty-handed."
    else
      "You are carrying:" + "\n" + currentinventory.keys.mkString("\n")




  /** Returns a brief description of the player’s state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name

end Player

