package bot

import bot.Tile.{Air, Mine, Tavern, Wall}

object Renderer {
  val YELLOW = "\u001B[33m"
  val CYAN = "\u001B[36m"
  val GREEN = "\u001B[32m"
  val RED = "\u001B[31m"
  val RESET = "\u001B[0m"

  def renderBoard(board: Seq[WeightedTile], input: Input, skipBoard: Boolean = false): String = {
    def printPositionedTile(pt: WeightedTile): String = {
      val weight = Math.round(pt.weight * 10) / 10d
      pt.tile match {
        case Wall => s"▓▓▓▓▓"
        //      case Air if dirReason.path.exists(_.positionedTiles.exists(_.pos == pt.pos)) => s"$YELLOW  " + s"${pt.weight}▶".padTo(3, " ").mkString + RESET
        case Air => s" $weight "
        case Mine(Some(id)) if id == input.hero.id => s"$GREEN $weight $RESET"
        case Mine(Some(id)) => s"$CYAN $weight $RESET"
        case Mine(None) => s"$CYAN $weight $RESET"
        case Tavern => s"$YELLOW $weight $RESET"
        case Tile.Hero(id) if id == input.hero.id => s"$GREEN  😀  $RESET"
        case Tile.Hero(id) => s"$RED  $id  $RESET"
        case _ => s"? $weight "
      }
    }

    println(input.game.board.size)
    val renderedBoard: String =
      board.foldLeft("") { (s: String, pt: WeightedTile) =>
        pt match {
          case WeightedTile(_, _, pos) =>
            val tileString = s + s"${printPositionedTile(pt).padTo(5, " ").mkString}"

            if (pos.y == input.game.board.size - 1) tileString + "\n"
            else tileString
          case _ => s + " ?? "
        }
      }

    s"Life: ${input.hero.life} |  \n\n$renderedBoard"
  }
}