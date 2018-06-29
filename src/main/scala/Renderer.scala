package bot

import bot.Tile.{Air, Mine, Tavern, Wall}

object Renderer {
  val YELLOW = "\u001B[33m"
  val CYAN = "\u001B[36m"
  val GREEN = "\u001B[32m"
  val RED = "\u001B[31m"
  val RESET = "\u001B[0m"

  def renderBoard(board: Set[WeightedTile], input: Input, skipBoard: Boolean = false): String = {
    def printPositionedTile(pt: WeightedTile): String = pt.tile match {
      case Wall => s"▓▓▓▓▓"
//      case Air if dirReason.path.exists(_.positionedTiles.exists(_.pos == pt.pos)) => s"$YELLOW  " + s"${pt.weight}▶".padTo(3, " ").mkString + RESET
      case Air => s"  ${pt.weight}  "
      case Mine(Some(id)) if id == input.hero.id => s"$GREEN  ◧ ${pt.weight} $id$RESET"
      case Mine(Some(id)) => s"$CYAN  ◧ ${pt.weight} $id$RESET"
      case Mine(None) => s"$CYAN  ◧ ${pt.weight}  $RESET"
      case Tavern => s"$YELLOW P ${pt.weight} $RESET"
      case Tile.Hero(id) if id == input.hero.id => s"$GREEN 😀  $RESET"
      case Tile.Hero(id) => s"$RED 😈 $id$RESET"
      case _ => s"?${pt.weight}"
    }

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

//    val outputBeforeEnd: String = s"Life: ${input.hero.life} | ${dirReason.reason}" + (if (skipBoard) "" else s"\n\n$renderedBoard")
    val outputBeforeEnd: String = s"Life: ${input.hero.life} |  \n\n$renderedBoard"

    outputBeforeEnd
//    if (skipBoard) outputBeforeEnd
//    else if (input.game.turn == input.game.maxTurns - 3) {
//      outputBeforeEnd + "\33[1A" * (board.size + 3)
//    } else outputBeforeEnd
  }
}