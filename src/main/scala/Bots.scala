package bot

import Dir._

import scala.util.Random
import Tile._

trait Bot {
  def move(input: Input): Dir
}

class LondonBot extends Bot {
  override def move(input: Input): Dir = {
    val heroPos = input.hero.pos

    val tiles: List[(Dir, Option[Tile])] = List(North, South, West, East).map(d => (d, input.game.board.at(heroPos.to(d))))

    val nextMove = tiles.collectFirst {
      case (dir, Some(Tavern)) => dir
      case (dir, Some(Mine(id))) if id.exists(_ == input.hero.id) => dir
      case (dir, Some(Air)) => dir
    }

    nextMove.getOrElse(Stay)
  }
}

class RandomBot extends Bot {

  def move(input: Input) = {
    Random.shuffle(List(Dir.North, Dir.South, Dir.East, Dir.West)) find { dir ⇒
      input.game.board at input.hero.pos.to(dir) exists (Wall!=)
    }
  } getOrElse Dir.Stay
}