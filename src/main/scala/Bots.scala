package bot

import Dir._

import scala.util.Random
import Tile._

trait Bot {
  def move(input: Input): Dir
}

class LondonBot extends Bot {
  override def move(input: Input): Dir = {
    case class PathNode(pos: Pos, h: Int, g: Int, parent: Option[PathNode] ) {
      val score: Int = g + h
    }

    def heuristicBetween(start: Pos, end: Pos): Int =
      Math.abs(start.x - end.x) + Math.abs(start.y - end.y)


    // Implement A* to move towards a fixed position
    // Figure out closest mine
    // Use A* to move towards closest mine

    val enemies: Seq[Hero] = input.game.heroes filterNot(_.id == input.hero.id)
    val firstEnemy: Hero = enemies.head

    def findPath(start: Pos, end: Pos, board: Board): Option[PathNode] = {
      val startNode: PathNode = PathNode(start, heuristicBetween(start, end), 0, None)
      val visited: Set[PathNode] = Set(startNode)

      loop(Set(startNode), visited)

      def loop(open: Set[PathNode], visited: Set[PathNode]): Option[PathNode] = {
        if (open.isEmpty) None
        else {
          val bestNode: PathNode = open.minBy(_.score)

          if (bestNode.pos == end) Some(bestNode)
          else {
            val neighbours: Set[PathNode] = bestNode.pos.neighbors.collect {
              case p: Pos if board.at(p).exists(Air ==) =>
                PathNode(p, heuristicBetween(p, end), bestNode.g + 1, Some(bestNode))
            }.diff(visited)

            loop(open ++ neighbours, visited + bestNode)
          }
        }
      }
    }

    findPath(input.hero.pos, firstEnemy.pos, input.game.board)

    Stay
  }
}
class RandomBot extends Bot {

  def move(input: Input) = {
    Random.shuffle(List(Dir.North, Dir.South, Dir.East, Dir.West)) find { dir ⇒
      input.game.board at input.hero.pos.to(dir) exists (Wall!=)
    }
  } getOrElse Dir.Stay
}