package bot

import Dir._

import scala.util.Random
import Tile._

import scala.annotation.tailrec

trait Bot {
  def move(input: Input): Dir
}

final case class TileWithPosition(tile: Tile, position: Pos);

class LondonBot extends Bot {
  override def move(input: Input): Dir = {
    case class PathNode(pos: Pos, h: Int, g: Int, parent: Option[PathNode] ) {
      val score: Int = g + h
    }

    println(s"turn ${input.game.turn} with ${input.hero.life}")

    val mapWithCoordinates: Vector[TileWithPosition] = input.game.board.tiles.zipWithIndex.map { t: (Tile, Int) =>
      TileWithPosition(
        t._1,
        Pos(t._2 / input.game.board.size, t._2 % input.game.board.size))}

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

      @tailrec
      def loop(open: Set[PathNode], visited: Set[PathNode]): Option[PathNode] = {
        if (open.isEmpty) {
          println("pathfinding: Nowhere to go")
          None
        } else {
          val bestNode: PathNode = open.minBy(_.score)

          if (bestNode.pos == end) {
            println("pathfinding: Reached destination")
            Some(bestNode)
          } else {
            val neighbours: Set[PathNode] = bestNode.pos.neighbors.collect {
              case p: Pos if board.at(p).exists(Air ==) =>
                println("pathfinding: neighbour is an open node")
                PathNode(p, heuristicBetween(p, end), bestNode.g + 1, Some(bestNode))
              case p: Pos if p == end =>
                println("pathfind: neighbour is end tile")
                PathNode(p, heuristicBetween(p, end), bestNode.g + 1, Some(bestNode))
            }.diff(visited)

            println("pathfinding: going for another round")
            loop(open ++ neighbours - bestNode, visited + bestNode)
          }
        }
      }

      loop(Set(startNode), visited)
    }

    // convert board to list of positioned tiles
    val size: Int = input.game.board.size

    def convertBoardToPositionedTiles() = ???

    // 1. Move towards enemy with most mines and less health than us
    // 2. Otherwise, move towards any enemy with less health than us
    // 3. Otherwise, go for the closest mine
    def findTargetPosition(map: Vector[TileWithPosition]): Pos = {
      val enemies: Seq[Hero] = input.game.heroes filterNot { _.id == input.hero.id }
      println(enemies)
      println(enemies.map { _.life })
      val unhealthyEnemies: Seq[Hero] = enemies.filter(_.life < input.hero.life)
      println(unhealthyEnemies)

      if (unhealthyEnemies.isEmpty) {
        println("found no enemies, going for the pub")
        map.find { _.tile == Tavern }.map(_.position).getOrElse(enemies.head.pos)
      } else {
        println("found a weak enemy!!!")
        unhealthyEnemies.maxBy(_.mineCount).pos
      }
    }

    @tailrec
    def findNext(path: PathNode): Pos = path match {
      case PathNode(p, _,_ , None) => ??? // boom crash bash!
      case PathNode(p, _,_ , Some(PathNode(_,_ , _, None))) => p
      case PathNode(_,_ , _, Some(x)) => findNext(x)
    }

    val path: Option[PathNode] = findPath(input.hero.pos, findTargetPosition(mapWithCoordinates), input.game.board)
    val neighbors = Set(North, South, West, East) map (x => (input.hero.pos.to(x), x))
    val nextPos: Option[Pos] = path.map(findNext)

    val destination = neighbors.find(n => nextPos.exists(n._1 ==)).map(_._2).getOrElse(Stay)
    println(s"moving towards ${destination}")
    destination
  }
}
class RandomBot extends Bot {

  def move(input: Input) = {
    Random.shuffle(List(Dir.North, Dir.South, Dir.East, Dir.West)) find { dir ⇒
      input.game.board at input.hero.pos.to(dir) exists (Wall!=)
    }
  } getOrElse Dir.Stay
}
