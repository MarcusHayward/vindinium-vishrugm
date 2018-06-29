package bot

import Dir._

import scala.util.Random
import Tile._

import scala.annotation.tailrec

trait Bot {
  def move(input: Input): Dir
}

final case class TileWithPosition(tile: Tile, position: Pos)
final case class WeightedTile(weight: Double, tile: Tile, position: Pos)

class LondonBot extends Bot {
  override def move(input: Input): Dir = {
    case class PathNode(weightedTile: WeightedTile, h: Double, g: Int, parent: Option[PathNode]) {
      val score: Double = g + h
    }

    val mapWithCoordinates: Vector[TileWithPosition] = input.game.board.tiles.zipWithIndex.map { t: (Tile, Int) =>
      TileWithPosition(
        t._1,
        Pos(t._2 / input.game.board.size, t._2 % input.game.board.size))
    }

    def heuristicBetween(start: Pos, end: WeightedTile): Double =
      Math.abs(start.x - end.position.x) + Math.abs(start.y - end.position.y) / end.weight

    val enemies: Seq[Hero] = input.game.heroes filterNot (_.id == input.hero.id)
    val firstEnemy: Hero = enemies.head

    def findPath(heroPosition: Pos, destinations: Set[WeightedTile], board: Board): Option[PathNode] = {
      val startNodes: Set[PathNode] = destinations.map((weightedPos: WeightedTile) => PathNode(weightedPos, heuristicBetween(heroPosition, weightedPos), 0, None))
      val visited: Set[PathNode] = startNodes

      @tailrec
      def loop(open: Set[PathNode], visited: Set[PathNode]): Option[PathNode] = {
        if (open.isEmpty) {
          println("pathfinding: Nowhere to go")
          None
        } else {
          val bestNode: PathNode = open.minBy(_.score)

          if (bestNode.weightedTile.position == heroPosition) {
            Some(bestNode)
          } else {
            val neighbours: Set[PathNode] = bestNode.weightedTile.position.neighbors.collect {
              case p: Pos if board.at(p).exists(Air ==) =>
                PathNode(WeightedTile(bestNode.weightedTile.weight, input.game.board.at(p).get, p), heuristicBetween(heroPosition, WeightedTile(bestNode.weightedTile.weight, bestNode.weightedTile.tile, p)), bestNode.g + 1, Some(bestNode))
              case p: Pos if p == heroPosition =>
                PathNode(WeightedTile(bestNode.weightedTile.weight, input.game.board.at(p).get, p), heuristicBetween(heroPosition, WeightedTile(bestNode.weightedTile.weight, bestNode.weightedTile.tile, p)), bestNode.g + 1, Some(bestNode))
            }.diff(visited)

            val bestNeighbourAndOpenNode = neighbours.map(neighbour => {
              val existingNeighbour: Option[PathNode] = open.collectFirst {
                case p: PathNode if p.weightedTile.position == neighbour.weightedTile.position => p
              }
              (neighbour, existingNeighbour)
            }).filter { case (n, e) => e.forall(_.score > n.score) }

            val bestNeighbours = bestNeighbourAndOpenNode.map(_._1)

            val worstExistingNeighbours: Set[PathNode] = bestNeighbourAndOpenNode.flatMap(_._2)

            loop(open ++ bestNeighbours -- worstExistingNeighbours - bestNode, visited + bestNode)
          }
        }
      }

      loop(startNodes, visited)
    }

    def weightOfMine: Double = {
      1
    }

    def weightOfTaverns: Double = ((input.hero.life + 20) / 100d)

    def findTakeableMines(hero: Hero, map: Set[WeightedTile]): Set[WeightedTile] = {
      val takeableMines = map.collect {
        case t: WeightedTile if isMineTakeable(t, hero) => t
      }

      takeableMines.filter((mine: WeightedTile) => {
        findPath(hero.pos, Set(WeightedTile(1d, mine.tile, mine.position)), input.game.board).map(p => {
          hero.life - p.g > 20
        }).getOrElse(false)
      })
    }

    def isMineTakeable(tileWithPosition: WeightedTile, hero: Hero): Boolean = tileWithPosition match {
      case WeightedTile(_, Mine(Some(hero)), _) => false
      case WeightedTile(_, Mine(_), _) => true
      case _ => false
    }

    def convertTileWithPositionToWeightedTile(tp: TileWithPosition): WeightedTile =
      tp match {
        case TileWithPosition(Tavern, position) => WeightedTile(weightOfTaverns, Tavern, position)
        case TileWithPosition(Mine(id), position) => WeightedTile(weightOfMine, Mine(id), position)
        case TileWithPosition(a, b) => WeightedTile(1, a, b)
      }


    val weightedMap: Set[WeightedTile] = mapWithCoordinates.map(convertTileWithPositionToWeightedTile).toSet

    val taverns: Set[WeightedTile] = weightedMap.filter(_.tile == Tavern)

    val minesPositions: Set[WeightedTile] = findTakeableMines(input.hero, weightedMap).toSet

    val weightedPositions: Set[WeightedTile] = taverns ++ minesPositions

    def goFor(possibleDestinations: Set[WeightedTile]): Dir = {
      val path: Option[PathNode] = findPath(input.hero.pos, possibleDestinations, input.game.board)
      val neighbors: Set[(Pos, bot.Dir.Value)] =
        Set(North, South, West, East) map (x => (input.hero.pos.to(x), x))

      def getDirectionForPos(neighbors: Set[(Pos, Dir)], pos: Pos): Dir =
        neighbors.find(n => pos == n._1).map(_._2).getOrElse(Stay)

      val destination: Option[Dir] = for {
        pathNode <- path
        parent <- pathNode.parent
      } yield getDirectionForPos(neighbors, parent.weightedTile.position)

      destination getOrElse Stay
    }

    val clearString = "\033[H\033[2J"
    print(clearString)

    print(Renderer.renderBoard(weightedMap, input))

    goFor(weightedPositions)
  }
}


class RandomBot extends Bot {

  def move(input: Input) = {
    Random.shuffle(List(Dir.North, Dir.South, Dir.East, Dir.West)) find { dir ⇒
      input.game.board at input.hero.pos.to(dir) exists (Wall !=)
    }
  } getOrElse Dir.Stay
}