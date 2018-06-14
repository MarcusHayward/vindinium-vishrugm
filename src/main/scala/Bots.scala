package bot

import Dir._

import scala.util.Random
import Tile._

import scala.annotation.tailrec

trait Bot {
  def move(input: Input): Dir
}

final case class TileWithPosition(tile: Tile, position: Pos)

class LondonBot extends Bot {
  override def move(input: Input): Dir = {
    case class PathNode(weightedPos: WeightedPosition, h: Double, g: Int, parent: Option[PathNode]) {
      val score: Double = g + h
    }

    val mapWithCoordinates: Vector[TileWithPosition] = input.game.board.tiles.zipWithIndex.map { t: (Tile, Int) =>
      TileWithPosition(
        t._1,
        Pos(t._2 / input.game.board.size, t._2 % input.game.board.size))
    }

    def heuristicBetween(start: Pos, end: WeightedPosition): Double =
      Math.abs(start.x - end.position.x) + Math.abs(start.y - end.position.y) / end.weight

    val enemies: Seq[Hero] = input.game.heroes filterNot (_.id == input.hero.id)
    val firstEnemy: Hero = enemies.head

    def findPath(heroPosition: Pos, destinations: Set[WeightedPosition], board: Board): Option[PathNode] = {
      val startNodes: Set[PathNode] = destinations.map((weightedPos: WeightedPosition) => PathNode(weightedPos, heuristicBetween(heroPosition, weightedPos), 0, None))
      val visited: Set[PathNode] = startNodes

      @tailrec
      def loop(open: Set[PathNode], visited: Set[PathNode]): Option[PathNode] = {
        if (open.isEmpty) {
          println("pathfinding: Nowhere to go")
          None
        } else {
          val bestNode: PathNode = open.minBy(_.score)

          if (bestNode.weightedPos.position == heroPosition) {
            Some(bestNode)
          } else {
            val neighbours: Set[PathNode] = bestNode.weightedPos.position.neighbors.collect {
              case p: Pos if board.at(p).exists(Air ==) =>
                PathNode(WeightedPosition(bestNode.weightedPos.weight, p), heuristicBetween(heroPosition, WeightedPosition(bestNode.weightedPos.weight, p)), bestNode.g + 1, Some(bestNode))
              case p: Pos if p == heroPosition =>
                PathNode(WeightedPosition(bestNode.weightedPos.weight, p), heuristicBetween(heroPosition, WeightedPosition(bestNode.weightedPos.weight, p)), bestNode.g + 1, Some(bestNode))
            }.diff(visited)

            val bestNeighbours = neighbours.filter(neighbour => {
              val existingNeighbour: Option[PathNode] = open.find {
                case p: PathNode if p.weightedPos.position == neighbour.weightedPos.position => true
                case _ => false
              }
              existingNeighbour.map(p => { p.score > neighbour.score }).getOrElse(true)
            })

            val worstExistingNeighbours: Set[PathNode] = bestNeighbours.flatMap(neighbour => {
              open.find {
                case p: PathNode if p.weightedPos.position == neighbour.weightedPos.position => true
                case _ => false
              }
            })

            loop(open ++ bestNeighbours -- worstExistingNeighbours - bestNode, visited + bestNode)
          }
        }
      }

      loop(startNodes, visited)
    }

    case class WeightedPosition(weight: Double, position: Pos)

    def weightOfMine: Double = {
      1
    }

    def weightOfTaverns: Double = 1 - (input.hero.life - 1) / 100d

    def findTakeableMines(hero: Hero, map: Vector[TileWithPosition]): Vector[TileWithPosition] = {
      val takeableMines = map.collect {
        case t: TileWithPosition if isMineTakeable(t, hero) => t
      }

      takeableMines.filter((mine: TileWithPosition) => {
        findPath(hero.pos, Set(WeightedPosition(1d, mine.position)), input.game.board).map(p => {
          hero.life - p.g > 20
        }).getOrElse(false)
      })
    }

    def isMineTakeable(tileWithPosition: TileWithPosition, hero: Hero): Boolean = tileWithPosition match {
      case TileWithPosition(Mine(Some(hero)), _) => false
      case TileWithPosition(Mine(_), _) => true
      case _ => false
    }

    val taverns: Set[Pos] = mapWithCoordinates.collect {
      case TileWithPosition(Tavern, position) => position
    }.toSet

    val minesPositions: Set[Pos] = findTakeableMines(input.hero, mapWithCoordinates).map(_.position).toSet

    val weightedPositions: Set[WeightedPosition] = taverns.map(WeightedPosition(weightOfTaverns, _)) ++ minesPositions.map(WeightedPosition(weightOfMine, _))

    def goFor(possibleDestinations: Set[WeightedPosition]): Dir = {
      val path: Option[PathNode] = findPath(input.hero.pos, possibleDestinations, input.game.board)
      val neighbors: Set[(Pos, bot.Dir.Value)] =
        Set(North, South, West, East) map (x => (input.hero.pos.to(x), x))

      def getDirectionForPos(neighbors: Set[(Pos, Dir)], pos: Pos): Dir =
        neighbors.find(n => pos == n._1).map(_._2).getOrElse(Stay)

      val destination: Option[Dir] = for {
        pathNode <- path
        parent <- pathNode.parent
      } yield getDirectionForPos(neighbors, parent.weightedPos.position)

      destination getOrElse Stay
    }

    val clearString = "\033[H\033[2J"
    print(clearString)

    print(Renderer.renderBoard(mapWithCoordinates, input))

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