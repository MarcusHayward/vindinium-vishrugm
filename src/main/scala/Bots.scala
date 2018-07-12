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
        Pos(t._2 / input.game.board.size, t._2 % input.game.board.size)
      )
    }

    def weightOfMine: Double = {
      1
    }

    def getWeight(life: Int): Double = {
      val e = 2.71828
      val modifier = 50d //the higher this is, the more likely I am to go to a tavern

      Math.pow(e, life / modifier) - 1
    }

    def weightOfTaverns: Double = getWeight(input.hero.life)


    def convertTileWithPositionToWeightedTile(tp: TileWithPosition): WeightedTile =
      tp match {
        case TileWithPosition(Tavern, position) => WeightedTile(weightOfTaverns, Tavern, position)
        case TileWithPosition(Mine(id), position) => WeightedTile(weightOfMine, Mine(id), position)
        case TileWithPosition(a, b) => WeightedTile(1 , a, b)
      }

    var weightedMap: Seq[WeightedTile] = mapWithCoordinates.map(convertTileWithPositionToWeightedTile)

    def heuristicBetween(start: Pos, end: WeightedTile): Double = {
      val h = (Math.abs(start.x - end.position.x) + Math.abs(start.y - end.position.y)) * end.weight

      weightedMap = weightedMap.map((wt: WeightedTile) => wt.position match {
        case end.position => wt.copy(weight = h)
        case _ => wt
      })

      h
    }

    val enemies: Seq[Hero] = input.game.heroes filterNot (_.id == input.hero.id)
    val firstEnemy: Hero = enemies.head

    def findPath(heroPosition: Pos, destinations: Seq[WeightedTile], board: Board): Option[PathNode] = {
      val startNodes: Seq[PathNode] = destinations.map((weightedPos: WeightedTile) => PathNode(weightedPos, heuristicBetween(heroPosition, weightedPos), 0, None))
      val visited: Seq[PathNode] = startNodes

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
            val neighbours = bestNode.weightedTile.position.neighbors.collect {
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

      loop(startNodes.toSet, visited.toSet)
    }

    def findTakeableMines(hero: Hero, map: Seq[WeightedTile]): Seq[WeightedTile] = {
      val takeableMines = map.collect {
        case t: WeightedTile if isMineTakeable(t, hero) => t
      }

      takeableMines.filter((mine: WeightedTile) => {
        findPath(hero.pos, Seq(WeightedTile(1d, mine.tile, mine.position)), input.game.board).map(p => {
          hero.life - p.g > 20
        }).getOrElse(false)
      })
    }

    def isMineTakeable(tileWithPosition: WeightedTile, hero: Hero): Boolean = tileWithPosition match {
      case WeightedTile(_, Mine(Some(hero)), _) => false
      case WeightedTile(_, Mine(_), _) => true
      case _ => false
    }

    val taverns: Seq[WeightedTile] = weightedMap.filter(_.tile == Tavern)

    val minesPositions: Seq[WeightedTile] = findTakeableMines(input.hero, weightedMap)

    val weightedPositions: Seq[WeightedTile] = taverns ++ minesPositions

    def goFor(possibleDestinations: Seq[WeightedTile]): Dir = {
      val path: Option[PathNode] = findPath(input.hero.pos, possibleDestinations, input.game.board)
      val neighbors: Seq[(Pos, bot.Dir.Value)] =
        List(North, South, West, East) map (x => (input.hero.pos.to(x), x))

      def getDirectionForPos(neighbors: Seq[(Pos, Dir)], pos: Pos): Dir =
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