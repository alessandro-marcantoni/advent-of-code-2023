import utils.Utils.readLines

object D02_CubeConundrum {
  private case class Game(id: Int, draws: List[Draw])
  private type Draw = List[Cubes]
  private case class Cubes(quantity: Int, cube: Cube)
  private sealed trait Cube
  private case object Red extends Cube
  private case object Green extends Cube
  private case object Blue extends Cube

  private def cube(color: String): Cube = color match {
    case "red"   => Red
    case "green" => Green
    case "blue"  => Blue
  }

  private def parseInput(input: List[String]): List[Game] = {
    input
      .map(_.split(':').map(_.trim).toList)
      .map { case gamePart :: drawsPart :: _ => (gamePart.replace("Game ", "").toInt, drawsPart) }
      .map { case (gameId, drawsPart) => (gameId, drawsPart.split(';').map(_.trim).toList) }
      .map { case (gameId, draws) => (gameId, draws.map(_.split(',').map(_.trim).toList)) }
      .map { case (gameId, cubes) => (gameId, cubes.map(_.map(_.split(' ').map(_.trim).toList))) }
      .map { case (gameId, pieces) => (gameId, pieces.map(_.map(cubes => Cubes(cubes.head.toInt, cube(cubes.last))))) }
      .map { case (gameId, cubes) => Game(gameId, cubes) }
  }

  private def isGamePossible(game: Game, config: Draw): Boolean = game.draws.forall(draw =>
    draw.forall { case Cubes(quantity, cube) =>
      config.forall {
        case Cubes(configQuantity, configCube) if configCube == cube =>
          quantity <= configQuantity
        case _ => true
      }
    }
  )

  private def gamePower(game: Game): Int = game.draws.flatten
    .groupBy(_.cube)
    .map(cubes => cubes._1 -> cubes._2.maxBy(_.quantity).quantity)
    .values
    .product

  private def solveFirst(games: List[Game], config: Draw): Int = games
    .filter(isGamePossible(_, config))
    .map { case Game(id, _) => id }
    .sum

  private def solveSecond(games: List[Game]): Int = games
    .map(gamePower)
    .sum

  def main(args: Array[String]): Unit = {
    val games = parseInput(readLines("02-cube-conundrum.txt"))
    println(solveFirst(games, List(Cubes(12, Red), Cubes(13, Green), Cubes(14, Blue))))
    println(solveSecond(games))
  }
}
