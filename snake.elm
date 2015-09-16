import Color exposing (..)
import Debug
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Random exposing (..)
import Text
import Time exposing (..)
import Trampoline exposing (..)
import Window


-- Values

(gameWidth, gameHeight) = (48, 27)
cellSize = 16

foodEnergy = 4

backColor = rgb 0 100 0
liveColor = rgb 100 200 100
headColor = rgb 60 160 60
deadColor = rgb 100 150 100
foodColor = rgb 200 200 100


-- Model

type State = Play | Dead | Pause

type Direction = Up | Right | Down | Left

type alias Point = { x : Int, y : Int }

type alias Game =
  { state : State
  , direction : Direction
  , arrows : Point
  , snake : List Point
  , length : Int
  , food : Point
  , seed : Seed
  }

type Update =
  Reset
  | Arrows Point
  | Tick Float
  | Space Bool



-- Update

defaultPoint : Point
defaultPoint = { x = 0, y = 0 }


randomPoint : Seed -> (Point, Seed)
randomPoint seed =
  let
    (x, seed0) = generate (Random.int 0 (gameWidth - 1)) seed
    (y, seed1) = generate (Random.int 0 (gameHeight - 1)) seed0
  in
    ({x = x, y = y}, seed1)


defaultGame : Game
defaultGame =
  { state = Pause
  , direction = Right
  , arrows = defaultPoint
  , snake = [ { x = gameWidth // 2, y = gameHeight // 2 } ]
  , length = 2
  , food = defaultPoint
  , seed = initialSeed 420
  }


gameOver : Game -> Game
gameOver game =
  { game | state <- Dead }


newGame : Game -> Game
newGame game =
  { defaultGame
  | seed <- game.seed
  , state <- Play
  }
  |> newFood


initialGame : Game
initialGame =
  defaultGame |> newFood


collisionTest : Point -> List Point -> Bool
collisionTest testPoint candidates =
  List.any (\point -> testPoint == point) candidates


newFood : Game -> Game
newFood game =
  let newFood' game =
    let
      (food, seed) = randomPoint game.seed
      game' = { game | seed <- seed, food <- food }
    in
      if collisionTest food game.snake then
        Continue (\() -> newFood' game')
      else
        Done game'
  in
    trampoline (newFood' game)


update : Update -> Game -> Game
update input game =
  case input of
    Reset -> newGame game
    Arrows (arrows) -> { game | arrows <- arrows }
    Tick (f) -> tickGame game
    Space (down) -> if down then changeGameState game else game


changeGameState : Game -> Game
changeGameState game =
  case game.state of
    Play -> { game | state <- Pause }
    Pause -> { game | state <- Play }
    Dead -> newGame game


tickGame : Game -> Game
tickGame game =
  case game.state of
    Play -> moveSnake game
    Pause -> game
    Dead -> game


moveSnake : Game -> Game
moveSnake game =
  let
    direction = changeDirection game
    newHead = moveHead game direction
  in
    if collisionTest newHead game.snake then gameOver game
    else
      let
        newSnake = (newHead :: game.snake)
        foodEaten = collisionTest game.food newSnake
        length = if foodEaten then (game.length + foodEnergy) else game.length
        result =
          { game
          | snake <- List.take length newSnake
          , direction <- direction
          , length <- length
          }
      in
        Debug.watchSummary "result" (\_ -> result.food) <|
        if foodEaten then (result |> newFood) else result


getHead : Game -> Point
getHead game =
  case List.head game.snake of
    Just (point) -> point
    Nothing -> defaultPoint


getTail : Game -> List Point
getTail game =
  case List.tail game.snake of
    Just (tail) -> tail
    Nothing -> []


moveHead : Game -> Direction -> Point
moveHead game direction =
  let
    oldHead = getHead game
    newHead = case direction of
      Up    -> { oldHead | y <- oldHead.y - 1 }
      Right -> { oldHead | x <- oldHead.x + 1 }
      Down  -> { oldHead | y <- oldHead.y + 1 }
      Left  -> { oldHead | x <- oldHead.x - 1 }
  in
    wrapPoint newHead


wrapPoint : Point -> Point
wrapPoint input =
  { x = wrapInt input.x 0 gameWidth
  , y = wrapInt input.y 0 gameHeight
  }


wrapInt : Int -> Int -> Int -> Int
wrapInt input min max =
  let
    size = max - min
  in
    if input < min then input + size
    else if input >= max then input - size
    else input


changeDirection : Game -> Direction
changeDirection game =
  let
    x = game.arrows.x
    y = game.arrows.y
  in
    if x > 0 && game.direction /= Left then Right
    else if x < 0 && game.direction /= Right then Left
    else if y < 0 && game.direction /= Down then Up
    else if y > 0 && game.direction /= Up then Down
    else game.direction


-- View

view : (Int, Int) -> Game -> Element
view (w, h) game =
  let
    width = gameWidth * cellSize
    height = gameHeight * cellSize
    background = rect width height |> filled backColor
    food = makeCell game.food width height foodColor
    head = makeCell (getHead game) width height headColor
    snakeColor = if game.state == Dead then deadColor else liveColor
    snake = List.map
      (\point -> makeCell point width height snakeColor)
      (getTail game)
  in
    container w h middle <|
      collage width height
      (background
      :: food
      :: head
      :: snake
      )


makeCell : Point -> Int -> Int -> Color -> Form
makeCell point width height color =
  let
    offset = toFloat cellSize / 2
    x' = toFloat width / 2 - offset
    y' = toFloat height / 2 - offset
  in
    rect cellSize cellSize
      |> filled color
      |> move (toFloat point.x * cellSize - x', toFloat point.y * cellSize - y')


-- Signals

updateSignal : Signal Update
updateSignal =
  Signal.mergeMany
  [ Signal.map Tick (fps 20)
  , Signal.map Arrows Keyboard.arrows
  , Signal.map Space Keyboard.space
  ]


gameSignal : Signal Game
gameSignal =
  Signal.foldp update initialGame updateSignal


main : Signal Element
main =
  Signal.map2 view Window.dimensions gameSignal

