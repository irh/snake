module Snake where

import Debug
import Random exposing(Seed)
import Trampoline


(gameWidth, gameHeight) = (48, 27)

foodEnergy = 4
foodScore = 1
bonusScore = 5

tickFps = 20
deathTicks = 32
deathFlashCount = deathTicks // 4

activeBonusTicks = 50
minTicksToNextBonus = 200
maxTicksToNextBonus = 400


type Mode =
  NewGame
  | Play
  | Pause
  | Dead Int
  | GameOver

type Direction = Up | Right | Down | Left

type alias Point = { x : Int, y : Int }

type alias Bonus =
  { point : Maybe Point
  , ticks : Int
  }

type alias Model =
  { mode : Mode
  , direction : Direction
  , arrows : Point
  , snake : List Point
  , length : Int
  , food : Point
  , bonus : Bonus
  , seed : Seed
  , score : Int
  }

type Update =
  Reset
  | Arrows Point
  | Wasd Point
  | Tick Float
  | Space Bool


defaultPoint : Point
defaultPoint = { x = 0, y = 0 }


defaultBonus : Bonus
defaultBonus = { point = Nothing, ticks = -1 }


defaultGame : Model
defaultGame =
  { mode = NewGame
  , direction = Right
  , arrows = defaultPoint
  , snake = [ { x = gameWidth // 2, y = gameHeight // 2 } ]
  , length = 2
  , food = defaultPoint
  , bonus = defaultBonus
  , seed = Random.initialSeed 420
  , score = 0
  }


newGame : Model -> Model
newGame game =
  { defaultGame
  | seed <- game.seed
  , mode <- Play
  }
  |> newFood
  |> resetBonus


initialGame : Model
initialGame =
  defaultGame |> newFood |> resetBonus


collisionTest : Point -> List Point -> Bool
collisionTest testPoint candidates =
  List.any (\point -> testPoint == point) candidates


randomInt : Int -> Int -> Seed -> (Int, Seed)
randomInt min max seed =
  Random.generate (Random.int min max) seed


randomPoint : Seed -> (Point, Seed)
randomPoint seed =
  let
    (x, seed0) = randomInt 0 (gameWidth - 1) seed
    (y, seed1) = randomInt 0 (gameHeight - 1) seed0
  in
    ({x = x, y = y}, seed1)


newFood : Model -> Model
newFood game =
  let newFood' game =
    let
      (food, seed) = randomPoint game.seed
      game' = { game | seed <- seed, food <- food }
      bonusCollision = case game.bonus.point of
        Just bonusPoint -> food == bonusPoint
        Nothing -> False
    in
      if bonusCollision || collisionTest food game.snake then
        Trampoline.Continue (\() -> newFood' game')
      else
        Trampoline.Done game'
  in Trampoline.trampoline (newFood' game)


newBonus : Model -> Model
newBonus game =
  let newBonus' game =
    let
      (bonus, seed) = randomPoint game.seed
      game' =
        { game
        | seed <- seed
        , bonus <-
          { point = Just bonus
          , ticks = activeBonusTicks
          }
        }
      foodCollision = bonus == game.food
    in
      if foodCollision || collisionTest bonus game.snake then
        Trampoline.Continue (\() -> newBonus' game')
      else
        Trampoline.Done game'
  in Trampoline.trampoline (newBonus' game)


resetBonus : Model -> Model
resetBonus game =
  let
    (ticks, seed') = randomInt minTicksToNextBonus maxTicksToNextBonus game.seed
  in
    { game
    | seed <- seed'
    , bonus <-
      { point = Nothing
      , ticks = ticks
      }
    }


update : Update -> Model -> Model
update input game =
  case input of
    Reset -> newGame game
    Arrows (arrows) -> { game | arrows <- arrows }
    Wasd (wasd) -> { game | arrows <- wasd }
    Tick _ -> tickGame game
    Space (down) -> if down then changeGameMode game else game


changeGameMode : Model -> Model
changeGameMode game =
  case game.mode of
    NewGame -> newGame game
    Play -> { game | mode <- Pause }
    Pause -> { game | mode <- Play }
    Dead _ -> game
    GameOver -> newGame game


tickGame : Model -> Model
tickGame game =
  Debug.watchSummary "game" (\_ -> game.mode) <|
  case game.mode of
    Play -> tickPlay game
    Dead (count) -> tickDead game count
    _ -> game


tickDead : Model -> Int -> Model
tickDead game count =
  let
    nextCount = count + 1
  in
    if nextCount == deathTicks then
      { game | mode <- GameOver }
    else
      { game | mode <- Dead nextCount }


tickPlay : Model -> Model
tickPlay game =
  let
    direction' = changeDirection game
    head' = moveHead game direction'
  in
    if collisionTest head' game.snake then
      { game | mode <- Dead 0 }
    else
      { game
      | snake <- head' :: game.snake
      , direction <- direction'
      }
      |> tickBonus
      |> tickFood
      |> tickSnake


tickSnake : Model -> Model
tickSnake game =
  { game | snake <- List.take game.length game.snake }


tickFood : Model -> Model
tickFood game =
  if collisionTest game.food game.snake then
    { game
    | length <- game.length + foodEnergy
    , score <- game.score + foodScore
    }
    |> newFood
  else game


tickBonus : Model -> Model
tickBonus game =
  case game.bonus.point of
    Just point ->
      if collisionTest point game.snake then
        { game
        | length <- game.length + foodEnergy
        , score <- game.score + bonusScore
        }
        |> resetBonus
      else
        let ticks' = game.bonus.ticks - 1
        in
          if ticks' == 0 then
            game |> resetBonus
          else
            { game | bonus <- { point = game.bonus.point, ticks = ticks' } }
    Nothing ->
      let ticks' = game.bonus.ticks - 1
      in
        if ticks' == 0 then
          game |> newBonus
        else
          { game | bonus <- { point = game.bonus.point, ticks = ticks' } }


getHead : Model -> Point
getHead game =
  case List.head game.snake of
    Just (point) -> point
    Nothing -> defaultPoint


getTail : Model -> List Point
getTail game =
  case List.tail game.snake of
    Just (tail) -> tail
    Nothing -> []


moveHead : Model -> Direction -> Point
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
    if | input < min -> input + size
       | input >= max -> input - size
       | otherwise -> input


changeDirection : Model -> Direction
changeDirection game =
  let
    x = game.arrows.x
    y = game.arrows.y
  in
    if | x > 0 && game.direction /= Left -> Right
       | x < 0 && game.direction /= Right -> Left
       | y < 0 && game.direction /= Down -> Up
       | y > 0 && game.direction /= Up -> Down
       | otherwise -> game.direction
