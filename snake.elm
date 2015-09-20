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
foodScore = 1
bonusScore = 5

backColor = rgb 0 100 0
snakeColor = rgb 100 200 100
headColor = rgb 60 160 60
foodColor = rgb 200 200 100
bonusColor = rgb 220 100 50
textColor = rgb 250 250 150

tickFps = 20
deathTicks = 32
deathFlashCount = deathTicks // 4

activeBonusTicks = 60
minTicksToNextBonus = 200
maxTicksToNextBonus = 400

textHeight = 30

textStyle : Color -> Text.Style
textStyle color =
  { typeface = [ "monospace" ]
  , height = Just textHeight
  , color = color
  , bold = True
  , italic = False
  , line = Nothing
  }


-- Model

type State =
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

type alias Game =
  { state : State
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
  | Tick Float
  | Space Bool


-- Update

defaultPoint : Point
defaultPoint = { x = 0, y = 0 }


defaultBonus : Bonus
defaultBonus = { point = Nothing, ticks = -1 }


defaultGame : Game
defaultGame =
  { state = NewGame
  , direction = Right
  , arrows = defaultPoint
  , snake = [ { x = gameWidth // 2, y = gameHeight // 2 } ]
  , length = 2
  , food = defaultPoint
  , bonus = defaultBonus
  , seed = initialSeed 420
  , score = 0
  }


newGame : Game -> Game
newGame game =
  { defaultGame
  | seed <- game.seed
  , state <- Play
  }
  |> newFood
  |> resetBonus


initialGame : Game
initialGame =
  defaultGame |> newFood |> resetBonus


collisionTest : Point -> List Point -> Bool
collisionTest testPoint candidates =
  List.any (\point -> testPoint == point) candidates


randomPoint : Seed -> (Point, Seed)
randomPoint seed =
  let
    (x, seed0) = generate (Random.int 0 (gameWidth - 1)) seed
    (y, seed1) = generate (Random.int 0 (gameHeight - 1)) seed0
  in
    ({x = x, y = y}, seed1)


newFood : Game -> Game
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
        Continue (\() -> newFood' game')
      else
        Done game'
  in trampoline (newFood' game)


newBonus : Game -> Game
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
        Continue (\() -> newBonus' game')
      else
        Done game'
  in trampoline (newBonus' game)


resetBonus : Game -> Game
resetBonus game =
  let
    (ticks, seed') =
      generate (Random.int minTicksToNextBonus maxTicksToNextBonus) game.seed
  in
    { game
    | seed <- seed'
    , bonus <-
      { point = Nothing
      , ticks = ticks
      }
    }


update : Update -> Game -> Game
update input game =
  case input of
    Reset -> newGame game
    Arrows (arrows) -> { game | arrows <- arrows }
    Tick _ -> tickGame game
    Space (down) -> if down then changeGameState game else game


changeGameState : Game -> Game
changeGameState game =
  case game.state of
    NewGame -> newGame game
    Play -> { game | state <- Pause }
    Pause -> { game | state <- Play }
    Dead _ -> game
    GameOver -> newGame game


tickGame : Game -> Game
tickGame game =
  Debug.watchSummary "game" (\_ -> game.state) <|
  case game.state of
    Play -> tickPlay game
    Dead (count) -> tickDead game count
    _ -> game


tickDead : Game -> Int -> Game
tickDead game count =
  let
    nextCount = count + 1
  in
    if nextCount == deathTicks then
      { game | state <- GameOver }
    else
      { game | state <- Dead nextCount }


tickPlay : Game -> Game
tickPlay game =
  let
    direction' = changeDirection game
    head' = moveHead game direction'
  in
    if collisionTest head' game.snake then
      { game | state <- Dead 0 }
    else
      { game
      | snake <- head' :: game.snake
      , direction <- direction'
      }
      |> tickBonus
      |> tickFood
      |> tickSnake


tickSnake : Game -> Game
tickSnake game =
  { game
  | snake <- List.take game.length game.snake
  }


tickFood : Game -> Game
tickFood game =
  if collisionTest game.food game.snake then
    { game
    | length <- game.length + foodEnergy
    , score <- game.score + foodScore
    }
    |> newFood
  else game


tickBonus : Game -> Game
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
    background = rect width height
      |> filled backColor
    score = toString game.score
      |> styledText textColor
      |> move ((toFloat width / 2 - 30), (toFloat height / 2 - 20))
    bonusString = case game.bonus.point of
      Just _ ->
        let x = activeBonusTicks // 10
        in toString ((game.bonus.ticks + x - 1) // x)
      Nothing -> ""
    bonus = bonusString
      |> styledText bonusColor
      |> move ((toFloat 30 - width / 2), (toFloat height / 2 - 20))
  in
    container w h middle <|
      collage width height
      (case game.state of
        NewGame -> (background :: gameText game)
        Pause -> (background :: score :: gameText game)
        GameOver -> (background :: score :: gameText game)
        _ -> (List.concat [[background], gameLayer width height game, [score, bonus]])
      )


styledText : Color -> String -> Form
styledText color string =
  Text.fromString string
  |> Text.style ( textStyle color )
  |> Graphics.Collage.text


gameText : Game -> List Form
gameText game =
  let (first, second) = case game.state of
    NewGame -> ("SNAKE", "PRESS SPACE TO PLAY")
    Pause -> ("PAUSED", "PRESS SPACE TO CONTINUE")
    GameOver -> ("GAME OVER", "PRESS SPACE TO RETRY")
    _ -> ("", "")
  in
    [ styledText textColor first
      |> move (0, textHeight)
    , styledText textColor second
      |> move (0, -textHeight)
    ]


gameLayer : Int -> Int -> Game -> List Form
gameLayer width height game =
  let
    food = makeCell game.food width height foodColor
    bonus = case game.bonus.point of
      Just bonusPoint -> Just (makeCell bonusPoint width height bonusColor)
      Nothing -> Nothing
    head = makeCell (getHead game) width height headColor
    tail = List.map
      (\point -> makeCell point width height snakeColor)
      (getTail game)
    snake = case game.state of
      Dead (count) ->
        if (count % (deathFlashCount * 2) >= deathFlashCount) then
          (head :: tail)
        else
          []
      _ -> (head :: tail)
  in
    case bonus of
      Just bonusCell -> food :: bonusCell :: snake
      Nothing -> food :: snake


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
  [ Signal.map Tick (fps tickFps)
  , Signal.map Arrows Keyboard.arrows
  , Signal.map Space Keyboard.space
  ]


gameSignal : Signal Game
gameSignal =
  Signal.foldp update initialGame updateSignal


main : Signal Element
main =
  Signal.map2 view Window.dimensions gameSignal

