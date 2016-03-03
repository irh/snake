module View (view) where

import Color exposing (Color, rgb)
import Debug
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Html exposing (..)
import Snake
import Text
import Window


cellSize = 16

backColor = rgb 0 100 0
snakeColor = rgb 100 200 100
headColor = rgb 60 160 60
foodColor = rgb 200 200 100
bonusColor = rgb 220 100 50
textColor = rgb 250 250 150

textHeight = 30


view : Signal.Address Snake.Action -> Snake.Model -> Html
view _ game =
  let
    width = Snake.gameWidth * cellSize
    height = Snake.gameHeight * cellSize
    background = rect width height
      |> filled backColor
    score = toString game.score
      |> styledText textColor
      |> move ((toFloat width / 2 - 30), (toFloat height / 2 - 20))
    bonusString = case game.bonus.point of
      Just _ ->
        let x = Snake.activeBonusTicks // 10
        in toString ((game.bonus.ticks + x - 1) // x)
      Nothing -> ""
    bonus = bonusString
      |> styledText bonusColor
      |> move ((toFloat 30 - width / 2), (toFloat height / 2 - 20))
  in
    ( case game.mode of
        Snake.NewGame -> (background :: gameText game)
        Snake.Pause -> (background :: score :: gameText game)
        Snake.GameOver -> (background :: score :: gameText game)
        _ -> (List.concat [background :: gameLayer width height game, [score, bonus]])
    )
    |> collage width height
    |> container (fst game.window) (snd game.window) middle
    |> Html.fromElement


styledText : Color -> String -> Form
styledText color string =
  Text.fromString string
  |> Text.style (textStyle color)
  |> Graphics.Collage.text


textStyle : Color -> Text.Style
textStyle color =
  { typeface = [ "monospace" ]
  , height = Just textHeight
  , color = color
  , bold = True
  , italic = False
  , line = Nothing
  }


gameText : Snake.Model -> List Form
gameText game =
  let (first, second) = case game.mode of
    Snake.NewGame -> ("SNAKE", "PRESS SPACE TO PLAY")
    Snake.Pause -> ("PAUSED", "PRESS SPACE TO CONTINUE")
    Snake.GameOver -> ("GAME OVER", "PRESS SPACE TO RETRY")
    _ -> ("", "")
  in
    [ styledText textColor first
      |> move (0, textHeight)
    , styledText textColor second
      |> move (0, -textHeight)
    ]


gameLayer : Int -> Int -> Snake.Model -> List Form
gameLayer width height game =
  let
    food = makeCell game.food width height foodColor
    bonus = case game.bonus.point of
      Just bonusPoint -> Just (makeCell bonusPoint width height bonusColor)
      Nothing -> Nothing
    head = makeCell (Snake.getHead game) width height headColor
    tail = List.map
      (\point -> makeCell point width height snakeColor)
      (Snake.getTail game)
    snake = case game.mode of
      Snake.Dead (count) ->
        if (count % (Snake.deathFlashCount * 2) >= Snake.deathFlashCount) then
          (head :: tail)
        else []
      _ -> (head :: tail)
  in
    case bonus of
      Just bonusCell -> food :: bonusCell :: snake
      Nothing -> food :: snake


makeCell : Snake.Point -> Int -> Int -> Color -> Form
makeCell point width height color =
  let
    offset = toFloat cellSize / 2
    x' = toFloat width / 2 - offset
    y' = toFloat height / 2 - offset
  in
    rect cellSize cellSize
      |> filled color
      |> move (toFloat point.x * cellSize - x', toFloat point.y * cellSize - y')
