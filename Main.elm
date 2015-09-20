module Main where

import Graphics.Element exposing (Element)
import Keyboard
import Snake
import Time
import View
import Window


-- Signals

updateSignal : Signal Snake.Update
updateSignal =
  Signal.mergeMany
  [ Signal.map Snake.Tick (Time.fps Snake.tickFps)
  , Signal.map Snake.Arrows Keyboard.arrows
  , Signal.map Snake.Space Keyboard.space
  ]


gameSignal : Signal Snake.Model
gameSignal =
  Signal.foldp Snake.update Snake.initialGame updateSignal


main : Signal Element
main =
  Signal.map2 View.view Window.dimensions gameSignal

