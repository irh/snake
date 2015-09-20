module Main (main) where

import Graphics.Element exposing (Element)
import Keyboard
import Signal.Extra
import Signal.Time
import Snake
import Time
import View
import Window


-- Signals

updateSignal : Signal Snake.Update
updateSignal =
  Signal.mergeMany
  [ Signal.map Snake.StartTime Signal.Time.startTime
  , Signal.map Snake.Tick (Time.fps Snake.tickFps)
  , Signal.map Snake.Arrows Keyboard.arrows
  , Signal.map Snake.Wasd Keyboard.wasd
  , Signal.map Snake.Space Keyboard.space
  ]


gameSignal : Signal Snake.Model
gameSignal =
  Signal.Extra.foldp' Snake.updateGame Snake.initialGame updateSignal


main : Signal Element
main =
  Signal.map2 View.view Window.dimensions gameSignal

