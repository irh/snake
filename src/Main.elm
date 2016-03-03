module Main (main) where

import Effects
import Keyboard
import Primer
import Snake
import StartApp
import Task
import Time
import View
import Window


inputSignals =
  [ Signal.map Snake.Tick (Time.fps Snake.tickFps)
  , Signal.map Snake.Arrows Keyboard.arrows
  , Signal.map Snake.Wasd Keyboard.wasd
  , Signal.map Snake.Space Keyboard.space
  , Signal.map Snake.Window (Primer.prime Window.dimensions)
  ]

app =
  StartApp.start
    { init = Snake.initialGame
    , update = Snake.updateGame
    , view = View.view
    , inputs = inputSignals
    }

main =
  app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks

port playSound : Signal String
port playSound =
  Snake.soundSignal
