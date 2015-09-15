import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import Random exposing (..)
import Signal exposing (..)
import Time exposing (..)
import Date exposing (..)
import List exposing (..)
import Mouse exposing (..)

--main :  Element
main = scene <~ random_walker_trace

-- Random
randomInt seed = 
  let (num, _) = generate (int 0 4) seed
  in num
initialSeeds = 
  (\(time) -> Random.initialSeed (round time)) <~ (foldp (+) 0 (fps 200))
randomChoice = randomInt <~ initialSeeds

walk : Int -> (Float, Float)  -> (Float, Float)
walk choice (x, y) =
  if choice == 0 then (x + 3, y)
  else if choice == 1 then (x - 3, y)
  else if choice == 2 then (x, y + 3)
  else if choice == 3 then (x, y - 3)
  else (x, y)

random_walker_trace : Signal (List (Float, Float))
random_walker_trace = foldp walk (0,0) randomChoice |> foldp (::) []

display : (Float, Float) -> a -> Element
display pos test = collage 600 600 [dotForm pos]

dotForm : (Float, Float) -> Form
dotForm pos = filled blue (ngon 4 1) |> move pos |> rotate (degrees 45)


scene : List (Float,Float) -> Element
scene pos =
    layers
    [ collage 600 600 (List.map dotForm pos)
    ]
        

