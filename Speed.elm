import Keyboard
import Window


type V = {x : Float, y : Float , z : Float}
type State = {position : V, speed : V, accel : V}

m : State
m = {position = {x = 0, y = 0, z = 0}, speed = {x = 0, y = 0, z = 0}, accel = {x = 0, y = 0, z = 0}}

addV : V -> V -> V
addV v1 v2 = {x = v1.x + v2.x, y = v1.y + v2.y, z = v1.z + v2.z }

-- Update
update : (Time, {x: Int, y: Int}) -> State -> State
update (t, {x,y}) state =
   let
        newAccel : V
        newAccel = {x = toFloat x, y = toFloat y, z = 0}
        newSpeed = addV newAccel state.speed
        newPosition = addV state.position newSpeed
   in
        {state | speed <- newSpeed, position <- newPosition, accel <- newAccel }

showV : String -> V -> Element
showV label score =
    let
        g = group [rect 200 50 |> filled lightGrey,
                   toForm (flow down [plainText label,
                                      show score |> toText |> rightAligned])]
    in
        collage 200 50 [g]

display : State -> Element
display m = collage 400 400 [ circle 10 |> filled blue |> move (m.position.x, m.position.y) ]

render : (Int, Int) -> State -> Element
render (w,h) m = container w h middle (flow down [flow right [showV "Position" m.position,
                                                              showV "Speed" m.speed,
                                                              showV "Accel" m.accel],
                                                 display m])
input =
  lift2 (,)
        (foldp (\t acc -> acc + (t/100)) 0 (fps 24))
        Keyboard.arrows
-- Main
main = lift2 render Window.dimensions <| foldp update m input
