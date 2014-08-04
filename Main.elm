import Keyboard
import Window

delta : Signal Time
delta = inSeconds <~ fps 35

type Input = { up: Int, down: Int, delta: Time }

input = sampleOn delta (Input <~ lift .x Keyboard.arrows
                               ~ lift .y Keyboard.arrows
                               ~ delta)

type Object = { x: Float, y: Float }
type Game = { snake: Object }

initGame : Game
initGame = { snake = { x = 0, y = 0 } }

stepGame : Input -> Game -> Game
stepGame { up, down, delta } { snake } =
   { snake = { x = snake.x + 5*(toFloat up), y = snake.y + 5*(toFloat down) } }

gameState : Signal Game
gameState = foldp stepGame initGame input

displayObj : Object -> Shape -> Form
displayObj obj shape =
    move (obj.x, obj.y) (filled (rgb 60 200 60) shape)

display : (Int, Int) -> Game -> Element
display (w, h) { snake } =
    container w h middle <| collage 600 400
                [
                  filled grey (rect 600 400)
                , displayObj snake (rect 15 15)
                ]

main = lift2 display Window.dimensions gameState
