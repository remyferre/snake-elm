import Keyboard
import Window

snakeWidth = 15
maxWidth   = 600
maxHeight  = 400

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

stepSnake : Input -> Object -> Object
stepSnake { up, down, delta } { x, y } =
    { x = x + 5*(toFloat up), y = y + 5*(toFloat down) }

stepGame : Input -> Game -> Game
stepGame ({ up, down, delta } as input) { snake } =
    let hasLost = snake.x - snakeWidth / 2 < -(maxWidth  / 2) ||
                  snake.x + snakeWidth / 2 >   maxWidth  / 2  ||
                  snake.y - snakeWidth / 2 < -(maxHeight / 2) ||
                  snake.y + snakeWidth / 2 >   maxHeight / 2
    in
      if hasLost then initGame else { snake = stepSnake input snake }

gameState : Signal Game
gameState = foldp stepGame initGame input

displayObj : Object -> Shape -> Form
displayObj obj shape =
    move (obj.x, obj.y) (filled (rgb 60 200 60) shape)

display : (Int, Int) -> Game -> Element
display (w, h) { snake } =
    container w h middle <| collage maxWidth maxHeight
                [
                  filled grey (rect maxWidth maxHeight)
                , displayObj snake (rect snakeWidth snakeWidth)
                ]

main = lift2 display Window.dimensions gameState
