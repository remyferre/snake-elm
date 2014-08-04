import Keyboard
import Window

snakeWidth = 15
maxWidth   = 600
maxHeight  = 400

delta : Signal Time
delta = inSeconds <~ fps 35

data Direction = Up | Down | Left | Right
direction : Int -> Direction
direction keyCode = case keyCode of
                      37 -> Left
                      38 -> Up
                      39 -> Right
                      40 -> Down

isArrow : Int -> Bool
isArrow k = k >= 37 && k <= 40

lastPressedArrow : Signal Int
lastPressedArrow = keepIf isArrow 37 Keyboard.lastPressed

type Input = { direction: Direction, delta: Time }

input = sampleOn delta (Input <~ lift direction lastPressedArrow
                               ~ delta)

type Object = { x: Float, y: Float }
type Game = { snake: Object }

initGame : Game
initGame = { snake = { x = 0, y = 0 } }

stepSnake : Input -> Object -> Object
stepSnake { direction, delta } { x, y } =
    let x' = x + toFloat 5 * case direction of
                               Left  -> -1
                               Right ->  1
                               _     ->  0
        y' = y + toFloat 5 * case direction of
                               Up   ->  1
                               Down -> -1
                               _    ->  0
    in
      { x = x', y = y' }

stepGame : Input -> Game -> Game
stepGame ({ direction, delta } as input) { snake } =
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
