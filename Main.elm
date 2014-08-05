import Keyboard
import Window

snakeWidth = 16
maxWidth   = snakeWidth * 40
maxHeight  = snakeWidth * 26

delta : Signal Time
delta = inSeconds <~ fps 10

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

type Snake = { positions: [(Float, Float)] }
type Game = { snake: Snake }

initGame : Game
initGame = { snake = { positions = map
                                   (\i -> (snakeWidth/2 + i * -snakeWidth,
                                           snakeWidth/2))
                                   [-5..5] } }

stepSnake : Input -> Snake -> Snake
stepSnake { direction, delta } { positions } =
    let (x, y) = last positions
        x' = x + snakeWidth * case direction of
                                Left  -> -1
                                Right ->  1
                                _     ->  0
        y' = y + snakeWidth * case direction of
                                Up   ->  1
                                Down -> -1
                                _    ->  0
    in
      { positions = (drop 1 positions) ++ [(x', y')] }

stepGame : Input -> Game -> Game
stepGame ({ direction, delta } as input) { snake } =
    let (x, y) = last snake.positions
        hasLost = x - snakeWidth / 2 < -(maxWidth  / 2) ||
                  x + snakeWidth / 2 >   maxWidth  / 2  ||
                  y - snakeWidth / 2 < -(maxHeight / 2) ||
                  y + snakeWidth / 2 >   maxHeight / 2
    in
      if hasLost then initGame else { snake = stepSnake input snake }

gameState : Signal Game
gameState = foldp stepGame initGame input

displaySnake : Snake -> Form
displaySnake { positions } =
    let style = { color = white
                , width = snakeWidth
                , cap = Flat
                , join = Sharp 10
                , dashOffset = 0
                , dashing = [] }
    in
      traced style (path positions)

display : (Int, Int) -> Game -> Element
display (w, h) { snake } =
    container w h middle <| collage maxWidth maxHeight
                [
                  filled black (rect maxWidth maxHeight)
                , displaySnake snake
                ]

main = lift2 display Window.dimensions gameState
