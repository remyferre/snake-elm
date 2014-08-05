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

opposite : Direction -> Direction -> Bool
opposite d1 d2 = case (d1, d2) of
                   (Left,Right) -> True
                   (Right,Left) -> True
                   (Up,Down)    -> True
                   (Down,Up)    -> True
                   _            -> False

isArrow : Int -> Bool
isArrow k = k >= 37 && k <= 40

lastPressedArrow : Signal Int
lastPressedArrow = keepIf isArrow 37 Keyboard.lastPressed

type Input = { direction: Direction, delta: Time }

input = sampleOn delta (Input <~ lift direction lastPressedArrow
                               ~ delta)

type Snake = { positions: [(Float, Float)], dir: Direction }
type Game = { snake: Snake }

initGame : Game
initGame = { snake = { positions = map
                                   (\i -> (snakeWidth/2 + i * -snakeWidth,
                                           snakeWidth/2))
                                   [-5..5]
                     , dir = Left
                     }
           }

stepSnake : Input -> Snake -> Snake
stepSnake { direction, delta } { positions, dir } =
    let dir' = if opposite direction dir then dir else direction
        (x, y) = last positions
        x' = x + snakeWidth * case dir' of
                                Left  -> -1
                                Right ->  1
                                _     ->  0
        y' = y + snakeWidth * case dir' of
                                Up   ->  1
                                Down -> -1
                                _    ->  0
    in
      { positions = (drop 1 positions) ++ [(x', y')], dir = dir' }

collideWalls : Snake -> Bool
collideWalls { positions, dir } =
    let (x, y) = last positions
    in
      x - snakeWidth / 2 < -(maxWidth  / 2) ||
      x + snakeWidth / 2 >   maxWidth  / 2  ||
      y - snakeWidth / 2 < -(maxHeight / 2) ||
      y + snakeWidth / 2 >   maxHeight / 2

collideSnake : Snake -> Bool
collideSnake { positions, dir } =
    let (x, y)   = last positions
        otherPos = tail <| reverse (tail positions)
    in
      any (\(x', y') -> x == x' && y == y') otherPos

stepGame : Input -> Game -> Game
stepGame ({ direction, delta } as input) { snake } =
    if collideWalls snake || collideSnake snake then
        initGame
    else
        { snake = stepSnake input snake }

gameState : Signal Game
gameState = foldp stepGame initGame input

displaySnake : Snake -> Form
displaySnake { positions, dir } =
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
