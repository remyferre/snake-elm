import Keyboard
import Window
import Random

snakeWidth = 16
columns    = 42
rows       = 26
maxWidth   = snakeWidth * columns
maxHeight  = snakeWidth * rows

delta : Signal Time
delta = inSeconds <~ fps 10

randX : Signal Int
randX = Random.range (round <| columns/ -2) ((round <| columns/2)-1) delta

randY : Signal Int
randY = Random.range (round <| rows/ -2) ((round <| rows/2)-1) delta

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

type Input = { direction: Direction, delta: Time, randX: Int, randY: Int }

input = sampleOn delta (Input <~ lift direction lastPressedArrow
                               ~ delta
                               ~ randX
                               ~ randY)

type Snake = { positions: [(Float, Float)], dir: Direction }
type Game = { snake: Snake, fruit: (Float, Float) }

newFruit : Int -> Int -> (Float, Float)
newFruit randX randY =
    (snakeWidth/2 + (toFloat <| snakeWidth * randX),
     snakeWidth/2 + (toFloat <| snakeWidth * randY))

initGame : Game
initGame = { snake = { positions = map
                                   (\i -> (snakeWidth/2 + i * -snakeWidth,
                                           snakeWidth/2))
                                   [-5..5]
                     , dir = Left
                     }
           , fruit = newFruit 0 0
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
      { positions = positions ++ [(x', y')], dir = dir' }

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
stepGame ({ direction, delta, randX, randY } as input) { snake, fruit } =
    if collideWalls snake || collideSnake snake then
        initGame
    else
        let snake'   = stepSnake input snake
            eatFruit = (last snake'.positions) == fruit
        in
          if eatFruit then
             { snake = snake', fruit = newFruit randX randY }
          else
              { snake = { snake' | positions <- drop 1 snake'.positions }
              , fruit = fruit }

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

displayFruit : (Float, Float) -> Form
displayFruit (x, y) =
    move (x, y) <| filled red (rect snakeWidth snakeWidth)

display : (Int, Int) -> Game -> Element
display (w, h) { snake, fruit } =
    container w h middle <| collage maxWidth maxHeight
                [
                  filled black (rect maxWidth maxHeight)
                , displaySnake snake
                , displayFruit fruit
                ]

main = lift2 display Window.dimensions gameState
