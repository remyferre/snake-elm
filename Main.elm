import Keyboard
import Window
import Random

--- CONSTANTS

snakeWidth = 16
columns    = 42
rows       = 26
width      = snakeWidth * columns
height     = snakeWidth * rows
maxX       = (toFloat width)  / 2
maxY       = (toFloat height) / 2

--- INPUT

type Input = { direction: Direction, delta: Time, randX: Int, randY: Int }

-- Key
isArrow : Int -> Bool
isArrow k = k >= 37 && k <= 40

lastPressedArrow : Signal Int
lastPressedArrow = keepIf isArrow 37 Keyboard.lastPressed

-- Direction
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

lastDirection : Signal Direction
lastDirection = direction <~ lastPressedArrow

-- Random ints to generate new fruit position
randCoord : Float -> Signal Int
randCoord range =
    let mid = round <| (range / 2)
        min = -mid
        max = mid-1
    in
      Random.range min max delta

randX = randCoord columns
randY = randCoord rows

--- MODEL

-- Helpers about coordinates
toCoord : (Int, Int) -> (Float, Float)
toCoord (x, y) = (snakeWidth/2 + (toFloat x) * snakeWidth,
                  snakeWidth/2 + (toFloat y) * snakeWidth)

nextCoord : Float -> Float -> Float
nextCoord coord max =
    let coord' = coord + snakeWidth
    in
      if coord' > max then snakeWidth/2 - max else coord'

right : Float -> Float
right x = nextCoord x maxX

up : Float -> Float
up y = nextCoord y maxY

-- Fruit
validFruit : (Float, Float) -> [(Float, Float)] -> (Float, Float)
validFruit (randX, randY) positions =
    if any (\(x, y) -> (x, y) == (randX, randY)) positions then
        let overflow = randX + snakeWidth > maxX
            randX' = right randX
            randY' = up    randY
        in
          validFruit (if overflow then (randX', randY') else (randX', randY)) positions
    else
        (randX, randY)

newFruit : Int -> Int -> [(Float, Float)] -> (Float, Float)
newFruit randX randY positions =
    validFruit (toCoord (randX, randY)) positions

-- Snake
type Snake = { positions: [(Float, Float)], dir: Direction }

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
      x - snakeWidth / 2 < -maxX ||
      x + snakeWidth / 2 >  maxX ||
      y - snakeWidth / 2 < -maxY ||
      y + snakeWidth / 2 >  maxY

collideSnake : Snake -> Bool
collideSnake { positions, dir } =
    let (x, y)   = last positions
        otherPos = tail <| reverse (tail positions)
    in
      any (\(x', y') -> (x', y') == (x, y)) otherPos

-- Game
type Game = { snake: Snake, fruit: (Float, Float) }

initGame : Game
initGame =
    let positions =  reverse <| map (\i -> toCoord (i, 0)) [-5..5]
    in { snake = { positions = positions
                 , dir = Left }
       , fruit = newFruit 0 0 positions }

stepGame : Input -> Game -> Game
stepGame ({ direction, delta, randX, randY } as input) { snake, fruit } =
    if collideWalls snake || collideSnake snake then
        initGame
    else
        let snake'   = stepSnake input snake
            eatFruit = (last snake'.positions) == fruit
        in
          if eatFruit then
             { snake = snake', fruit = newFruit randX randY snake'.positions}
          else
              { snake = { snake' | positions <- drop 1 snake'.positions }
              , fruit = fruit }

--- DISPLAY

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
    container w h middle <| collage width height
                [
                 filled black (rect (toFloat width) (toFloat height))
                , displaySnake snake
                , displayFruit fruit
                ]

--- MAIN

delta : Signal Time
delta = inSeconds <~ fps 9

input = sampleOn delta (Input <~ lastDirection
                               ~ delta
                               ~ randX
                               ~ randY)

gameState : Signal Game
gameState = foldp stepGame initGame input

main = lift2 display Window.dimensions gameState
