module Lab1 where

-- First exercise
one :: ((Char, Integer), String, [Double])
one = (('s', 2), "sa", [1.2, 3.5])

two :: [(Double, Bool, (String, Integer))]
two = []

three :: ([Integer], [Double], [(Bool, Char)])
three = ([1, 2, 3], [1.2, 3.5, 5.6], [(True, 'a'), (False, 'f')])

four :: [[[(Integer, Bool)]]]
four = [[[(1, True), (2, False), (3, True)], [(1, True), (2, False), (3, True)], [(1, True), (2, False), (3, True)]], [[(1, True), (2, False), (3, True)], [(1, True), (2, False), (3, True)], [(1, True), (2, False), (3, True)]], [[(1, True), (2, False), (3, True)], [(1, True), (2, False), (3, True)], [(1, True), (2, False), (3, True)]]]

five :: (((Char, Char), Char), [String])
five = ((('a', 'b'), 'c'), ["hello", "world"])

six :: (([Double], [Bool]), [Integer])
six = (([1.2, 3.4], [True, False]), [1, 2, 3])

seven :: [(Integer, (Integer, [Bool]))]
seven = [(1, (2, [True, False])), (3, (4, [False, True]))]

eight :: (Bool, ([Bool], [Integer]))
eight = (True, ([True, False], [1, 3, 5]))

nine :: [([Bool], [Double])]
nine = [([True, False], [2.4, 5.2]), ([False, True], [75.3, 23.1])]

ten :: [([Integer], String)]
ten = [([1, 2], ['s', 'f']), ([3, 4], ['w', 'e'])]

-- Second exercise

-- 2.1
isPointBetween :: Double -> Double -> Double -> Bool
isPointBetween ab ac bc = ab + ac == bc

-- 2.2
calculateDegree :: Double -> Double
calculateDegree deg = 180 - deg

-- 2.3
findBaseOfTriangle :: Double -> Double -> Double
findBaseOfTriangle p l = p - 2 * l

-- 2.4
findAnglesByProportions :: Double -> Double -> Double -> (Double, Double, Double)
findAnglesByProportions first second third = (alpha, beta, gamma)
  where
    middle = 180 / (first + second + third)
    alpha = middle * first
    beta = middle * second
    gamma = middle * third

-- 2.5
triangleHeight :: Double -> Double -> Double
triangleHeight side base = sqrt (side ^ 2 - (base / 2) ^ 2)

-- 2.6

type Circle = (Double, Double, Double)

distance :: Double -> Double -> Double -> Double -> Double
distance x1 y1 x2 y2 = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

isInside :: Circle -> Circle -> Bool
isInside (x1, y1, r1) (x2, y2, r2) = r1 > r2 + distance x1 y1 x2 y2

intersection :: Circle -> Circle -> Int
intersection (x1, y1, r1) (x2, y2, r2)
  | d > r1 + r2 = 0
  | d == r1 + r2 = 1
  | d < abs (r1 - r2) = 0
  | otherwise = 2
  where
    d = distance x1 y1 x2 y2

checkCircles :: Circle -> Circle -> Either Bool Int
checkCircles (x1, y1, r1) (x2, y2, r2) = if isInside firstC secondC then Left True else Right (intersection firstC secondC)
  where
    firstC = (x1, y1, r1)
    secondC = (x2, y2, r2)

-- 2.7
isPossibleBuildRightTriangle :: Double -> Double -> Double -> String
isPossibleBuildRightTriangle a b c = if a + b > c && a + c > b && b + c > a then "Yes" else "No"

-- 2.8
isPossibleGetSquare :: Double -> Double -> Bool
isPossibleGetSquare diameter side = sqrt (diameter * 2) <= side

-- 2.9
findHeightToC :: Double -> Double -> Double -> Double
findHeightToC a b c = height
  where
    p = (a + b + c) / 2
    s = sqrt (p * (p - a) * (p - b) * (p - c))
    height = (2 / c) * s

-- 2.10
trigAngles :: Double -> Double -> (Double, Double, Double)
trigAngles h l = (alpha, beta, gamma)
  where
    alpha = asin (h / (0.5 * l)) * 180.0 / pi
    beta = 180.0 - 2 * alpha
    gamma = alpha

sideLength :: Double -> Double -> Double
sideLength h l = sqrt ((0.5 * l) ^ 2 + h ^ 2)

findAnglesAndSide :: Double -> Double -> ((Double, Double, Double), Double)
findAnglesAndSide h l = (angles, side)
  where
    angles = trigAngles h l
    side = sideLength h l

-- 2.11
arcLength :: Double -> Double -> Double
arcLength a c = 2 * pi * r * (c / 360.0)
  where
    r = a / 2 * sin (c / 2 * pi / 180.0)

-- 2.12
distanceFromOrigin :: (Double, Double) -> Double
distanceFromOrigin (x, y) = sqrt (x ^ 2 + y ^ 2)

equidistantPoint :: (Double, Double) -> (Double, Double)
equidistantPoint (x, y) = (newX, newY)
  where
    r = distanceFromOrigin (x, y)
    theta = atan2 y x
    newX = r * cos (theta + pi / 2)
    newY = r * sin (theta + pi / 2)

-- 2.13

type Point = (Double, Double)

slope :: Point -> Point -> Double
slope (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1)

isParallelogram :: Point -> Point -> Point -> Point -> Bool
isParallelogram p1 p2 p3 p4 =
  slope p1 p2 == slope p3 p4 && slope p1 p4 == slope p2 p3

-- 2.14
lineEquation :: (Float, Float) -> (Float, Float) -> (Float, Float, Float)
lineEquation (x1, y1) (x2, y2)
  | x1 == x2 = (1, 0, -x1)
  | y1 == y2 = (0, 1, -y1)
  | otherwise = (a, -1, y1 - a * x1)
  where
    a = (y2 - y1) / (x2 - x1)

-- 2.15
distanceByTuples :: Point -> Point -> Double
distanceByTuples (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

isSquare :: Point -> Point -> Point -> Point -> Bool
isSquare a b c d = (distAB == distCD) && (distBC == distAD) && (distAB == distBC || distAB == distAD)
  where
    distAB = distanceByTuples a b
    distBC = distanceByTuples b c
    distCD = distanceByTuples c d
    distAD = distanceByTuples a d
