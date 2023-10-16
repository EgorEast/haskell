-- :?

-- :set +
-- :set +t
-- 1
-- 1 :: Integer
-- 1.2
-- 1.2 :: Double
-- 'a'
-- 'a' :: Char
-- True
-- True :: Bool
-- 2*2
-- 4 :: Integer
-- 2^2
-- 4 :: Integer
-- 2^3
-- 8 :: Integer
-- sqrt 2
-- 1.4142135623731 :: Double
-- 1 + sqrt 2
-- 2.41421356237309 :: Double
-- 1 + sqrt(2)
-- 2.41421356237309 :: Double
-- sqrt(2) + 1
-- 2.41421356237309 :: Double
-- sqrt 2 + 1
-- 2.41421356237309 :: Double
-- sqrt(2 + 1)
-- 1.73205080756888 :: Double
-- 2 ^ 5000
-- 141246703213942603683520966701614733366889617518454111681368808585711816984270751255808912631671152637335603208431366082764203838069979338335971185726639923431051777851865399011877999645131707069373498212631323752553111215372844035950900535954860733418453405575566736801565587405464699640499050849699472357900905617571376618228216434213181520991556677126498651782204174061830939239176861341383294018240225838692725596147005144243281075275629495339093813198966735633606329691023842454125835888656873133981287240980008838073668221804264432910894030789020219440578198488267339768238872279902157420307247570510423845868872596735891805818727796435753018518086641356012851302546726823009250218328018251907340245449863183265637987862198511046362985461949587281119139907228004385942880953958816554567625296086916885774828934449941362416588675326940332561103664556982622206834474219811081872404929503481991376740379825998791411879802717583885498575115299471743469241117070230398103378615232793710290992656444842895511830355733152020804157920090041811951880456705515468349446182731742327685989277607620709525878318766488368348965015474997864119765441433356928012344111765735336393557879214937004347568208665958717764059293592887514292843557047089164876483116615691886203812997555690171892169733755224469032475078797830901321579940127337210694377283439922280274060798234786740434893458120198341101033812506720046609891160700284002100980452964039788704335302619337597862052192280371481132164147186514169090917191909376 :: Integer
-- :?
-- (5, 3)
-- (5,3) :: (Integer,Integer)
-- ((1,'a'),1.2)
-- ((1,'a'),1.2) :: ((Integer,Char),Double)
-- fst(5,1)
-- 5 :: Integer

-- fst(5,True)
-- 5 :: Integer

-- snd(5,True)
-- True :: Bool

-- (1,2,3)
-- (1,2,3) :: (Integer,Integer,Integer)

-- fst (snd (1, ('a', 23.12)))
-- 'a' :: Char

-- [1,2]
-- [1,2] :: [Integer]

-- ['a','f']
-- "af" :: [Char]

-- ["a","f"]
-- ["a","f"] :: [[Char]]

-- -- "A"
-- "A" :: String
-- []
-- [] :: [a]

-- 1:[]
-- [1] :: [Integer]

-- '5':['1','2','3','4','5']
-- "512345" :: [Char]

-- False:[]
-- [False] :: [Bool]

-- 1:(2:(3:[]))
-- [1,2,3] :: [Integer]

-- [(1,'a'),(2,'b')]
-- [(1,'a'),(2,'b')] :: [(Integer,Char)]

-- [[1,2],[3,4,5]]
-- [[1,2],[3,4,5]] :: [[Integer]]

-- head [1,2,4]
-- 1 :: Integer

-- null []
-- True :: Bool

-- null [0]
-- False :: Bool

-- length []
-- 0 :: Int

-- length ['w']
-- 1 :: Int

-- head [1,2,3]
-- 1 :: Integer

-- tail [1,2,3]
-- [2,3] :: [Integer]

-- tail [1]
-- [] :: [Integer]

-- elem(2)([1,2,3])
-- True :: Bool

-- take(2)([1,2,3])
-- [1,2] :: [Integer]

-- zip(["20","30"])([1,2,3])
-- [("20",1),("30",2)] :: [([Char],Integer)]

-- [1,2,3,4,5] !! 3
-- 4 :: Integer

-- [1,2,3,4,5] !!(3)
-- 4 :: Integer
-- ([1,2,3,4,5])!!(3)
-- 4 :: Integer

-- [1,2]++[3,4]
-- [1,2,3,4] :: [Integer]

-- head "hello"
-- 'h' :: Char

-- tail "hello"
-- "ello" :: [Char]

-- length "hello"
-- 5 :: Int

-- "hello" ++ ", world"
-- "hello, world" :: [Char]

-- show 1
-- "1" :: [Char]

-- "Formula " ++ show 1
-- "Formula 1" :: [Char]

-- 1 + read "12"
-- 13 :: Integer


-- <, >, <=, >= – эти операторы имеют такой же смысл, как и в языке Си (меньше, больше, меньше или равно, больше или равно). 
-- = = – оператор проверки на равенство. 
-- /= – оператор проверки на неравенство. 


-- x >= 0 && x <= 10 
-- x > 3 && x /= 10 
-- (x > 10 || x < -10) && not (x == y) 