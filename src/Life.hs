module Life where

import Control.Comonad

data Stream a = a :> Stream a deriving (Show, Eq)

infixr 5 :>

instance Functor Stream where
  fmap f (a :> as) = f a :> fmap f as

instance Applicative Stream where
  pure a                  = a :> pure a
  (f :> fs) <*> (a :> as) = f a :> (fs <*> as)

instance Comonad Stream where
  extract (a :> _)    = a
  duplicate (a :> as) = pure a :> duplicate as

instance Foldable Stream where
  foldr f acc (a :> as) = f a (foldr f acc as)

takes :: Int -> Stream a -> [a]
takes n (a :> as)
  | n <= 0    = []
  | otherwise = a : takes (n - 1) as

tails :: Stream a -> Stream a
tails (_ :> as) = as

data Zipper a
  = Zipper
  { lefts :: Stream a
  , focus :: a
  , rights :: Stream a
  } deriving Show

goRight :: Zipper a -> Zipper a
goRight (Zipper ls a (r :> rs)) =
  Zipper (a :> ls) r rs

goLeft :: Zipper a -> Zipper a
goLeft (Zipper (l :> ls) a rs) =
  Zipper ls l (a :> rs)

genericMove :: (z a -> z a) -> (z a -> z a) -> z a -> Zipper (z a)
genericMove a b z =
  Zipper (tails $ iterate' a z) z (tails $ iterate' b z)

iterate' :: (a -> a) -> a -> Stream a
iterate' f seed = seed :> iterate' f (f seed)

instance Functor Zipper where
  fmap f (Zipper l a r) = Zipper (fmap f l) (f a) (fmap f r)

instance Applicative Zipper where
  pure a = Zipper (pure a) a (pure a)
  Zipper lfs f rfs <*> Zipper las a ras =
    Zipper (lfs <*> las) (f a) (rfs <*> ras)

instance Comonad Zipper where
  extract (Zipper _ a _) = a
  duplicate z@(Zipper lfs a rfs) =
    genericMove goLeft goRight z

data Board a = Board { unBoard :: Zipper (Zipper a) }

instance Functor Board where
  fmap f (Board z) = Board (fmap (fmap f) z)

up :: Board a -> Board a
up = Board . goRight . unBoard

down :: Board a -> Board a
down = Board . goLeft . unBoard

left :: Board a -> Board a
left = Board . fmap goLeft . unBoard

right :: Board a -> Board a
right = Board . fmap goRight . unBoard

horizontal :: Board a -> Zipper (Board a)
horizontal = genericMove left right

vertical :: Board a -> Zipper (Board a)
vertical = genericMove up down

instance Comonad Board where
  extract = extract . extract . unBoard
  duplicate = Board . fmap horizontal . vertical

neighbors :: [Board a -> Board a]
neighbors = horiz ++ vert ++ ((.) <$> horiz <*> vert)
  where
    horiz = [left, right]
    vert  = [up, down]

aliveNeighbors :: Board Bool -> Int
aliveNeighbors board =
  card . map (\dir -> extract . dir $ board) $ neighbors

card :: [Bool] -> Int
card = foldr (\b -> if b then succ else id) 0

rule :: Board Bool -> Bool
rule board = case aliveNeighbors board of
                  2 -> extract board
                  3 -> True
                  _ -> False

evolve :: Board Bool -> Board Bool
evolve = extend rule

boardToList :: Int -> Board a -> [a]
boardToList n = undefined

displayLine :: Zipper Bool -> String
displayLine = ('|' :) . map (\x -> if x then '*' else ' ') . toList 20

displayBoard :: Board Bool -> String
displayBoard = unlines . map displayLine . toList 10 . unBoard

toList :: Int -> Zipper a -> [a]
toList n (Zipper ls x rs) =
  reverse (takes n ls) ++ [x] ++ takes n rs

(>++) :: [a] -> Stream a -> Stream a
[] >++ as = as
(x:xs) >++ as = x :> (xs >++ as)

glider :: Board Bool
glider =
  Board $ Zipper (pure fz) fz rs
    where
      rs = [ line [f, t, f]
           , line [f, f, t]
           , line [t, t, t]
           ] >++ pure fz
      t = True
      f = False
      fl = pure f
      fz :: Zipper Bool
      fz = pure f
      line l =
        Zipper fl f (l >++ fl)

timeline :: Board Bool -> Stream (Board Bool)
timeline = iterate' evolve
