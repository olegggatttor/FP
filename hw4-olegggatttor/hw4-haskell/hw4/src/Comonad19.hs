module Comonad19 where

import Control.Comonad
import Control.Applicative ((<$>))
import Data.List (intercalate)
import System.Random (RandomGen (..), StdGen, random, mkStdGen, randomIO)
import Control.Monad (liftM2)
import Control.Concurrent (threadDelay)
import System.Process (system)

data ListZipper a
  = LZ
  { leftList  :: [a]
  , focus     :: a
  , rightList :: [a]
  }

instance Show a => Show (ListZipper a) where
  show (LZ xs a ys) = show xs ++ " " ++ show a ++ " " ++ show ys

goLeft
  :: ListZipper a
  -> ListZipper a
goLeft (LZ (x:xs) a ys) = LZ xs x (a:ys)
goLeft _                = error "Can't move left."

goRight
  :: ListZipper a
  -> ListZipper a
goRight (LZ xs a (y:ys)) = LZ (a:xs) y ys
goRight _                = error "Can't move right."

listWrite
  :: a
  -> ListZipper a
  -> ListZipper a
listWrite x lz = lz { focus = x }

genericMove
  :: (a -> a)
  -> (a -> a)
  -> a
  -> ListZipper a
genericMove f g e = LZ (tail . iterate f $ e) e (tail . iterate g $ e)

instance Functor ListZipper where
  fmap f (LZ xs a ys) = LZ (fmap f xs) (f a) (fmap f ys)

instance Comonad ListZipper where
  extract = focus

  duplicate = genericMove goLeft goRight

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

up
  :: Grid a
  -> Grid a
up = Grid . goLeft . unGrid

down
  :: Grid a
  -> Grid a
down = Grid . goRight . unGrid

left
  :: Grid a
  -> Grid a
left g = Grid $ fmap goLeft (unGrid g)

right
  :: Grid a
  -> Grid a
right g = Grid $ fmap goRight (unGrid g)

gridRead
  :: Grid a
  -> a
gridRead g = extract $ extract $ unGrid g

gridWrite
  :: a
  -> Grid a
  -> Grid a
gridWrite x g = Grid $ listWrite newLine (unGrid g)
  where
    newLine = listWrite x (extract $ unGrid g)

horizontal
  :: Grid a
  -> ListZipper (Grid a)
horizontal = genericMove left right

vertical
  :: Grid a
  -> ListZipper (Grid a)
vertical = genericMove up down

instance Functor Grid where
  fmap f (Grid g) = Grid $ (fmap $ fmap f) g

instance Comonad Grid where
  extract = gridRead

  duplicate = Grid . fmap horizontal . vertical


instance Show a => Show (Grid a) where
  show (Grid (LZ xs a ys)) = intercalate "\n" (fmap show xs)
                             ++ "\n>" ++ show a ++ "<\n"
                             ++ intercalate "\n" (fmap show ys)

data CovidParams
  = CovidParams
  { infectionProb       :: Double
  , incubation          :: Int
  , desease             :: Int
  , immunity            :: Int
  }

data HealthStatus
  = NotInfected
  | IncubatedInfection Int
  | Infected Int
  | Immunity Int
  deriving Eq

instance Show HealthStatus where
  show NotInfected = " "
  show (IncubatedInfection _) = "!"
  show (Infected _) = "#"
  show (Immunity _) = "@"

data Human
  = Human
  { status :: HealthStatus
  , gen    :: StdGen
  }

instance Show Human where
  show = show . status

data CovidField
  = CovidField
  { params :: CovidParams
  , field  :: Grid Human
  }

neighbours
  :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals
  where
    horizontals
      :: [Grid a -> Grid a]
    horizontals = [left, right]

    verticals
      :: [Grid a -> Grid a]
    verticals   = [up, down]


rule
  :: CovidParams
  -> Grid Human
  -> Human
rule setup grid = do
  let cur = gridRead grid
  let curParams = status cur
  case curParams of
    IncubatedInfection 0 -> Human (Infected (desease setup)) (gen cur)
    IncubatedInfection n -> Human (IncubatedInfection (n - 1)) (gen cur)
    Infected 0 -> Human (Immunity (immunity setup)) (gen cur)
    Infected n -> Human (Infected (n - 1)) (gen cur)
    Immunity 0 -> Human NotInfected (gen cur)
    Immunity n -> Human (Immunity (n - 1)) (gen cur)
    NotInfected  -> do
      let nbs = gridRead <$> (neighbours <*> [grid])
      let canBeInfected = any isInfected nbs
      if canBeInfected
      then do
        let rndGenerator =  gen cur
        let prob = infectionProb setup
        let (p, newGenerator) = random rndGenerator
        case p <= prob of
          True  -> Human (IncubatedInfection (incubation setup)) newGenerator
          False -> Human NotInfected newGenerator
      else cur


step
  :: CovidField
  -> CovidField
step (CovidField setup grid) = CovidField setup $ extend (rule setup) grid

isInfected
  :: Human
  -> Bool
isInfected (Human (Infected _) _)           = True
isInfected (Human (IncubatedInfection _) _) = True
isInfected _                                = False


defaultSetup
  :: CovidParams
defaultSetup = CovidParams { infectionProb = 0.2, incubation = 7, desease = 14, immunity = 30 }

transformHuman
  :: Int
  -> Human
  -> Human
transformHuman shift (Human status gen)  = do
  let (p, _) = (random gen :: (Int, StdGen))
  Human status (mkStdGen $ shift + p)

createField
  :: CovidParams
  -> Int
  -> CovidField
createField setup seed = CovidField setup genField
  where
    genField
      :: Grid Human
    genField = gridWrite (Human (IncubatedInfection (incubation setup)) (mkStdGen seed))
             $ Grid
             $ genericMove (extend (transformHuman 3 . focus)) (extend (transformHuman 4 . focus))
             $ genericMove (transformHuman 0) (transformHuman 2) (Human NotInfected (mkStdGen seed))

toList
  :: Int
  -> ListZipper a
  -> [a]
toList n (LZ xs x ys) = reverse (take n xs) ++ [x] ++ take n ys

explore
  :: Int
  -> CovidField
  -> String
explore n (CovidField _ g) = intercalate "\n" $ map (>>= show) $ toList n $ (toList n) <$> (unGrid g)

simulate
  :: CovidField
  -> Int
  -> IO ()
simulate field 500 = return ()
simulate field i = do
  system "cls"
  putStrLn $ explore 10 $ step field
  threadDelay 100000
  simulate (step field) (i + 1)


play = do
  seed <- randomIO
  simulate (createField defaultSetup seed) 0
