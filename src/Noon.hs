module Noon where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Control.Lens ((&), (+~), (-~), (^.), (.~), _2)
import Data.Ord (comparing)
import Data.Foldable (minimumBy, maximumBy)
import Data.Witherable (mapMaybe)
import AStar (astar, FCost)
import Linear.V2 (V2(..), _x, _y)
import Graphics.Gloss (Picture, rectangleSolid, translate, white, display, Display(..), green, blue, red, color, play)
import Graphics.Gloss.Interface.IO.Interact (Event(..), Key(..), KeyState(..))
import qualified Data.Vector as V

type Point = V2 Int
type Piece = Set Point
type Board = Set Piece

sun :: Point -> Piece
sun (V2 x y) = Set.fromList [V2 a b | a <- [x..x+1], b <- [y..y+1]]

single :: Point -> Piece
single = Set.singleton

doubleV :: Point -> Piece
doubleV p = Set.fromList [p, p & _y +~ 1]

doubleH :: Point -> Piece
doubleH p = Set.fromList [p, p & _x +~ 1]

data Direction
  = North
  | East
  | South
  | West

bounds :: [[Point]]
bounds = [[V2 x y | x <- [0..3]] | y <- [0..4]]

boundsSet :: Set Point
boundsSet = Set.fromList (concat bounds)

blanks :: Board -> Set Point
blanks = Set.difference boundsSet . Set.unions

move :: Piece -> Direction -> Piece
move p d = Set.map f p
  where
    f = case d of
      North -> _y +~ 1
      East  -> _x +~ 1
      South -> _y -~ 1
      West  -> _x -~ 1

movesFor :: Board -> Piece -> [Board]
movesFor b p = mapMaybe (fmap (addMove b p) . isValid . move p) [North, East, South, West]
  where
    isValid s | Set.isSubsetOf s $ Set.intersection boundsSet $ Set.union p $ blanks b = Just s
              | otherwise = Nothing
    addMove b p p' = Set.insert p' $ Set.delete p b

allMoves :: Board -> [Board]
allMoves b = foldMap (movesFor b) b 

victory :: Board -> Bool
victory = Set.member $ sun $ V2 1 0

initialBoard :: Board
initialBoard = Set.fromList
  [ doubleV $ V2 0 0
  , doubleV $ V2 3 0
  , single $ V2 1 0
  , single $ V2 2 0
  , single $ V2 1 1
  , single $ V2 2 1
  , doubleH $ V2 1 2
  , doubleV $ V2 0 3
  , doubleV $ V2 3 3
  , sun $ V2 1 3
  ]

boardMap :: Board -> Map Point Int
boardMap = Map.fromList 
         . foldMap (\(ps, i) -> zip (Set.toList ps) (repeat i))
         . flip zip [0..] 
         . Set.toList

render :: Board -> String
render b = foldMap (\row -> foldMap go row <> "\n") (reverse bounds)
  where
    bm = boardMap b
    go p = maybe "." show $ Map.lookup p bm

blockSize :: Num a => a
blockSize = 50

renderPiece :: Piece -> Picture
renderPiece = go . fmap (fmap fromIntegral) . Set.toList
  where
    go [] = mempty
    go xs = 
      let mid = sum xs / fromIntegral (length xs)
          (minx, miny, maxx, maxy) = extents xs
          w   = (1 + (maxx ^. _x - minx ^. _x)) * blockSize - 2
          h   = (1 + (maxy ^. _y - miny ^. _y)) * blockSize - 2
          c = case length xs of
            1 -> green
            2 -> blue
            _ -> red
      in translate (mid ^. _x * blockSize) (mid ^. _y * blockSize) $ color c $ rectangleSolid w h

renderBoard :: Board -> Picture
renderBoard bs = translate (- blockSize * 1.5) (- blockSize * 2) $ foldMap renderPiece bs

extents :: Ord a => [V2 a] -> (V2 a, V2 a, V2 a, V2 a)
extents ps = (minx, miny, maxx, maxy)
  where
    minx = minimumBy (comparing (^. _x)) ps
    maxx = maximumBy (comparing (^. _x)) ps
    miny = minimumBy (comparing (^. _y)) ps
    maxy = maximumBy (comparing (^. _y)) ps

solveAStar :: Maybe ([Board], FCost Int)
solveAStar = astar initialBoard victory h nbs
  where
    h = const 0
    nbs b = Map.fromList $ zip (allMoves b) (repeat 1)

interactive :: IO ()
interactive = do
  let Just (bs, _) = solveAStar
      bs' = V.fromList bs
      is  = (bs', 0)
  play (InWindow "Noon" (4 * blockSize, 5 * blockSize) (10, 10)) white 1 is (uncurry renderWorld) handleEvent (\_ w -> w)
  where
    renderWorld bs n = renderBoard $ bs V.! n
    handleEvent (EventKey (Char 'n') Up _ _) w = w & _2 +~ 1
    handleEvent (EventKey (Char 'b') Up _ _) w = w & _2 -~ 1
    handleEvent _ w = w