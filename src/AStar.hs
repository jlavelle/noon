{-# LANGUAGE TemplateHaskell #-}

module AStar where

import Linear.V2 (V2(..), _x, _y)
import Control.Lens (Lens', _Wrapped', Wrapped, (%=), (.=), (^.), use, (<&>), FoldableWithIndex, ifor_)
import Control.Lens.TH (makeLenses)
import Control.Monad.State (get, evalState)
import GHC.Generics (Generic)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ

newtype FCost a = FCost { getFCost :: V2 a }
  deriving (Eq, Generic, Show)

instance Wrapped (FCost a)

instance (Num a, Ord a) => Ord (FCost a) where
  compare (FCost a) (FCost b) = compare (sum a) (sum b)

gcost :: Lens' (FCost a) a
gcost = _Wrapped' . _x

hcost :: Lens' (FCost a) a
hcost = _Wrapped' . _y

mkFCost :: a -> a -> FCost a
mkFCost g h = FCost (V2 g h)

data AStar a c = AStar
  { _openSet  :: OrdPSQ a (FCost c) a
  , _asParent :: Map a a
  , _scores   :: Map a (FCost c)
  }

makeLenses ''AStar

initAStar :: (Ord a, Ord c, Num c) => a -> c -> AStar a c
initAStar a ac =
  let cost = mkFCost 0 ac
  in AStar (PSQ.singleton a cost a) Map.empty (Map.singleton a cost)

astar :: (Ord a, Ord c, Num c, FoldableWithIndex a f) => a -> (a -> Bool) -> (a -> c) -> (a -> f c) -> Maybe ([a], FCost c)
astar s g h = fmap reconstruct . astar' s g h
  where
    reconstruct (l, c, as) = (go [l] (as ^. asParent), c)
      where
        go xs m = case Map.lookup (head xs) m of
          Nothing -> xs
          Just x  -> go (x : xs) m

astar' :: (Ord a, Ord c, Num c, FoldableWithIndex a f) => a -> (a -> Bool) -> (a -> c) -> (a -> f c) -> Maybe (a, FCost c, AStar a c)
astar' start isGoal h adjacent = evalState go $ initAStar start $ h start
  where
    go = do
      mcc <- dequeue
      case mcc of
        Just (curr, cost) | isGoal curr -> Just . (curr, cost,) <$> get
                          | otherwise   -> handleAdj curr cost *> go
        Nothing -> pure Nothing

    handleAdj curr cost = ifor_ (adjacent curr) \n nc -> do
      let tgs = cost ^. gcost + nc
      mns <- use scores <&> Map.lookup n
      case mns of
        Just ns | tgs < ns ^. gcost -> newPath curr n tgs
                | otherwise         -> pure ()
        Nothing -> newPath curr n tgs

      where
        newPath c n tgs = do
          let score = mkFCost tgs $ h n
          asParent %= Map.insert n c
          scores   %= Map.insert n score
          openSet  %= PSQ.insert n score n

    dequeue = do
      mmv <- use openSet <&> PSQ.minView
      case mmv of
        Just (_, p, v, q') -> do
          openSet .= q'
          pure $ Just (v, p)
        Nothing -> pure Nothing