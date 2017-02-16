{-# LANGUAGE FlexibleContexts #-}

module Truco.Mazo where

import        Control.Monad.State
import        Control.Lens
import        System.Random.Shuffle hiding (shuffle)
import        Control.Monad.Random.Class 
import        Extra (chunksOf)

import        Truco.Types

mazo  :: [Card]
mazo  =  [Card x y | x <- [minBound..] , y <- [minBound..] ]

shuffle :: MonadRandom m => m [Card]
shuffle = shuffleM mazo

deal :: (MonadState Game m, MonadRandom m) => m ()
deal = do
  ms <- shuffle
  let mss = chunksOf 3 ms
  players %= zipWith (set cards) mss
  players %= map (set down [])

