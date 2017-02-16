module Truco.Hand where

import        Prelude              hiding (round)
import        Extra                       ((||^), anySame)
import        Control.Lens         hiding (indexed)
import        Control.Monad
import        Data.Maybe()

import        Truco.Types
import        Extras


---End of Hand---

notEndOfHand  :: IOS Bool
notEndOfHand  = liftM not endOfHand 

endOfHand     :: IOS Bool
endOfHand     = (uses (hand.round) handCompleted)
            ||^ (uses (hand.truco) isAlMazo)
            ||^ (uses (hand.formerRounds) thereIsAWinner)

wonTwoRounds  :: [(Round, Maybe TeamId)] -> Bool
wonTwoRounds  = anySame . filter (/= Nothing) . map snd 

wonWithTie :: [(Round, Maybe TeamId)] -> Bool
wonWithTie xs  = if elem (1, Nothing) xs                                 --first is tie
                 then (>0) . length . filter (/= Nothing) . map snd $ xs --there was a break tie
                 else (>0) . length . filter (== Nothing) . map snd $ xs --first not tied but other did

thereIsAWinner :: [(Round, Maybe TeamId)] -> Bool
thereIsAWinner xs =  wonWithTie xs || wonTwoRounds xs

handCompleted :: Round -> Bool
handCompleted = (> 3)

resetHandState    :: IOS ()
resetHandState    = do
  sp <- nextStartingHandPlayer
  hand .= Hand { _truco            = (Left NotPlayed, Nothing)
               , _envido           = Nothing
               , _round            = 1
               , _roundCards       = []
               , _handStarter      = sp
               , _roundStarter     = sp
               , _formerRounds     = [] } 

  
nextStartingHandPlayer :: IOS PlayerId
nextStartingHandPlayer = do
  l <- use (hand.handStarter)
  n <- uses players length
  return $ (l + 1) `mod` n
  

whoWonGame    :: IOS (Maybe TeamId)
whoWonGame    = do
  (l,r) <- use points
  return $ case (l>=30,r>=30, compare l r) of
    (True, True, EQ) -> Nothing
    (True, True, LT) -> Just (toEnum 0)
    (True, True, GT) -> Just (toEnum 1)
    (True, _, _)     -> Just (toEnum 1)
    (_, True, _)     -> Just (toEnum 0)
    _                -> Nothing
   
    
