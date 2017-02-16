module Truco.Round where

import        Prelude              hiding (round)
import        Extra                       (ifM)
import        Control.Lens         hiding (indexed)
import        Data.List                   (maximumBy)
import        Data.Maybe()
import        Control.Monad               (unless)

import        Truco.Types
import        Truco.Player
import        Truco.Hand
import        Extras

--in a round, each player has to play a card
playRound     :: IOS ()
playRound     = do
  getPlayerOrder >>= mapM_ startPlayerAction
  end <- endOfHand
  unless end $ showRoundResults >> resetRoundState

resetRoundState   :: IOS ()
resetRoundState   = do
  rwinnerid <- whoWonRound
  winnerTeam <- maybe (return Nothing) (\w-> getPlayer w >>= return . Just . (^.team)) rwinnerid
  lr <- use (hand.round)
  nsrp <- nextRoundStarter
  hand.round %= (+1)
  hand.roundCards .= []
  hand.roundStarter .= nsrp
  hand.formerRounds %= (:) (lr,winnerTeam)

whoWonRound :: IOS (Maybe PlayerId)
whoWonRound = do
  rc <- use (hand.roundCards)
  let w1 = fst (maximumBy compareSnd rc)
      w2 = fst (maximumBy compareSnd (reverse rc))
  ifM (sameTeam w1 w2) (return (Just w1)) (return Nothing)

showRoundResults :: IOS ()
showRoundResults = return ()

nextRoundStarter :: IOS PlayerId
nextRoundStarter = uses (hand.roundCards) (fst . (maximumBy compareSnd))
