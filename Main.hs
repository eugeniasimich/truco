module Main where

import        Control.Monad.State
import        Control.Monad.Loops
import        System.Console.ANSI

import        Truco.Types
import        Truco.Hand
import        Truco.Round
import        Truco.Points
import        Truco.Mazo
import        Print

--initialstate with four players
initialState  :: Game
initialState  = Game
  { _players  = cuatrojugs 
  , _hand     = emptyHand
  , _points   = (0,0)
  } where emptyHand   = Hand (Left NotPlayed, Nothing) Nothing 1 [] 0 0 []

cuatrojugs = [juan,maria,pedro,ana]
  where  juan        = Player "Juan" [] [] (toEnum 0) Red
         maria       = Player "María" [] [] (toEnum 1) Blue
         pedro       = Player "Pedro" [] [] (toEnum 0) Red
         ana         = Player "Ana" [] [] (toEnum 1) Blue

playGame     :: IOS ()
playGame     = do
  playHand
  b <- whoWonGame
  maybe (resetHandState >> playGame) printEndOfGame b


playHand      :: IOS ()
playHand      = do
  deal                   --deal out three cards for each player
  whileM notEndOfHand playRound
  computePoints

main          :: IO Game
main           = execStateT playGame initialState

init :: IO ()
init = setTitle "TRUCO"
