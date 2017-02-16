module Truco.Envido where

import        Control.Lens hiding (indexed)
import        Prelude hiding (round)
import        Extra (dupe, (***))
import        Control.Monad
import        Control.Monad.IO.Class
import        Data.List (elemIndex)

import        Truco.Types
import        Truco.Points
import        Extras
import        Print
----------------------------------------------


envidoRoundPoints :: Envido -> IOS ()
envidoRoundPoints e = do
  list <- getPlayerOrder
  b <- foldlM_ startPlayerEnvidoPoints list Nothing 
  maybe (printIOS "something wrong") (envidoWon e) b

startPlayerEnvidoPoints :: PlayerId -> Maybe (Score,PlayerId,TeamId) -> IOS (Maybe (Score,PlayerId,TeamId))
startPlayerEnvidoPoints i pwinner = do
  p <- getPlayer i
  switchToPlayer p
  printPartialWinner p pwinner
  opts <- getEnvidoPointsOptions p pwinner
  printPlayerState p
  ch <- if length opts == 1
          then return $ head opts
          else do  printOptions opts;
                   c <- parseOption (length opts)
                   return $ opts !! (c-1)
  pts <- getPlayerPoints ch p
  max' (Just (pts, i, p^.team)) pwinner

max' :: Maybe (Score,PlayerId,TeamId) -> Maybe (Score,PlayerId,TeamId) -> IOS (Maybe (Score,PlayerId,TeamId))
max' l Nothing = return l
max' x@(Just (a,px,_)) y@(Just (b,py,_)) = case compare a b of
                                       EQ -> getPlayerOrder >>= \o -> if elemIndex px o > elemIndex py o then return x else return y 
                                       LT -> return y
                                       GT -> return x

printPartialWinner :: Player -> Maybe (Score,PlayerId,TeamId) -> IOS ()
printPartialWinner _ Nothing        = return ()
printPartialWinner p (Just (sw,pidw,tw)) = do
  pw <- getPlayer pidw 
  let cond = if p^.team == tw then " ganando " else " perdiendo "
  printIOS $ p^.name ++ ", tu equipo va" ++ cond ++ "el envido. " 
  printIOS $  pw^.name ++ " cantó " ++ show sw ++ " puntos" 

getPlayerPoints :: EnvidoPointsOption -> Player -> IOS Score
getPlayerPoints Mesa p       = return $ (valueCard . num . head)  (p^.down)
getPlayerPoints SonBuenas _  = return 0
getPlayerPoints Cantar    _  = printIOS "Ingrese el valor de su envido:" >> parseOption 33
  
getEnvidoPointsOptions :: Player -> Maybe (Score,PlayerId,TeamId) -> IOS [EnvidoPointsOption]
getEnvidoPointsOptions p Nothing  =
  return $ [Cantar] ++ if p^.down == [] then [] else [Mesa]
getEnvidoPointsOptions p (Just _) = 
  return $ [Cantar, SonBuenas] ++ if p^.down == [] then [] else [Mesa]


envidoWon :: Envido -> (Score,PlayerId,TeamId) -> IOS ()
envidoWon e (s,p,tid) = do
  hand.envido .= Just (e, Won s p, tid)
