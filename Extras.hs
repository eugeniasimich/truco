module Extras where

import        Prelude hiding (round,last)
import        Control.Monad.IO.Class (liftIO, MonadIO)
import        Control.Monad (liftM2)
import        Control.Lens hiding (indexed)
import        System.Console.ANSI
import        Data.List (findIndex)
import        Data.Maybe (fromJust, isNothing)
import        Data.Function               (on)

import        Truco.Types

---------------------------------------------------------


getPlayer :: PlayerId -> IOS Player
getPlayer i = uses players (!! i)

getPlayerId :: Player -> IOS PlayerId
getPlayerId p = 
  uses players (fromJust . findIndex (\player -> player^.name == p^.name) )

indexed :: [a] -> [(Int,a)]
indexed = zip [1..]

teamPoints :: TeamId -> IOS Int
teamPoints Nos = uses points fst
teamPoints Ellos = uses points snd
  
oponent :: TeamId -> TeamId
oponent Nos = Ellos
oponent Ellos = Nos

foldlM_  :: Monad m => (a -> b -> m b) -> [a] -> b -> m b
foldlM_ _ []     b = return b
foldlM_ f (x:xs) b = f x b >>= foldlM_ f xs

getPlayerOrder :: IOS [PlayerId]
getPlayerOrder = do
  startp <- use (hand.roundStarter)
  nplayers <- uses players length
  return $ [startp..nplayers-1] ++ [0..startp-1]

getCaptain :: TeamId -> IOS Player
getCaptain tid = do
  os <- getPlayerOrder
  let n = length os
  r1 <- getPlayer (os !! (n-1))
  if r1^.team == tid then return r1 else getPlayer (os !! (n-2))
  
isAlMazo      :: TrucoChallenge -> Bool
isAlMazo (Left (AlMazo _),_)  = True
isAlMazo _                    = False

trucoNotPlayed :: TrucoChallenge -> Bool
trucoNotPlayed (Left NotPlayed, _) = True
trucoNotPlayed _                = False

envidoNotPlayed :: EnvidoChallenge -> Bool
envidoNotPlayed = isNothing

sameTeam :: PlayerId -> PlayerId -> IOS Bool
sameTeam = on (liftM2 (on (==) (^.team))) getPlayer

compareSnd :: Ord b => (a,b) -> (a,b) -> Ordering
compareSnd x y = compare (snd x) (snd y)

compareFst :: Ord a => (a,b) -> (a,b) -> Ordering
compareFst x y = compare (fst x) (fst y)
