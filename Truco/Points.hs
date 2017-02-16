module Truco.Points where

import Truco.Types
import Data.Maybe (isJust,fromJust)
import Data.List.Utils (flipAL)
import Data.List (maximumBy, sortBy)
import Control.Lens

import Extras

computePoints :: IOS ()
computePoints = computeEnvido >> computeTruco

computeTruco :: IOS ()
computeTruco = do
  t <- use (hand.truco)
  fr <- use (hand.formerRounds)
  let pts = trucoPoints $ fst t
  let fr' = sortBy compareFst $ filter ((/= Nothing).snd) fr
  winner <- case (t, length fr') of
              ((Left (AlMazo _), Just tid), _) -> return tid
              (       _                   , 0) -> uses (hand.roundCards) breakTie >>= getPlayer >>= return . (^.team)
              (_                          , 3) -> let (Just x, a):(Just y, b):_ = (flipAL fr') in return $ if length a > length b then x else y 
              _                                -> (return . fromJust . snd . head) fr'
  addPoints winner pts

addPoints :: TeamId -> Int -> IOS ()
addPoints Nos n = (points._1) %= (+ n)
addPoints Ellos n = (points._2) %= (+ n) 

breakTie :: [(PlayerId,Card)] -> PlayerId
breakTie = fst . maximumBy compareSnd

trucoPoints :: Either NotPlayed Truco -> Int
trucoPoints (Left NotPlayed)   = 1
trucoPoints (Left (AlMazo t))  = trucoPoints (Right t) - 1
trucoPoints (Right Truco)      = 2
trucoPoints (Right Retruco)    = 3
trucoPoints (Right ValeCuatro) = 4 
   
computeEnvido :: IOS ()
computeEnvido = do
  e <- use (hand.envido)
  case e of
    Nothing -> return ()
    Just (env,a,tid) -> envidoPoints env a >>= addPoints tid 
      
envidoPoints :: Envido -> Action -> IOS Int
envidoPoints Envido Rejected        = return 1
envidoPoints EnvidoEnvido Rejected  = return 3
envidoPoints RealEnvido Rejected    = return 4
envidoPoints FaltaEnvido Rejected   = return 1
envidoPoints e (Won i pid)          = do
  p <- getPlayer pid
  let val = envidoScore $ p^.cards ++ p^.down
  if val == i then penv e else return 0  {-change this, lesser envido should be valid-}
  where
    penv :: Envido -> IOS Int
    penv Envido          = return 2
    penv EnvidoEnvido    = return 4
    penv RealEnvido      = return 5
    penv FaltaEnvido     = use points >>= return . (30 -) . uncurry max
        
  
tresMismoPalo :: Card -> Card -> Card -> Bool
tresMismoPalo a b c = suit a == suit b && suit a == suit c

dosMismoPalo :: Card -> Card -> Card -> Maybe (Card, Card)
dosMismoPalo a b c | suit a == suit b  = Just (a,b)
                   | suit c == suit b  = Just (b,c)
                   | suit a == suit c  = Just (a,c)
                   | otherwise         = Nothing

envidoScore :: [Card] -> Int
envidoScore xs = eS (xs !! 0) (xs !!1) (xs!!2)
  where eS a b c | tresMismoPalo a b c = max (sumCards (a, b)) (max (sumCards (b, c)) (sumCards (a, c)))
                 | isJust (dosMismoPalo a b c) = sumCards $ fromJust (dosMismoPalo a b c)
                 | otherwise = max (valueCard (num a)) $ max (valueCard (num b)) (valueCard (num c))

sumCards :: (Card, Card) -> Int
sumCards (a,b) = 20 + valueCard (num a) + valueCard (num b)

valueCard :: Number -> Int
valueCard Ancho   = 1
valueCard Dos     = 2
valueCard Tres    = 3
valueCard Cuatro  = 4
valueCard Cinco   = 5
valueCard Seis    = 6
valueCard Siete   = 7
valueCard Sota    = 0
valueCard Caballo = 0
valueCard Rey     = 0
