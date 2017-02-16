module Truco.Player where

import        Control.Lens hiding (indexed)
import        Prelude hiding (round)
import        Extra (dupe, (***))
import        Control.Monad
import        Data.List (delete)

import        Truco.Types
import        Truco.Envido
import        Extras
import        Print
----------------------------------------------

startPlayerAction :: PlayerId -> IOS ()
startPlayerAction i = getPlayer i >>= playerAction

playerAction  :: Player -> IOS ()
playerAction p = do
  almazo <- uses (hand.truco) isAlMazo
  unless almazo $ do
    switchToPlayer p
    printGameState (p^.team)
    printRoundState (p^.team)
    os <- getOptions p
    printOptions (map fst os)
    i <- parseOption (length os)
    snd $ os !! (i - 1)

-- OPTIONS --

getOptions :: Player -> IOS Handler
getOptions p = optEnvido p +++ optTruco p +++ optCard p
  where (+++) = liftM2 (++)

optEnvido :: Player -> IOS Handler
optEnvido p = do
  t <- use (hand.truco)
  e <- use (hand.envido)
  return $ if trucoNotPlayed t && envidoNotPlayed e
           then _env Envido ++ _env FaltaEnvido
           else []
  where _env x = [(E x, envidoHandler p x >>  playerAction p)]

optTruco  :: Player -> IOS Handler
optTruco p = do
  tc <- use (hand.truco)
  return $ trucoChallenge [] (_tru Truco) _higher [] tc
  where
    _tru c              = [(T c, trucoHandler p c >> playerAction p)]
    _myTeamChallenged x = x == p^.team
    _higher tid t       = if _myTeamChallenged tid then [] else _tru (succ t)

optCard   :: Player -> IOS Handler
optCard p  = return $ map ((C *** playCard p) . dupe) (p^.cards)

--------------------------------

playCard :: Player -> Card -> IOS ()
playCard p c = do
  pid <- getPlayerId p
  players. (ix pid) . cards %= delete c
  players. (ix pid) . down  %= (:) c
  hand.roundCards %= (:) (pid, c)

--------------------------------

trucoHandler :: Player -> Truco -> IOS ()
trucoHandler = handler getTrucoResponse handleTrucoAnswer

envidoHandler :: Player -> Envido -> IOS ()
envidoHandler = handler getEnvidoResponse handleEnvidoAnswer

handler getresp handleansw p c = 
  getresp (oponent $ p^.team) c >>= handleansw (p^.team) c

getTrucoResponse :: TeamId -> Truco -> IOS Answer
getTrucoResponse tid ValeCuatro = getResponse tid ValeCuatro []
getTrucoResponse tid t          = getResponse tid t [CT (succ t)]

getEnvidoResponse :: TeamId -> Envido -> IOS Answer
getEnvidoResponse tid FaltaEnvido = getResponse tid FaltaEnvido []
getEnvidoResponse tid RealEnvido  = getResponse tid RealEnvido [CE FaltaEnvido]
getEnvidoResponse tid e           = getResponse tid e [CE (succ e), CE FaltaEnvido]

getResponse :: Show a => TeamId -> a -> [Answer]-> IOS Answer
getResponse tid a la = do
  p <- getCaptain tid
  askChallenge p (show a)
  printInBox $ p^.name
  let opts = [Y,N] ++ la
  printPlayerState p 
  printOptions opts
  i <- parseOption (length opts)
  return $ opts !! (i-1)

handleTrucoAnswer :: TeamId -> Truco -> Answer -> IOS ()
handleTrucoAnswer = handleAnswer trucoRejected trucoAccepted

handleEnvidoAnswer :: TeamId -> Envido -> Answer -> IOS ()
handleEnvidoAnswer = handleAnswer envidoRejected (const envidoRoundPoints)

handleAnswer negative _ tid c N      = negative tid c
handleAnswer _ positive tid c Y      = positive tid c
handleAnswer _ _        tid _ (CE r) = higherChallenge tid r getEnvidoResponse handleEnvidoAnswer
handleAnswer _ _        tid _ (CT r) = higherChallenge tid r getTrucoResponse  handleTrucoAnswer

higherChallenge tid r getresp handleansw = getresp tid r >>= handleansw (oponent tid) r
        
trucoAccepted :: TeamId -> Truco -> IOS ()
trucoAccepted tid t = hand.truco .= (Right t, Just tid) 

trucoRejected :: TeamId -> Truco -> IOS ()
trucoRejected tid t = hand.truco .= (Left (AlMazo t), Just tid)

envidoRejected :: TeamId -> Envido -> IOS ()
envidoRejected tid e = hand.envido .= Just (e, Rejected, tid)

trucoChallenge :: c -> c -> (TeamId -> Truco -> c) -> c -> TrucoChallenge -> c
trucoChallenge am _ _ _ (Left (AlMazo _),_) = am
trucoChallenge _ np _ _ (Left NotPlayed ,_) = np
trucoChallenge _ _ _ vc (Right ValeCuatro,_) = vc
trucoChallenge _ _ tr _ (Right t, Just tid) = tr tid t

