{-# LANGUAGE TemplateHaskell #-}

module Truco.Types where

import        Control.Monad.State
import        Control.Lens
import        System.Console.ANSI
--------------------------------

data Number   = Tres | Dos | Ancho | Rey | Caballo | Sota | Siete | Seis | Cinco | Cuatro
  deriving (Eq, Ord, Enum, Bounded)
instance Show Number where
  show x = case x of
    Tres      -> " 3"
    Dos       -> " 2"
    Ancho     -> " 1"
    Rey       -> "12"
    Caballo   -> "11"
    Sota      -> "10"
    Siete     -> " 7"
    Seis      -> " 6"
    Cinco     -> " 5"
    Cuatro    -> " 4"

data Palo     = Espada | Basto | Oro | Copa
  deriving (Eq, Enum, Bounded)
instance Show Palo where
  show x = case x of
    Espada    -> "🗡"
    Basto     -> "🍗"
    Oro       -> "☼"
    Copa      -> "🍷"

data Card     = Card
  { num       :: Number
  , suit      :: Palo }
  deriving (Eq)
instance Show Card where
  show (Card n s) = show n ++ show s
instance Ord Card where
  compare x y =
    if num x == num y && suit x == suit y then EQ else cmp' x y
      where cmp' (Card Ancho Espada)  _  = GT
            cmp' _ (Card Ancho Espada)   = LT
            cmp' (Card Ancho Basto)   _  = GT
            cmp' _ (Card Ancho Basto)    = LT
            cmp' (Card Siete Espada)  _  = GT
            cmp' _ (Card Siete Espada)   = LT
            cmp' (Card Siete Oro)     _  = GT
            cmp' _ (Card Siete Oro)      = LT
            cmp' (Card a _) (Card c _)  = compare c a

type Name = String

data Player   = Player
  { _name     :: Name     --player name
  , _cards    :: [Card]   --cards in hand
  , _down     :: [Card]   --cards on table
  , _team     :: TeamId
  , _color    :: Color
  } deriving (Eq)
instance Show Player where
  show (Player n c _ _ _) = show n ++ ":" ++ show c

type PlayerId = Int

data TeamId   = Nos | Ellos deriving (Show, Eq, Enum)

data Action   = Rejected   --For the envido, challenged rejected, points won by team TeamId.
              | Won Int PlayerId    --For the envido, challenge won by playerId, which claimed Int points.

data Truco = Truco | Retruco | ValeCuatro
  deriving (Eq,Enum)
instance Show Truco where
  show Truco = "truco"
  show Retruco = "retruco"
  show ValeCuatro = "vale cuatro"

type TrucoChallenge = (Either NotPlayed Truco, Maybe TeamId)  --truco challenged by teamId 

data NotPlayed = AlMazo Truco | NotPlayed
  deriving (Eq, Show)
                 
data Envido = Envido | EnvidoEnvido | RealEnvido | FaltaEnvido
  deriving (Eq,Enum)
instance Show Envido where
  show Envido = "envido"
  show EnvidoEnvido = "envido"
  show RealEnvido = "real envido"
  show FaltaEnvido = "falta envido"
  
type EnvidoChallenge = Maybe (Envido, Action, TeamId) -- who won or challenged (a rejected) envido
  
type Round    = Int

data Hand     = Hand
  { _truco              :: TrucoChallenge         --truco challenge already? 
  , _envido             :: EnvidoChallenge        --envido challenge already?
  , _round              :: Round                  --1st, 2nd, 3rd round?
  , _roundCards         :: [(PlayerId,Card)]      --Cards in play
  , _handStarter        :: PlayerId               --player who start the hand
  , _roundStarter       :: PlayerId               --player who starts the round
  , _formerRounds       :: [(Round,Maybe TeamId)] }     --which team won each round
  
data Game     = Game
  { _players  :: [Player]          --Players
  , _hand     :: Hand              --current hand
  , _points   :: (Int, Int) }      --points by team 

makeLenses    ''Game
makeLenses    ''Player
makeLenses    ''Hand

type IOS a = StateT Game IO a


data Option = E Envido | T Truco | C Card 
instance Show Option where
  show (E e) = "Cantar " ++ show e
  show (T t) = "Cantar " ++ show t
  show (C c) = "Jugar carta " ++ show c

type Handler = [(Option, IOS ())]

data Answer = CE Envido | CT Truco | Y | N
instance Show Answer where
  show (CE e) = "Cantar " ++ show e
  show (CT t) = "Cantar " ++ show t
  show Y      = "Quiero " 
  show N      = "No quiero"

type Score = Int

data EnvidoPointsOption = Cantar | SonBuenas | Mesa deriving (Show,Eq)

