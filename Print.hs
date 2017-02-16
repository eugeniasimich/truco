{-# LANGUAGE FlexibleContexts #-}

module Print where

import        Prelude hiding (round, last)
import        System.Console.ANSI
import        Control.Monad.State
import        Control.Monad.IO.Class (liftIO, MonadIO)
import        Control.Lens hiding (indexed)
import        Data.List (sortBy)
import        Data.String.Utils (maybeRead)

import        Truco.Types
import        Extras 


printIOS :: String -> IOS ()
printIOS s = liftIO $ putStrLn s


switchToPlayer :: MonadIO m => Player -> m ()
switchToPlayer p = liftIO $ do
  clearScreenForPlayer p
  setCursorPosition 0 0
  putStrLn $ "Próximo jugador: " ++ p^.name 
  putStrLn "Presione ENTER para continuar"
  _ <- getLine
  setCursorPosition 0 0 >> clearFromCursorToScreenEnd
  printInBox (p^.name)


printInBox :: MonadIO m => String -> m ()
printInBox s = liftIO $ do
  let len = length s
      l = floor $ (30 - fromIntegral len) / 2 
      r = ceiling $ (30 - fromIntegral len) / 2 
  putStrLn $ replicate 40 '-'
  putStrLn $ replicate l '-' ++ replicate 5 ' ' ++ s ++ replicate 5 ' ' ++ replicate r '-'
  putStrLn $ replicate 40 '-'
  putStrLn ""

clearScreenForPlayer :: MonadIO m => Player -> m ()
clearScreenForPlayer p =
  liftIO $ setSGR [SetColor Background Dull (p^.color)] >> clearScreen

printPlayerState :: (MonadState Game m, MonadIO m) => Player -> m ()
printPlayerState p = liftIO $ do
  putStrLn $ "Tus cartas en mano: " ++ show (p^.cards)
  putStrLn $ "Tus cartas en mesa: " ++ show (p^.down)

printRoundState :: TeamId -> IOS ()
printRoundState tid = do
  r <- use (hand.round)
  printHandState tid
  printIOS $ show r ++ "° Mano" 
  printRoundCards

printGameState :: TeamId ->  IOS ()
printGameState tid = do
  ourpoints <- teamPoints tid
  theirpoints <- teamPoints (oponent tid)
  printIOS $ "Nosotros " ++ show ourpoints ++ " | Ellos " ++ show theirpoints

printHandState :: TeamId -> IOS ()
printHandState tid =
  uses (hand.formerRounds) (sortBy compareFst) >>= mapM_ pfr
  where pfr (i,wt) = liftIO $ putStr $ "Mano " ++ show i ++ cond tid wt
        cond _ Nothing  = " parda. "
        cond a (Just b) =  if a == b then " ganada. " else " perdida. "

printRoundCards :: IOS ()
printRoundCards = do
  cs <- use (hand.roundCards)
  unless (null cs) $ do
      liftIO $ putStr "Cartas en mesa "
      mapM_ _printPlayerCard (reverse cs)
      printIOS ""
  where _printPlayerCard (pid,c) = do
                        p <- getPlayer pid
                        liftIO $ putStr ( " | "  ++ p^.name ++ " ")
                        printCard c

printCard :: Card -> IOS ()
printCard = liftIO . putStr . show

printOptions :: Show a => [a] -> IOS ()
printOptions o = printIOS "Elige una opción:" >> mapM_ (printIOS . _showOpt) _opts
  where _opts           = indexed o
        _showOpt (i,j)  = show i ++ ": " ++ show j


printEndOfGame :: TeamId -> IOS ()
printEndOfGame tid = do
  liftIO $ clearScreen >> setCursorPosition 0 0 
  ps <- teamPlayersNames tid
  case ps of
    _   -> printIOS "empty player list?"
    [p] -> printIOS $ "¡Ganó " ++ p ++ "!"
    _   -> printIOS $ "¡Ganaron " ++ _showPlayers ps ++ "!"
    where _showPlayers [x,y] = x ++ " y " ++ y
          _showPlayers (x:xs) = x ++ ", " ++ _showPlayers xs

teamPlayersNames :: TeamId -> IOS [String]
teamPlayersNames i = uses players (filter $ (== i) . (^.team)) >>= return . map (^.name)

parseOption   :: Int -> IOS Int
parseOption last = do
  c <- liftIO getLine
  case maybeRead c :: Maybe Int of
    Nothing -> _invalid
    Just i -> if i > last
              then _invalid
              else return i
  where _invalid = liftIO (putStrLn "Valor inválido") >> parseOption last

askChallenge :: MonadIO m => Player -> String -> m ()
askChallenge p s = liftIO $ do
  clearScreenForPlayer p
  setCursorPosition 0 0
  putStrLn $ "Han cantado " ++ s
  putStrLn $ p^.name ++ " debe responder al reto."
  putStrLn "Presione ENTER para continuar"
  _ <- getLine
  setCursorPosition 0 0 >> clearFromCursorToScreenEnd
