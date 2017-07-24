module Main where

import Control.Monad.Trans.Free (FreeT, runFree, runFreeT, FreeF(..))
import CommandLine
import ReservationsApi
import Wizard
import qualified ReservationsHttpClient as HttpClient

interpretCommandLine :: CommandLineProgram a -> IO a
interpretCommandLine program =
  case runFree program of
    Pure r -> return r
    Free (ReadLine next) -> do
      line <- getLine
      interpretCommandLine $ next line
    Free (WriteLine line next) -> do
      putStrLn line
      interpretCommandLine next

interpretReservationsApi :: ReservationsApiProgram a -> IO a
interpretReservationsApi program =
  case runFree program of
    Pure x -> return x
    Free (GetSlots zt next) -> do
      slots <- HttpClient.getSlots zt
      interpretReservationsApi $ next slots
    Free (PostReservation r next) -> do
      HttpClient.postReservation r
      interpretReservationsApi next

interpret :: FreeT ReservationsApiProgram CommandLineProgram a -> IO a
interpret program = do
  r <- interpretCommandLine $ runFreeT program
  case r of
    Pure x -> return x
    Free p -> do
      y <- interpretReservationsApi p
      interpret y

main :: IO ()
main = interpret tryReserve
