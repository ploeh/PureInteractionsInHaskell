module Wizard (tryReserve) where

import Text.Read (readMaybe)
import Control.Applicative ((<$>))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Free (FreeT, liftF)
import CommandLine
import ReservationsApi

readParse :: Read a => String -> String -> CommandLineProgram a
readParse prompt errorMessage = do
  writeLine prompt
  l <- readLine
  case readMaybe l of
    Just dt -> return dt
    Nothing -> do
      writeLine errorMessage
      readParse prompt errorMessage

readAnything :: String -> CommandLineProgram String
readAnything prompt = do
  writeLine prompt
  readLine

tryReserve :: FreeT ReservationsApiProgram CommandLineProgram ()
tryReserve = do
  q <- lift $ readParse "Please enter number of diners:" "Not an Integer."
  d <- lift $ readParse "Please enter your desired date:" "Not a date."
  availableSeats <- liftF $ (sum . fmap seatsLeft) <$> getSlots d
  if availableSeats < q
    then lift $ writeLine $ "Only " ++ show availableSeats ++ " remaining seats."
    else do
      n <- lift $ readAnything "Please enter your name:"
      e <- lift $ readAnything "Please enter your email address:"
      liftF $ postReservation Reservation
        { reservationDate = d
        , reservationName = n
        , reservationEmail = e
        , reservationQuantity = q }
