{-# LANGUAGE DeriveFunctor #-}
module ReservationsApi where

import Data.Time.LocalTime (ZonedTime (..))
import Control.Monad.Trans.Free (Free, liftF)

data Slot = Slot { slotDate :: ZonedTime, seatsLeft :: Int } deriving (Show)

data Reservation =
  Reservation { reservationDate :: ZonedTime
              , reservationName :: String
              , reservationEmail :: String
              , reservationQuantity :: Int }
              deriving (Show)

data ReservationsApiInstruction next =
    GetSlots ZonedTime ([Slot] -> next)
  | PostReservation Reservation next
  deriving (Functor)

type ReservationsApiProgram = Free ReservationsApiInstruction

getSlots :: ZonedTime -> ReservationsApiProgram [Slot]
getSlots d = liftF (GetSlots d id)

postReservation :: Reservation -> ReservationsApiProgram ()
postReservation r = liftF (PostReservation r ())
