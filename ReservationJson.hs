{-# LANGUAGE DeriveGeneric #-}
module ReservationJson where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import ReservationsApi

data JReservation = JReservation
  { date :: String
  , name :: String
  , email :: String
  , quantity :: Int }
  deriving (Generic, Show, Eq)
instance ToJSON JReservation
instance FromJSON JReservation

fromReservation Reservation
  { reservationDate = d
  , reservationName = n
  , reservationEmail = e
  , reservationQuantity = q } =
  JReservation (show d) n e q
