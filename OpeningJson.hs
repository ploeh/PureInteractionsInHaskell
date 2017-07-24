{-# LANGUAGE DeriveGeneric #-}
module OpeningJson where

import Data.Time.LocalTime (ZonedTime(..), LocalTime(..), midnight, utc)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import ReservationsApi (Slot(..))

data JOpening = JOpening { date :: String, seats :: Int }
  deriving (Generic, Show, Eq)
instance ToJSON JOpening
instance FromJSON JOpening

toSlot JOpening { date = d, seats = s } =
  Slot { slotDate = ZonedTime (LocalTime (read d) midnight) utc, seatsLeft = s }

newtype JOpenings = JOpenings { openings :: [JOpening] }
  deriving (Generic, Show, Eq)
instance ToJSON JOpenings
instance FromJSON JOpenings
