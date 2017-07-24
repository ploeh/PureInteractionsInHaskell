module ReservationsHttpClient where

import Data.Time.Calendar (toGregorian)
import Data.Time.LocalTime (ZonedTime, localDay, zonedTimeToLocalTime)
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow)
import Text.Printf (printf)
import Network.HTTP.Simple
import ReservationsApi (Slot, Reservation)
import qualified OpeningJson as OJ
import qualified ReservationJson as RJ

baseAddress = "http://localhost:56268"

getSlots :: (MonadIO m, MonadThrow m) => ZonedTime -> m [Slot]
getSlots zt = do
  let (y, m, d) = toGregorian $ localDay $ zonedTimeToLocalTime zt
  request <- parseRequest $ printf "%s/availability/%d/%d/%d" baseAddress y m d
  response <- httpJSON request
  return $ fmap OJ.toSlot $ OJ.openings $ getResponseBody response

postReservation :: Reservation -> IO ()
postReservation r = do
  request <- setRequestBodyJSON (RJ.fromReservation r)
    <$> parseRequest (printf "POST %s/reservations" baseAddress)
  response <- httpNoBody request
  print $ getResponseStatus response
