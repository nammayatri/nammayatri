{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Beckn.Types.Time where

import Beckn.Utils.Dhall (FromDhall)
import Data.Aeson (Value (..))
import Data.Aeson.Types (typeMismatch)
import Data.OpenApi (ToSchema)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import qualified Data.Time as Time
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import EulerHS.Prelude
import qualified System.Clock as Clock

newtype Microseconds = Microseconds
  { getMicroseconds :: Int
  }
  deriving newtype (Show, Num, FromDhall, FromJSON, ToJSON, Integral, Real, Ord, Eq, Enum)
  deriving stock (Generic)

newtype Milliseconds = Milliseconds
  { getMilliseconds :: Int
  }
  deriving newtype (Show, Num, FromDhall, FromJSON, ToJSON, Integral, Real, Ord, Eq, Enum)
  deriving stock (Generic)

newtype Seconds = Seconds
  { getSeconds :: Int
  }
  deriving newtype (Show, Num, FromDhall, FromJSON, ToJSON, Integral, Real, Ord, Eq, Enum)
  deriving stock (Generic)

type MeasuringDuration m a = MonadClock m => m a -> m a

class Monad m => MonadTime m where
  getCurrentTime :: m UTCTime

class Monad m => MonadClock m where
  getClockTime :: m Clock.TimeSpec

instance MonadTime IO where
  getCurrentTime = Time.getCurrentTime

instance MonadClock IO where
  getClockTime = Clock.getTime Clock.Monotonic

newtype Iso8601Time = Iso8601Time {getUtcTime :: UTCTime}
  deriving (Show, Eq)
  deriving newtype (ToSchema)

instance FromJSON Iso8601Time where
  parseJSON (String s) = Iso8601Time <$> iso8601ParseM (Text.unpack s)
  parseJSON e = typeMismatch "Iso8601Time String" e

instance ToJSON Iso8601Time where
  toJSON (Iso8601Time t) = String . Text.pack $ iso8601Show t
