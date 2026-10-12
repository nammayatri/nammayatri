-- | Shared between the DRIVER_POOL_STATE writer and the SESSIONIZER: the event
-- the driver app publishes, the Redis layout of the sessionizer's state store,
-- and the small per-driver state the sessionizer tags pings with.
module Processor.DriverPoolState.Types
  ( DriverPoolStateEvent (..),
    DriverStateSnapshot (..),
    snapshotFromFields,
    encodeMember,
    decodeMember,
    currentKey,
    historyKey,
    writerWatermarkKey,
    sessionizerWatermarkKey,
    stateExpiry,
  )
where

import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Kernel.Prelude

-- | One message on the driver-pool-state topic: only the fields that changed,
-- under their DriverPoolData names.
data DriverPoolStateEvent = DriverPoolStateEvent
  { driverId :: Text,
    ts :: Integer,
    changes :: A.Object
  }
  deriving (Generic, Show, FromJSON)

-- | The fields the sessionizer's outputs depend on. A new history entry is
-- written only when one of these changes.
data DriverStateSnapshot = DriverStateSnapshot
  { mode :: Maybe Text,
    active :: Bool,
    onRide :: Bool,
    enabled :: Bool,
    blocked :: Bool,
    subscribed :: Bool,
    tiers :: [Text]
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

-- | Build the snapshot from the driver's current-state hash (field -> JSON value).
-- Missing fields fall back to the most conservative value.
snapshotFromFields :: HM.HashMap Text A.Value -> DriverStateSnapshot
snapshotFromFields m =
  DriverStateSnapshot
    { mode = textField "mode",
      active = boolField "active",
      onRide = boolField "onRide",
      enabled = boolField "enabled",
      blocked = boolField "blocked",
      subscribed = boolField "subscribed",
      tiers = case HM.lookup "selectedServiceTiers" m of
        Just (A.Array xs) -> [t | A.String t <- toList xs]
        _ -> []
    }
  where
    textField k = case HM.lookup k m of
      Just (A.String t) -> Just t
      _ -> Nothing
    boolField k = case HM.lookup k m of
      Just (A.Bool b) -> b
      _ -> False

-- | Hash: DriverPoolData field -> JSON value, the merged current state.
currentKey :: Text -> Text
currentKey did = "driver_state:current:" <> did

-- | Sorted set: score = event ts (epoch ms), member = "<ts>:<snapshot json>".
historyKey :: Text -> Text
historyKey did = "driver_state:history:" <> did

-- | Epoch ms up to which the writer has applied every event.
writerWatermarkKey :: Text
writerWatermarkKey = "state_writer:watermark"

-- | Epoch ms up to which the sessionizer has committed pings (T in the plan).
sessionizerWatermarkKey :: Text
sessionizerWatermarkKey = "sessionizer:watermark"

-- | A driver with no state change for this long drops out; their next event
-- re-seeds them from the DB.
stateExpiry :: Int
stateExpiry = 30 * 24 * 60 * 60

-- | Member text "<ts>:<snapshot json>". The ts prefix keeps two identical
-- snapshots at different times as separate entries.
encodeMember :: Integer -> DriverStateSnapshot -> Text
encodeMember t s = show t <> ":" <> decodeUtf8 (LBS.toStrict (A.encode s))

decodeMember :: ByteString -> Maybe (Integer, DriverStateSnapshot)
decodeMember raw = do
  memberText :: Text <- A.decode (LBS.fromStrict raw)
  let (tsPart, rest) = T.breakOn ":" memberText
  t <- readMaybe (T.unpack tsPart)
  s <- A.decode (LBS.fromStrict (encodeUtf8 (T.drop 1 rest)))
  pure (t, s)
