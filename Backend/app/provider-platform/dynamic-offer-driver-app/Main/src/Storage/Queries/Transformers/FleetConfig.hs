module Storage.Queries.Transformers.FleetConfig (decodePreferencePolicy) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import qualified Domain.Types.DriverPreferencePolicy as DDPP
import Kernel.Prelude

-- | Tolerant decode of the stored fleet_config.preference_policy column.
--
-- Each entry of "locked" and "hidden" is decoded on its own and dropped when it does not parse (an unknown
-- preference name or value tag, e.g. written by a newer release), so one bad entry never loses the whole policy.
-- A missing list reads as empty; anything that is not a JSON object reads as no policy.
decodePreferencePolicy :: A.Value -> Maybe DDPP.PreferencePolicy
decodePreferencePolicy = \case
  A.Object obj ->
    Just
      DDPP.PreferencePolicy
        { locked = decodeEach (KM.lookup "locked" obj),
          hidden = decodeEach (KM.lookup "hidden" obj)
        }
  _ -> Nothing
  where
    decodeEach :: A.FromJSON a => Maybe A.Value -> [a]
    decodeEach = \case
      Just (A.Array entries) -> mapMaybe fromResult (V.toList entries)
      _ -> []
    fromResult :: A.FromJSON a => A.Value -> Maybe a
    fromResult v = case A.fromJSON v of
      A.Success a -> Just a
      A.Error _ -> Nothing
