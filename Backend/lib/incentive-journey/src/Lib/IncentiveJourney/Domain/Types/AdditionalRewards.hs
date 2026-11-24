{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Typed view over the generic @CohortJourneyMapping.additionalRewards@ JSON blob.
-- Each ad-hoc reward layered on a campaign is one optional, namespaced key, e.g.
-- @{"riderCancellationCompensation":{"enabled":true,"payout":30}}@. Storage stays JSON
-- (one column, future rewards slot in without a migration); the read side decodes to
-- these records so callers stay type-safe.
module Lib.IncentiveJourney.Domain.Types.AdditionalRewards
  ( AdditionalRewards (..),
    RiderCancellationCompensation (..),
    parseAdditionalRewards,
    parseRiderCancellationCompensation,
  )
where

import qualified Data.Aeson as A
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)

-- | All ad-hoc rewards that can be configured on a cohort campaign.
newtype AdditionalRewards = AdditionalRewards
  { riderCancellationCompensation :: Maybe RiderCancellationCompensation
  }
  deriving (Generic, Show, Eq)

instance FromJSON AdditionalRewards where
  parseJSON = A.withObject "AdditionalRewards" $ \o ->
    AdditionalRewards <$> o A..:? "riderCancellationCompensation"

instance ToJSON AdditionalRewards where
  toJSON (AdditionalRewards rcc) =
    A.object ["riderCancellationCompensation" A..= rcc]

-- | Wallet compensation paid to a driver in this cohort when a rider-fault cancel occurs.
-- @payout@ is the ₹ amount credited.
data RiderCancellationCompensation = RiderCancellationCompensation
  { enabled :: Bool,
    payout :: HighPrecMoney
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

-- | Decode the whole @additionalRewards@ blob; Nothing when absent or malformed.
parseAdditionalRewards :: Maybe A.Value -> Maybe AdditionalRewards
parseAdditionalRewards mbVal = do
  val <- mbVal
  case A.fromJSON val of
    A.Success ar -> Just ar
    A.Error _ -> Nothing

-- | Convenience: pull just the riderCancellationCompensation config out of the blob.
parseRiderCancellationCompensation :: Maybe A.Value -> Maybe RiderCancellationCompensation
parseRiderCancellationCompensation mbVal =
  parseAdditionalRewards mbVal >>= riderCancellationCompensation
