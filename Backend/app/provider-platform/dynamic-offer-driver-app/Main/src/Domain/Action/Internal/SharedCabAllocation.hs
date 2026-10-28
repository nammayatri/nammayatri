{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.SharedCabAllocation where

import Environment
import EulerHS.Prelude
import Kernel.Beam.Functions
import Kernel.External.Types (Language (..))
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverPool as DP
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Person as QPerson
import Tools.Notifications

sharedCabAllocationFCM :: SharedCabAllocationEntityData -> Maybe Text -> Flow APISuccess
sharedCabAllocationFCM req apiKey = do
  -- Auth first: an unknown driver, an unknown merchant and a wrong key all answer AuthBlocked, so an
  -- unauthenticated caller cannot probe for existing driver ids (PersonNotFound vs AuthBlocked).
  verified <- do
    mbPerson <- runInReplica $ QPerson.findById (Id req.driverId)
    mbMerchant <- maybe (pure Nothing) (QM.findById . (.merchantId)) mbPerson
    pure $ case (mbPerson, mbMerchant) of
      (Just p, Just m) | Just m.internalApiKey == apiKey -> Just p
      _ -> Nothing
  person <- fromMaybeM (AuthBlocked "Invalid BPP internal api key") verified
  notifySharedCabAllocation person.merchantOperatingCityId person.id person.deviceToken (fromMaybe ENGLISH person.language) req
  -- an offer is a request the driver answers, counted like a ride request so the accept and reject rates stay true
  when (isJust req.offerExpiresAt) $ DP.incrementSrdSentCount person.id
  pure Success
