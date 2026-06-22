{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Utils
  ( getStaticCustomerId,
    getPureStaticCustomerId,
    getPersonUdf1,
  )
where

import qualified Data.Time as Time
import qualified Domain.Types.Person as DP
import EulerHS.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Types.Version as Version
import Kernel.Utils.Common
import qualified Kernel.Utils.UUID as UUID
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)

-- | udf1 (user defined field 1) sent in the Juspay session/createOrder request.
-- Resolves to the customer's device id based on the client OS:
--   iOS     -> person.deviceId
--   Android -> person.androidId
-- Returns Nothing when the device/OS is unknown.
getPersonUdf1 :: Applicative m => DP.Person -> m (Maybe Text)
getPersonUdf1 person =
  pure $ case person.clientDevice of
    Just device -> case device.deviceType of
      Version.IOS -> person.deviceId
      Version.ANDROID -> person.androidId
    Nothing -> Nothing

-- | Pure version of static customer ID generation.
-- Generates the ID deterministically from phone and merchantId without any config lookups.
getPureStaticCustomerId :: DP.Person -> Text -> Text
getPureStaticCustomerId person phone =
  let key = phone <> ":" <> person.merchantId.getId
   in UUID.generateStaticUUID key

getStaticCustomerId :: (MonadFlow m, EsqDBReplicaFlow m r, EsqDBFlow m r, CacheFlow m r) => DP.Person -> Text -> m Text
getStaticCustomerId person phone = do
  mbRiderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId})
  let mbThreshold = mbRiderConfig >>= (.staticCustomerIdThresholdDay)
  case mbThreshold of
    Just threshold ->
      if Time.utctDay person.createdAt > threshold
        then do
          let key = phone <> ":" <> person.merchantId.getId
          return $ UUID.generateStaticUUID key
        else return person.id.getId
    Nothing -> return person.id.getId
