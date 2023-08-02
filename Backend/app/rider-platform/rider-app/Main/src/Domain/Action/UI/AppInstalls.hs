{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.AppInstalls
  ( AppInstallsReq (..),
    createAppInstallsDetails,
  )
where

import qualified Domain.Types.AppInstalls as DAppInstalls
import qualified Domain.Types.Merchant as DMerchant
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions)
import Kernel.Types.Version
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.Queries.AppInstalls as QAppInstalls

data AppInstallsReq = AppInstallsReq
  { deviceToken :: Text,
    source :: Maybe Text,
    merchantId :: ShortId DMerchant.Merchant,
    appVersion :: Maybe Version,
    bundleVersion :: Maybe Version,
    platform :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

appInstallsHitsCountKey :: Text -> Text
appInstallsHitsCountKey deviceToken = "BAP:AppInstalls:createAppInstallsDetails:" <> deviceToken <> ":hitsCount"

createAppInstallsDetails ::
  ( HasFlowEnv m r '["apiRateLimitOptions" ::: APIRateLimitOptions],
    EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r
  ) =>
  AppInstallsReq ->
  m APISuccess.APISuccess
createAppInstallsDetails req = do
  checkSlidingWindowLimit (appInstallsHitsCountKey req.deviceToken)
  merchant <- QMerchant.findByShortId req.merchantId >>= fromMaybeM (MerchantDoesNotExist req.merchantId.getShortId)
  appInstallsId <- generateGUID
  now <- getCurrentTime
  let appInstallsDetails =
        DAppInstalls.AppInstalls
          { id = appInstallsId,
            deviceToken = req.deviceToken,
            source = fromMaybe "Direct" req.source,
            merchantId = merchant.id,
            createdAt = now,
            updatedAt = now,
            appVersion = req.appVersion,
            bundleVersion = req.bundleVersion,
            platform = req.platform
          }
  QAppInstalls.upsert appInstallsDetails
  return APISuccess.Success
