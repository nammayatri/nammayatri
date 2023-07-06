{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Merchant where

import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Merchant as Common
import qualified Domain.Action.Dashboard.Merchant as DMerchant
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, withFlowHandlerAPI)
import Servant hiding (Unauthorized, throwError)
import SharedLogic.Merchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as SMOC

type API =
  "merchant"
    :> ( Common.MerchantUpdateAPI
           :<|> Common.ServiceUsageConfigAPI
           :<|> Common.MapsServiceConfigUpdateAPI
           :<|> Common.MapsServiceUsageConfigUpdateAPI
           :<|> Common.SmsServiceConfigUpdateAPI
           :<|> Common.SmsServiceUsageConfigUpdateAPI
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  merchantUpdate merchantId
    :<|> serviceUsageConfig merchantId
    :<|> mapsServiceConfigUpdate merchantId
    :<|> mapsServiceUsageConfigUpdate merchantId
    :<|> smsServiceConfigUpdate merchantId
    :<|> smsServiceUsageConfigUpdate merchantId

merchantUpdate ::
  ShortId DM.Merchant ->
  Common.MerchantUpdateReq ->
  FlowHandler APISuccess
merchantUpdate merchantShortId = withFlowHandlerAPI . DMerchant.merchantUpdate merchantShortId

serviceUsageConfig ::
  ShortId DM.Merchant ->
  FlowHandler Common.ServiceUsageConfigRes
serviceUsageConfig merchantShortId = withFlowHandlerAPI $ do
  m <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- SMOC.findByMerchantId m.id >>= fromMaybeM (MerchantOperatingCityNotFound m.id.getId)
  DMerchant.serviceUsageConfig merchantOperatingCity.id

mapsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Common.MapsServiceConfigUpdateReq ->
  FlowHandler APISuccess
mapsServiceConfigUpdate merchantShortId req = withFlowHandlerAPI $ do
  m <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- SMOC.findByMerchantId m.id >>= fromMaybeM (MerchantOperatingCityNotFound m.id.getId)
  DMerchant.mapsServiceConfigUpdate merchantOperatingCity.id req

mapsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  Common.MapsServiceUsageConfigUpdateReq ->
  FlowHandler APISuccess
mapsServiceUsageConfigUpdate merchantShortId req = withFlowHandlerAPI $ do
  m <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- SMOC.findByMerchantId m.id >>= fromMaybeM (MerchantOperatingCityNotFound m.id.getId)
  DMerchant.mapsServiceUsageConfigUpdate merchantOperatingCity.id req

smsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Common.SmsServiceConfigUpdateReq ->
  FlowHandler APISuccess
smsServiceConfigUpdate merchantShortId req = withFlowHandlerAPI $ do
  m <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- SMOC.findByMerchantId m.id >>= fromMaybeM (MerchantOperatingCityNotFound m.id.getId)
  DMerchant.smsServiceConfigUpdate merchantOperatingCity.id req

smsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  Common.SmsServiceUsageConfigUpdateReq ->
  FlowHandler APISuccess
smsServiceUsageConfigUpdate merchantShortId req = withFlowHandlerAPI $ do
  m <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- SMOC.findByMerchantId m.id >>= fromMaybeM (MerchantOperatingCityNotFound m.id.getId)
  DMerchant.smsServiceUsageConfigUpdate merchantOperatingCity.id req
