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
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import qualified Lib.Types.SpecialLocation as SL
import Servant hiding (Unauthorized, throwError)
import Storage.Beam.SystemConfigs ()

type API =
  "merchant"
    :> ( Common.MerchantUpdateAPI
           :<|> Common.ServiceUsageConfigAPI
           :<|> Common.MapsServiceConfigUpdateAPI
           :<|> Common.MapsServiceUsageConfigUpdateAPI
           :<|> Common.SmsServiceConfigUpdateAPI
           :<|> Common.SmsServiceUsageConfigUpdateAPI
           :<|> Common.CreateMerchantOperatingCityAPIT
           :<|> Common.UpsertSpecialLocationAPIT
           :<|> Common.DeleteSpecialLocationAPI
           :<|> Common.UpsertSpecialLocationGateAPIT
           :<|> Common.DeleteSpecialLocationGateAPI
       )

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
handler merchantId city =
  merchantUpdate merchantId city
    :<|> serviceUsageConfig merchantId city
    :<|> mapsServiceConfigUpdate merchantId city
    :<|> mapsServiceUsageConfigUpdate merchantId city
    :<|> smsServiceConfigUpdate merchantId city
    :<|> smsServiceUsageConfigUpdate merchantId city
    :<|> createMerchantOperatingCity merchantId city
    :<|> upsertSpecialLocation merchantId city
    :<|> deleteSpecialLocation merchantId city
    :<|> upsertSpecialLocationGate merchantId city
    :<|> deleteSpecialLocationGate merchantId city

merchantUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.MerchantUpdateReq ->
  FlowHandler APISuccess
merchantUpdate merchantShortId city = withFlowHandlerAPI . DMerchant.merchantUpdate merchantShortId city

serviceUsageConfig ::
  ShortId DM.Merchant ->
  Context.City ->
  FlowHandler Common.ServiceUsageConfigRes
serviceUsageConfig merchantShortId = withFlowHandlerAPI . DMerchant.serviceUsageConfig merchantShortId

mapsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.MapsServiceConfigUpdateReq ->
  FlowHandler APISuccess
mapsServiceConfigUpdate merchantShortId city = withFlowHandlerAPI . DMerchant.mapsServiceConfigUpdate merchantShortId city

mapsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.MapsServiceUsageConfigUpdateReq ->
  FlowHandler APISuccess
mapsServiceUsageConfigUpdate merchantShortId city = withFlowHandlerAPI . DMerchant.mapsServiceUsageConfigUpdate merchantShortId city

smsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.SmsServiceConfigUpdateReq ->
  FlowHandler APISuccess
smsServiceConfigUpdate merchantShortId city = withFlowHandlerAPI . DMerchant.smsServiceConfigUpdate merchantShortId city

smsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.SmsServiceUsageConfigUpdateReq ->
  FlowHandler APISuccess
smsServiceUsageConfigUpdate merchantShortId city = withFlowHandlerAPI . DMerchant.smsServiceUsageConfigUpdate merchantShortId city

createMerchantOperatingCity ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.CreateMerchantOperatingCityReqT ->
  FlowHandler Common.CreateMerchantOperatingCityRes
createMerchantOperatingCity merchantShortId city = withFlowHandlerAPI . DMerchant.createMerchantOperatingCity merchantShortId city

upsertSpecialLocation :: ShortId DM.Merchant -> Context.City -> Maybe (Id SL.SpecialLocation) -> Common.UpsertSpecialLocationReqT -> FlowHandler APISuccess
upsertSpecialLocation merchantShortId opCity mbSpecialLocationId = withFlowHandlerAPI . DMerchant.upsertSpecialLocation merchantShortId opCity mbSpecialLocationId

deleteSpecialLocation :: ShortId DM.Merchant -> Context.City -> Id SL.SpecialLocation -> FlowHandler APISuccess
deleteSpecialLocation merchantShortId opCity = withFlowHandlerAPI . DMerchant.deleteSpecialLocation merchantShortId opCity

upsertSpecialLocationGate :: ShortId DM.Merchant -> Context.City -> Id SL.SpecialLocation -> Common.UpsertSpecialLocationGateReqT -> FlowHandler APISuccess
upsertSpecialLocationGate merchantShortId opCity specialLocationId = withFlowHandlerAPI . DMerchant.upsertSpecialLocationGate merchantShortId opCity specialLocationId

deleteSpecialLocationGate :: ShortId DM.Merchant -> Context.City -> Id SL.SpecialLocation -> Text -> FlowHandler APISuccess
deleteSpecialLocationGate merchantShortId opCity specialLocationId = withFlowHandlerAPI . DMerchant.deleteSpecialLocationGate merchantShortId opCity specialLocationId
