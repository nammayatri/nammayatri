{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Plan where

import qualified Domain.Action.UI.Driver as Driver
import qualified Domain.Action.UI.Plan as DPlan
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverPlan as DPlan
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Plan as DPlan
import qualified Domain.Types.SubscriptionPurchase as DSP
import qualified Domain.Types.VehicleVariant as Vehicle
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  "plan"
    :> ( "list"
           :> TokenAuth
           :> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> QueryParam "vehicleVariant" Vehicle.VehicleVariant
           :> QueryParam "serviceName" DPlan.ServiceNames
           :> Get '[JSON] DPlan.PlanListAPIRes
           :<|> "suspend"
             :> TokenAuth
             :> QueryParam "serviceName" DPlan.ServiceNames
             :> Put '[JSON] APISuccess
           :<|> "resume"
             :> TokenAuth
             :> QueryParam "serviceName" DPlan.ServiceNames
             :> Put '[JSON] APISuccess
           :<|> "currentPlan"
             :> TokenAuth
             :> QueryParam "serviceName" DPlan.ServiceNames
             :> Get '[JSON] DPlan.CurrentPlanRes
           :<|> Capture "planId" (Id DPlan.Plan)
             :> "subscribe"
             :> TokenAuth
             :> QueryParam "serviceName" DPlan.ServiceNames
             :> Post '[JSON] DPlan.PlanSubscribeRes
           :<|> Capture "planId" (Id DPlan.Plan)
             :> "select"
             :> QueryParam "serviceName" DPlan.ServiceNames
             :> TokenAuth
             :> Put '[JSON] APISuccess
           :<|> "services"
             :> TokenAuth
             :> Get '[JSON] DPlan.ServicesEntity
           :<|> "subscriptionPurchases"
             :> TokenAuth
             :> QueryParam "limit" Int
             :> QueryParam "offset" Int
             :> QueryParam "status" DSP.SubscriptionPurchaseStatus
             :> Get '[JSON] DPlan.SubscriptionPurchaseListRes
       )

handler :: FlowServer API
handler =
  planList
    :<|> planSuspend
    :<|> planResume
    :<|> currentPlan
    :<|> planSubscribe
    :<|> planSelect
    :<|> planServiceLists
    :<|> subscriptionPurchases

planList :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Int -> Maybe Int -> Maybe Vehicle.VehicleVariant -> Maybe DPlan.ServiceNames -> FlowHandler DPlan.PlanListAPIRes
planList (driverId, merchantId, merchantOpCityId) mbLimit mbOffset vehicleVariant mbServiceName = withFlowHandlerAPI $ DPlan.planList (driverId, merchantId, merchantOpCityId) (fromMaybe DPlan.YATRI_SUBSCRIPTION mbServiceName) mbLimit mbOffset vehicleVariant

planSuspend :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe DPlan.ServiceNames -> FlowHandler APISuccess
planSuspend (driverId, merchantId, merchantOpCityId) mbServiceName = withFlowHandlerAPI $ DPlan.planSuspend (fromMaybe DPlan.YATRI_SUBSCRIPTION mbServiceName) False (driverId, merchantId, merchantOpCityId)

planResume :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe DPlan.ServiceNames -> FlowHandler APISuccess
planResume (driverId, merchantId, merchantOpCityId) mbServiceName = withFlowHandlerAPI $ DPlan.planResume (fromMaybe DPlan.YATRI_SUBSCRIPTION mbServiceName) (driverId, merchantId, merchantOpCityId)

currentPlan :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe DPlan.ServiceNames -> FlowHandler DPlan.CurrentPlanRes
currentPlan (driverId, merchantId, merchantOpCityId) mbServiceName = withFlowHandlerAPI $ DPlan.currentPlan (fromMaybe DPlan.YATRI_SUBSCRIPTION mbServiceName) (driverId, merchantId, merchantOpCityId)

planSubscribe :: Id DPlan.Plan -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe DPlan.ServiceNames -> FlowHandler DPlan.PlanSubscribeRes
planSubscribe planId (personId, merchantId, merchantOpCityId) mbServiceName = withFlowHandlerAPI $ do
  case mbServiceName of
    Just DPlan.PREPAID_SUBSCRIPTION -> DPlan.planSubscribe DPlan.PREPAID_SUBSCRIPTION planId (False, Nothing) (personId, merchantId, merchantOpCityId) DPlan.NoData
    _ -> do
      autoPayStatus <- fst <$> DPlan.getSubcriptionStatusWithPlan (fromMaybe DPlan.YATRI_SUBSCRIPTION mbServiceName) personId
      if autoPayStatus == Just DI.SUSPENDED
        then do
          void $ DPlan.planResume (fromMaybe DPlan.YATRI_SUBSCRIPTION mbServiceName) (personId, merchantId, merchantOpCityId)
          Driver.ClearDuesRes {..} <- Driver.clearDriverDues (personId, merchantId, merchantOpCityId) (fromMaybe DPlan.YATRI_SUBSCRIPTION mbServiceName) Nothing Nothing
          return $ DPlan.PlanSubscribeRes {..}
        else do DPlan.planSubscribe (fromMaybe DPlan.YATRI_SUBSCRIPTION mbServiceName) planId (False, Nothing) (personId, merchantId, merchantOpCityId) DPlan.NoData

planSelect :: Id DPlan.Plan -> Maybe DPlan.ServiceNames -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler APISuccess
planSelect planId mbServiceName (personId, merchantId, merchantOpCityId) = withFlowHandlerAPI $ DPlan.planSwitch (fromMaybe DPlan.YATRI_SUBSCRIPTION mbServiceName) planId (personId, merchantId, merchantOpCityId)

planServiceLists :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler DPlan.ServicesEntity
planServiceLists = withFlowHandlerAPI . DPlan.planServiceLists

subscriptionPurchases :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Int -> Maybe Int -> Maybe DSP.SubscriptionPurchaseStatus -> FlowHandler DPlan.SubscriptionPurchaseListRes
subscriptionPurchases (driverId, merchantId, merchantOpCityId) mbLimit mbOffset mbStatus =
  withFlowHandlerAPI $ DPlan.subscriptionPurchaseList (driverId, merchantId, merchantOpCityId) mbLimit mbOffset mbStatus
