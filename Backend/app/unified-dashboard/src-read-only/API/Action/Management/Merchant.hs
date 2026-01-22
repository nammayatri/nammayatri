{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Management.Merchant
  ( API,
    handler,
  )
where

import qualified "this" API.Types.Management
import qualified "this" API.Types.Management.Merchant
import qualified Data.Text
import qualified Domain.Action.Management.Merchant
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("/merchant" :> (CreateMerchantWithAdmin :<|> CreateMerchant :<|> ListMerchants :<|> ChangeMerchantEnableState))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = createMerchantWithAdmin merchantId city :<|> createMerchant merchantId city :<|> listMerchants merchantId city :<|> changeMerchantEnableState merchantId city

type CreateMerchantWithAdmin = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('CREATE_MANAGEMENT_MERCHANT_WITH_ADMIN) :> API.Types.Management.Merchant.CreateMerchantWithAdmin)

type CreateMerchant = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('CREATE_MANAGEMENT_MERCHANT) :> API.Types.Management.Merchant.CreateMerchant)

type ListMerchants = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('LIST_MANAGEMENT_MERCHANTS) :> API.Types.Management.Merchant.ListMerchants)

type ChangeMerchantEnableState = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('CHANGE_MANAGEMENT_MERCHANT_ENABLE_STATE) :> API.Types.Management.Merchant.ChangeMerchantEnableState)

createMerchantWithAdmin :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Management.Merchant.CreateMerchantWithAdminReq -> Environment.FlowHandler API.Types.Management.Merchant.PersonAPIEntity)
createMerchantWithAdmin merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.Management.Merchant.createMerchantWithAdmin merchantShortId opCity apiTokenInfo req

createMerchant :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Management.Merchant.CreateMerchantReq -> Environment.FlowHandler API.Types.Management.Merchant.MerchantAPIEntity)
createMerchant merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.Management.Merchant.createMerchant merchantShortId opCity apiTokenInfo req

listMerchants :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Data.Text.Text) -> Environment.FlowHandler API.Types.Management.Merchant.ListMerchantResp)
listMerchants merchantShortId opCity apiTokenInfo limit offset shortId = withFlowHandlerAPI' $ Domain.Action.Management.Merchant.listMerchants merchantShortId opCity apiTokenInfo limit offset shortId

changeMerchantEnableState :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Management.Merchant.ChangeMerchantEnableStateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
changeMerchantEnableState merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.Management.Merchant.changeMerchantEnableState merchantShortId opCity apiTokenInfo req
