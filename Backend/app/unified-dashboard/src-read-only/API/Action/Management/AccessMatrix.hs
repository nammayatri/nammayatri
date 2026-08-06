{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Management.AccessMatrix
  ( API,
    handler,
  )
where

import qualified "this" API.Types.Management
import qualified "this" API.Types.Management.AccessMatrix
import qualified Domain.Action.Management.AccessMatrix
import qualified Domain.Types.Merchant
import qualified Domain.Types.Role
import qualified Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = (GetAccessMatrix :<|> GetMerchantWithCityList)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getAccessMatrix merchantId city :<|> getMerchantWithCityList merchantId city

type GetAccessMatrix = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('GET_MANAGEMENT_ACCESS_MATRIX) :> API.Types.Management.AccessMatrix.GetAccessMatrix)

type GetMerchantWithCityList = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('GET_MANAGEMENT_MERCHANT_WITH_CITY_LIST) :> API.Types.Management.AccessMatrix.GetMerchantWithCityList)

getAccessMatrix :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Integer) -> Kernel.Prelude.Maybe (Kernel.Prelude.Integer) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Role.Role) -> Environment.FlowHandler API.Types.Management.AccessMatrix.AccessMatrixAPIEntity)
getAccessMatrix merchantShortId opCity apiTokenInfo limit offset roleId = withFlowHandlerAPI' $ Domain.Action.Management.AccessMatrix.getAccessMatrix merchantShortId opCity apiTokenInfo limit offset roleId

getMerchantWithCityList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler [API.Types.Management.AccessMatrix.MerchantCityList])
getMerchantWithCityList merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.Management.AccessMatrix.getMerchantWithCityList merchantShortId opCity apiTokenInfo
