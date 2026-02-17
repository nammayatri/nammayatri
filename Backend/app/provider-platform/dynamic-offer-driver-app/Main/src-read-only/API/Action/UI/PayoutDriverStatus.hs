{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.PayoutDriverStatus
  ( API,
    handler,
  )
where

import qualified Domain.Action.Common.PayoutRequest as CommonPayoutRequest
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import Kernel.Types.Error (GenericError (InvalidRequest))
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import qualified Lib.Payment.API.Payout as PayoutAPI
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (TokenAuth :> PayoutAPI.UIAPI)

handler :: Environment.FlowServer API
handler auth =
  let server = PayoutAPI.payoutUIHandler (mkConfig auth)
   in \entityName entityId -> withFlowHandlerAPI $ server entityName entityId

mkConfig ::
  ( Id.Id Domain.Types.Person.Person,
    Id.Id Domain.Types.Merchant.Merchant,
    Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  PayoutAPI.PayoutUIHandlerConfig Environment.Flow
mkConfig (personId, merchantId, merchantOpCityId) =
  PayoutAPI.PayoutUIHandlerConfig
    { refreshPayoutRequest = refreshWithAuth
    }
  where
    refreshWithAuth payoutRequest = do
      ensureDriverAccess payoutRequest
      CommonPayoutRequest.refreshPayoutRequestStatus payoutRequest

    ensureDriverAccess payoutRequest = do
      when (payoutRequest.beneficiaryId /= Id.getId personId) $
        throwError $ InvalidRequest "Unauthorized: You can only view your own payouts"
      when (payoutRequest.merchantId /= Id.getId merchantId) $
        throwError $ InvalidRequest "Invalid merchant for payout request"
      when (payoutRequest.merchantOperatingCityId /= Id.getId merchantOpCityId) $
        throwError $ InvalidRequest "Invalid merchant operating city for payout request"
