{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.ProviderPlatform.Fleet.PayoutAccount (postPayoutAccount, postPayoutAccountStatus) where

import qualified API.Client.ProviderPlatform.Fleet as Client
import qualified API.Types.ProviderPlatform.Fleet.PayoutAccount
import Domain.Action.ProviderPlatform.Management.DriverRegistration (determineAuditRequestorRole)
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

postPayoutAccount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Fleet.PayoutAccount.PayoutAccountReq -> Environment.Flow API.Types.ProviderPlatform.Fleet.PayoutAccount.PayoutAccountResp)
postPayoutAccount merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  -- Audit actor role, gated on merchant.sendDocumentAuditActorDetails, forwarded so the BPP records the acting
  -- dashboard user's real role (the actor id is the path requestorId). Frontend never sends it.
  mbRequestorRole <- determineAuditRequestorRole apiTokenInfo
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    Client.callFleetAPI checkedMerchantId opCity (.payoutAccountDSL.postPayoutAccount) apiTokenInfo.personId.getId (req {API.Types.ProviderPlatform.Fleet.PayoutAccount.requestorRole = mbRequestorRole} :: API.Types.ProviderPlatform.Fleet.PayoutAccount.PayoutAccountReq)

postPayoutAccountStatus ::
  ( Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
    Kernel.Types.Beckn.Context.City ->
    ApiTokenInfo ->
    API.Types.ProviderPlatform.Fleet.PayoutAccount.PayoutAccountStatusReq ->
    Environment.Flow API.Types.ProviderPlatform.Fleet.PayoutAccount.PayoutAccountStatusResp
  )
postPayoutAccountStatus merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  -- Same audit-role forwarding as the create side so the status result is attributed to the same acting user.
  mbRequestorRole <- determineAuditRequestorRole apiTokenInfo
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    Client.callFleetAPI checkedMerchantId opCity (.payoutAccountDSL.postPayoutAccountStatus) apiTokenInfo.personId.getId (req {API.Types.ProviderPlatform.Fleet.PayoutAccount.requestorRole = mbRequestorRole} :: API.Types.ProviderPlatform.Fleet.PayoutAccount.PayoutAccountStatusReq)
