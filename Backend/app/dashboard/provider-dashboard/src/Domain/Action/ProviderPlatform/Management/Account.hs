{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.ProviderPlatform.Management.Account
  ( getAccountFetchUnverifiedAccounts,
    postAccountVerifyAccount,
  )
where

import qualified API.Client.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.Account as Common
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.Person.Type as DP
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import Kernel.External.Encryption (decrypt, encrypt)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Storage.Queries.Person (findAllByFromDateAndToDateAndMobileNumberAndStatus)
import Tools.Auth.Api
import Tools.Auth.Merchant

getAccountFetchUnverifiedAccounts ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Common.FleetOwnerStatus ->
  Environment.Flow [Common.PersonAPIEntity]
getAccountFetchUnverifiedAccounts _merchantShortId _opCity _apiTokenInfo mbFromDate mbToDate mbMobileNumber mbStatus = do
  mbEncryptMobileNumber <- traverse encrypt mbMobileNumber
  encryptPersonLs <- findAllByFromDateAndToDateAndMobileNumberAndStatus mbFromDate mbToDate mbEncryptMobileNumber mbStatus
  traverse convertPersonToPersonAPIEntity encryptPersonLs
  where
    --convertPersonToPersonAPIEntity :: DP.Person -> Common.PersonAPIEntity
    convertPersonToPersonAPIEntity DP.Person {..} = do
      mobileNumber <- decrypt mobileNumber
      email <- traverse decrypt email
      pure $
        Common.PersonAPIEntity
          { id = Kernel.Types.Id.cast id,
            roleId = Kernel.Types.Id.cast roleId,
            email = email,
            mobileNumber = mobileNumber,
            ..
          }

postAccountVerifyAccount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Common.VerifyAccountReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postAccountVerifyAccount merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.accountDSL.postAccountVerifyAccount) req)
