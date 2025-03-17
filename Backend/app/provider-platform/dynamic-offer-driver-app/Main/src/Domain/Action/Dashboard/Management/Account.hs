{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Management.Account
  ( getAccountFetchUnverifiedAccounts,
    postAccountVerifyAccount,
  )
where

import qualified API.Types.ProviderPlatform.Management.Account as Common
import Data.OpenApi (ToSchema)
import Domain.Action.Dashboard.Fleet.Registration (FleetOwnerRegisterReq (..), fleetOwnerRegister)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant
import SharedLogic.Merchant (findMerchantByShortId)
import Tools.Auth

-- This function will not be called.
getAccountFetchUnverifiedAccounts ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Common.FleetOwnerStatus ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Environment.Flow [Common.PersonAPIEntity]
getAccountFetchUnverifiedAccounts _merchantShortId _opCity _mbFromDate _mbToDate _mbMobileNumber _mbStatus _mbLimit _mbOffset = do
  pure []

postAccountVerifyAccount ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.PersonAPIEntity ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postAccountVerifyAccount merchantShortId opCity Common.PersonAPIEntity {..} =
  fleetOwnerRegisterReq >>= fleetOwnerRegister
    >> pure Kernel.Types.APISuccess.Success
  where
    fleetOwnerRegisterReq = do
      merchant <- findMerchantByShortId merchantShortId
      pure $
        FleetOwnerRegisterReq
          { merchantId = merchant.id.getId,
            city = opCity,
            fleetType = Nothing,
            panNumber = Nothing,
            gstNumber = Nothing,
            panImageId1 = Nothing,
            panImageId2 = Nothing,
            gstCertificateImage = Nothing,
            ..
          }
