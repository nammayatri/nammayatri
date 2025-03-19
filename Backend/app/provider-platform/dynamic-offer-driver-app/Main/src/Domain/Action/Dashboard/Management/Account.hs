{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Management.Account
  ( getAccountFetchUnverifiedAccounts,
    postAccountVerifyAccount,
  )
where

import qualified API.Types.ProviderPlatform.Management.Account as Common
import Dashboard.Common (Person)
import Data.OpenApi (ToSchema)
import Domain.Action.Dashboard.Fleet.Registration
  ( FleetOwnerRegisterReq (..),
    FleetOwnerRegisterWithFlasAndPersonIdReq (..),
    processFleetOwnerRegister,
  )
import qualified Domain.Types.FleetOwnerInformation as FOI
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
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Person) ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Common.FleetOwnerRegisterReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postAccountVerifyAccount _merchantShortId _opCity mbPersonId mbRequireApproval req = do
  let personId = fromMaybe (error "Id Person was not received") mbPersonId
      requireApproval = fromMaybe (error "Value for requireAdminApprovalForFleetOnboarding was not received") mbRequireApproval
  _ <-
    processFleetOwnerRegister $
      FleetOwnerRegisterWithFlasAndPersonIdReq
        (castFleetOwnerRegisterReq req)
        requireApproval
        (Kernel.Types.Id.cast personId)
  pure Kernel.Types.APISuccess.Success
  where
    castFleetOwnerRegisterReq :: Common.FleetOwnerRegisterReq -> FleetOwnerRegisterReq
    castFleetOwnerRegisterReq Common.FleetOwnerRegisterReq {..} =
      FleetOwnerRegisterReq
        { fleetType = castFleetType <$> fleetType,
          ..
        }
    castFleetType :: Common.FleetType -> FOI.FleetType
    castFleetType = \case
      Common.RENTAL_FLEET -> FOI.RENTAL_FLEET
      Common.NORMAL_FLEET -> FOI.NORMAL_FLEET
      Common.BUSINESS_FLEET -> FOI.BUSINESS_FLEET
