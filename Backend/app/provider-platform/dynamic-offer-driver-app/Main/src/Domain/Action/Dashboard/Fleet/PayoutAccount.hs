module Domain.Action.Dashboard.Fleet.PayoutAccount
  ( postPayoutAccount,
    postPayoutAccountStatus,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.PayoutAccount as Common
import qualified "dashboard-helper-api" Dashboard.Common as DC
import qualified Domain.Action.Dashboard.Fleet.Access as FleetAccess
import qualified Domain.Action.Dashboard.Management.DriverRegistration as MDR
import qualified Domain.Action.UI.DriverOnboarding.BankAccountVerification as BankAccountVerification
import qualified Domain.Types.FleetOwnerInformation as DFOI
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverOnboarding.Audit as Audit
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.FleetOwnerInformationExtra as QFOIE
import qualified Tools.ActorInfo as ActorInfo
import Tools.Error

postPayoutAccount ::
  ShortId DMerchant.Merchant ->
  City.City ->
  Text ->
  Common.PayoutAccountReq ->
  Flow Common.PayoutAccountResp
postPayoutAccount merchantShortId opCity requestorId req = ActorInfo.withDashboardPersonIdActorInfo (Id @DP.Person requestorId) $ do
  let fleetOwnerId = fromMaybe requestorId req.fleetOwnerId
  FleetAccess.FleetOwnerInfo {fleetOwner} <- FleetAccess.checkRequestorAccessToFleet False (Just requestorId) fleetOwnerId
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = fleetOwner.id
      driverId = cast @DP.Person @DC.Driver fleetOwner.id
  case req.accountType of
    Common.BANK -> do
      bankNo <- req.bankAccountNo & fromMaybeM (InvalidRequest "bankAccountNo required for Bank")
      ifsc <- req.bankIfscCode & fromMaybeM (InvalidRequest "bankIfscCode required for Bank")
      let verifyReq = BankAccountVerification.DriverBankAccountVerifyReq {bankAccountNo = bankNo, bankIfscCode = ifsc, nfVerification = False}
      -- Fleet payout setup comes via the dashboard: attribute to the acting requestor + their real forwarded role.
      -- Role not forwarded (merchant opted out) → Nothing → verifyBankAccount falls back to the person's own role.
      let mbAuditRequestor = Audit.dashboardActorFromForwarded (Just requestorId) req.requestorRole
      verifyResp <- BankAccountVerification.verifyBankAccount mbAuditRequestor (personId, merchant.id, merchantOpCityId) verifyReq
      pure $
        Common.PayoutAccountResp
          { accountType = Common.BANK,
            bankVerifyResp = Just verifyResp,
            upiRegistrationResp = Nothing
          }
    Common.UPI -> do
      upiReg <- MDR.getDriverRegistrationPayoutRegistrationWithActor merchantShortId opCity driverId
      pure $
        Common.PayoutAccountResp
          { accountType = Common.UPI,
            bankVerifyResp = Nothing,
            upiRegistrationResp = Just upiReg
          }

postPayoutAccountStatus ::
  ShortId DMerchant.Merchant ->
  City.City ->
  Text ->
  Common.PayoutAccountStatusReq ->
  Flow Common.PayoutAccountStatusResp
postPayoutAccountStatus merchantShortId opCity requestorId req = do
  let fleetOwnerId = fromMaybe requestorId req.fleetOwnerId
  FleetAccess.FleetOwnerInfo {fleetOwner} <- FleetAccess.checkRequestorAccessToFleet False (Just requestorId) fleetOwnerId
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = fleetOwner.id
      driverId = cast @DP.Person @DC.Driver fleetOwner.id
  case req.accountType of
    Common.BANK -> do
      bankRequestId <- req.bankRequestId & fromMaybeM (InvalidRequest "bankRequestId required for Bank status")
      -- Fleet payout status poll: attribute to the acting requestor + their real forwarded role, matching create.
      -- Role not forwarded → Nothing → getInfoBankAccount falls back to the driver-self default.
      let mbAuditRequestor = Audit.dashboardActorFromForwarded (Just requestorId) req.requestorRole
      bankInfoResp <- BankAccountVerification.getInfoBankAccount mbAuditRequestor (personId, merchant.id, merchantOpCityId) bankRequestId
      when (bankInfoResp.accountExists) $
        case (bankInfoResp.bankAccountNumber, bankInfoResp.ifscCode) of
          (Just accNo, Just ifsc) ->
            QFOIE.updatePayoutVpaAndStatus (Just (accNo <> "@" <> ifsc <> ".ifsc.npci")) (Just DFOI.MANUALLY_ADDED) personId
          _ -> pure ()
      pure $
        Common.PayoutAccountStatusResp
          { accountType = Common.BANK,
            bankInfoResp = Just bankInfoResp,
            upiStatusResp = Nothing
          }
    Common.UPI -> do
      upiOrderId <- req.upiOrderId & fromMaybeM (InvalidRequest "upiOrderId required for UPI status")
      upiStatusResp <- MDR.getDriverRegistrationPayoutOrderStatus merchantShortId opCity driverId upiOrderId
      pure $
        Common.PayoutAccountStatusResp
          { accountType = Common.UPI,
            bankInfoResp = Nothing,
            upiStatusResp = Just upiStatusResp
          }
