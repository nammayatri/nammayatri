module Domain.Action.Dashboard.Fleet.PayoutAccount
  ( postPayoutAccount,
    postPayoutAccountStatus,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.PayoutAccount as Common
import qualified "dashboard-helper-api" Dashboard.Common as DC
import qualified Domain.Action.Dashboard.Fleet.RegistrationV2 as RegV2
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
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.FleetOwnerInformationExtra as QFOIE
import Tools.Error

postPayoutAccount ::
  ShortId DMerchant.Merchant ->
  City.City ->
  Text ->
  Common.PayoutAccountReq ->
  Flow Common.PayoutAccountResp
postPayoutAccount merchantShortId opCity requestorId req = do
  fleetOwner <- RegV2.checkRequestorAccessToFleet req.fleetOwnerId requestorId
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = fleetOwner.id
      driverId = cast @DP.Person @DC.Driver fleetOwner.id
  case req.accountType of
    Common.BANK -> do
      bankNo <- req.bankAccountNo & fromMaybeM (InvalidRequest "bankAccountNo required for Bank")
      ifsc <- req.bankIfscCode & fromMaybeM (InvalidRequest "bankIfscCode required for Bank")
      let verifyReq = BankAccountVerification.DriverBankAccountVerifyReq {bankAccountNo = bankNo, bankIfscCode = ifsc, nfVerification = False}
      verifyResp <- BankAccountVerification.verifyBankAccount (personId, merchant.id, merchantOpCityId) verifyReq
      pure $
        Common.PayoutAccountResp
          { accountType = Common.BANK,
            bankVerifyResp = Just verifyResp,
            upiRegistrationResp = Nothing
          }
    Common.UPI -> do
      upiReg <- MDR.getDriverRegistrationPayoutRegistration merchantShortId opCity driverId
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
  fleetOwner <- RegV2.checkRequestorAccessToFleet req.fleetOwnerId requestorId
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = fleetOwner.id
      driverId = cast @DP.Person @DC.Driver fleetOwner.id
  case req.accountType of
    Common.BANK -> do
      bankRequestId <- req.bankRequestId & fromMaybeM (InvalidRequest "bankRequestId required for Bank status")
      bankInfoResp <- BankAccountVerification.getInfoBankAccount (personId, merchant.id, merchantOpCityId) bankRequestId
      when (bankInfoResp.accountExists) $ do
        mbFleetInfo <- QFOI.findByPrimaryKey personId
        when (isNothing (mbFleetInfo >>= (.payoutVpa))) $
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
