module Domain.Action.Dashboard.Fleet.PayoutAccount
  ( postPayoutAccount,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.PayoutAccount as Common
import qualified "dashboard-helper-api" Dashboard.Common as DC
import qualified Domain.Action.Dashboard.Fleet.RegistrationV2 as RegV2
import qualified Domain.Action.Dashboard.Management.DriverRegistration as MDR
import qualified Domain.Action.UI.DriverOnboarding.BankAccountVerification as BankAccountVerification
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error

postPayoutAccount ::
  ShortId DMerchant.Merchant ->
  City.City ->
  Text ->
  Common.PayoutAccountReq ->
  Flow Common.PayoutAccountResp

postPayoutAccount merchantShortId opCity requestorId req = do
  fleetOwner <- RegV2.checkRequestorAcccessToFleet req.fleetOwnerId requestorId
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
      let requestId = verifyResp.requestId
      infoResp <- BankAccountVerification.getInfoBankAccount (personId, merchant.id, merchantOpCityId) requestId
      pure $
        Common.PayoutAccountResp
          { accountType = Common.BANK,
            bankVerifyResp = Just verifyResp,
            bankInfoResp = Just infoResp,
            upiRegistrationResp = Nothing,
            upiStatusResp = Nothing
          }
    Common.UPI -> do
      upiReg <- MDR.getDriverRegistrationPayoutRegistration merchantShortId opCity driverId
      upiStatus <- MDR.getDriverRegistrationPayoutOrderStatus merchantShortId opCity driverId upiReg.orderId
      pure $
        Common.PayoutAccountResp
          { accountType = Common.UPI,
            bankVerifyResp = Nothing,
            bankInfoResp = Nothing,
            upiRegistrationResp = Just upiReg,
            upiStatusResp = Just upiStatus
          }
