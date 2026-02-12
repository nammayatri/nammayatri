module SharedLogic.PersonBankAccount where

import qualified API.Types.UI.DriverOnboardingV2
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Time as DT
import qualified Domain.Types.DriverBankAccount as DDBA
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.DriverSSN as QDriverSSN
import Tools.Error
import qualified Tools.Payment as TPayment

data PersonStripeInfo = PersonStripeInfo
  { personDob :: Maybe UTCTime,
    address :: Maybe Payment.Address,
    idNumber :: Maybe (EncryptedHashed Text)
  }

newtype PersonRegisterBankAccountLinkHandle = PersonRegisterBankAccountLinkHandle
  { fetchPersonStripeInfo :: Flow PersonStripeInfo
  }

getPersonRegisterBankAccountLink ::
  PersonRegisterBankAccountLinkHandle ->
  Maybe DMPM.PaymentMode ->
  Domain.Types.Person.Person ->
  Environment.Flow API.Types.UI.DriverOnboardingV2.BankAccountLinkResp
getPersonRegisterBankAccountLink h mbPaymentMode person = do
  mPersonBankAccount <- runInReplica $ QDBA.findByPrimaryKey person.id
  paymentMode <- validatePaymentMode mbPaymentMode mPersonBankAccount
  now <- getCurrentTime
  case mPersonBankAccount of
    Just bankAccount -> do
      when bankAccount.chargesEnabled $ throwError $ InvalidRequest "Bank account already enabled"
      case (bankAccount.currentAccountLink, bankAccount.currentAccountLinkExpiry) of
        (Just link, Just expiry) -> do
          if expiry > now
            then
              return $
                API.Types.UI.DriverOnboardingV2.BankAccountLinkResp
                  { chargesEnabled = bankAccount.chargesEnabled,
                    accountLink = link,
                    accountUrlExpiry = expiry,
                    detailsSubmitted = bankAccount.detailsSubmitted,
                    paymentMode
                  }
            else refreshLink bankAccount paymentMode
        _ -> refreshLink bankAccount paymentMode
    _ -> createAccount now paymentMode
  where
    refreshLink :: DDBA.DriverBankAccount -> DMPM.PaymentMode -> Environment.Flow API.Types.UI.DriverOnboardingV2.BankAccountLinkResp
    refreshLink bankAccount paymentMode = do
      resp <- TPayment.retryAccountLink person.merchantId person.merchantOperatingCityId (Just paymentMode) bankAccount.accountId
      accountUrl <- Kernel.Prelude.parseBaseUrl resp.accountUrl
      QDBA.updateAccountLink (Just accountUrl) (Just resp.accountUrlExpiry) person.id
      return $
        API.Types.UI.DriverOnboardingV2.BankAccountLinkResp
          { chargesEnabled = bankAccount.chargesEnabled,
            accountLink = accountUrl,
            accountUrlExpiry = resp.accountUrlExpiry,
            detailsSubmitted = bankAccount.detailsSubmitted,
            paymentMode
          }

    createAccount :: UTCTime -> DMPM.PaymentMode -> Environment.Flow API.Types.UI.DriverOnboardingV2.BankAccountLinkResp
    createAccount now paymentMode = do
      merchantOpCity <- CQMOC.findById person.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
      when (merchantOpCity.country `notElem` [Context.USA, Context.Netherlands, Context.Finland]) $ throwError $ InvalidRequest "Bank account creation is only supported for USA, Netherlands and Finland"

      mbMobileNumber <- mapM decrypt person.mobileNumber
      mobileNumber <- mbMobileNumber & fromMaybeM (InvalidRequest "Mobile number is required for opening a bank account")
      defaultCountryCode <- case merchantOpCity.country of
        Context.Finland -> pure "+358"
        Context.Netherlands -> pure "+31"
        Context.USA -> pure "+1"
        _ -> throwError $ InvalidRequest "Bank account creation is only supported for USA, Netherlands and Finland"
      let mobileCountryCode = fromMaybe defaultCountryCode $ person.mobileCountryCode
      personStripeInfo <- h.fetchPersonStripeInfo
      personDob <- personStripeInfo.personDob & fromMaybeM (InvalidRequest "Driver DOB is required for opening a bank account")
      idNumber <- forM personStripeInfo.idNumber decrypt
      ssnLast4 <-
        if merchantOpCity.country == Context.USA
          then do
            driverSSN <- runInReplica $ QDriverSSN.findByDriverId person.id >>= fromMaybeM (DriverSSNNotFound person.id.getId)
            ssnNumber <- decrypt driverSSN.ssn
            return $ Just $ T.takeEnd 4 ssnNumber
          else return Nothing

      let createAccountReq =
            Payment.IndividualConnectAccountReq
              { country = merchantOpCity.country,
                email = person.email,
                dateOfBirth = DT.utctDay personDob,
                firstName = person.firstName,
                lastName = person.lastName,
                address = personStripeInfo.address,
                ssnLast4 = ssnLast4,
                idNumber,
                mobileNumber = mobileCountryCode <> mobileNumber
              }
      resp <- TPayment.createIndividualConnectAccount person.merchantId person.merchantOperatingCityId (Just paymentMode) createAccountReq
      accountUrl <- Kernel.Prelude.parseBaseUrl resp.accountUrl
      let driverBankAccount =
            DDBA.DriverBankAccount
              { accountId = resp.accountId,
                chargesEnabled = resp.chargesEnabled,
                currentAccountLink = Just accountUrl,
                currentAccountLinkExpiry = Just resp.accountUrlExpiry,
                detailsSubmitted = resp.detailsSubmitted,
                driverId = person.id,
                merchantId = Just person.merchantId,
                merchantOperatingCityId = Just person.merchantOperatingCityId,
                paymentMode = Just paymentMode,
                createdAt = now,
                updatedAt = now,
                ifscCode = Nothing,
                nameAtBank = Nothing
              }
      QDBA.create driverBankAccount
      return $
        API.Types.UI.DriverOnboardingV2.BankAccountLinkResp
          { chargesEnabled = resp.chargesEnabled,
            accountLink = accountUrl,
            accountUrlExpiry = resp.accountUrlExpiry,
            detailsSubmitted = resp.detailsSubmitted,
            paymentMode
          }

validatePaymentMode :: Maybe DMPM.PaymentMode -> Maybe DDBA.DriverBankAccount -> Environment.Flow DMPM.PaymentMode
validatePaymentMode mbPaymentMode mbDriverBankAccount = do
  let paymentMode = fromMaybe DMPM.LIVE mbPaymentMode
  whenJust mbDriverBankAccount $ \driverBankAccount -> do
    let paymentMode' = fromMaybe DMPM.LIVE driverBankAccount.paymentMode
    unless (paymentMode == paymentMode') $
      throwError (InvalidRequest "Wrong payment mode")
  pure paymentMode

getPersonRegisterBankAccountStatus ::
  Domain.Types.Person.Person ->
  Environment.Flow API.Types.UI.DriverOnboardingV2.BankAccountResp
getPersonRegisterBankAccountStatus person = do
  driverBankAccount <- runInReplica $ QDBA.findByPrimaryKey person.id >>= fromMaybeM (DriverBankAccountNotFound person.id.getId)
  let paymentMode = fromMaybe DMPM.LIVE driverBankAccount.paymentMode
  if driverBankAccount.chargesEnabled
    then
      return $
        API.Types.UI.DriverOnboardingV2.BankAccountResp
          { chargesEnabled = driverBankAccount.chargesEnabled,
            detailsSubmitted = driverBankAccount.detailsSubmitted,
            paymentMode
          }
    else do
      resp <- TPayment.getAccount person.merchantId person.merchantOperatingCityId (Just paymentMode) driverBankAccount.accountId
      QDBA.updateAccountStatus resp.chargesEnabled resp.detailsSubmitted person.id
      return $
        API.Types.UI.DriverOnboardingV2.BankAccountResp
          { chargesEnabled = resp.chargesEnabled,
            detailsSubmitted = resp.detailsSubmitted,
            paymentMode
          }
