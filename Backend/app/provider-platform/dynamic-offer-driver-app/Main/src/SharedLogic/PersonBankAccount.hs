module SharedLogic.PersonBankAccount where

import qualified API.Types.UI.DriverOnboardingV2
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Time as DT
import qualified Domain.Types.DriverBankAccount as DDBA
import qualified Domain.Types.FleetOwnerInformation as DFOI
import qualified Domain.Types.InitiatedBy as DIB
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person
import qualified Domain.Types.TransporterConfig as DTC
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.External.Types (ServiceFlow)
import qualified Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.SlidingWindowCounters (SlidingWindowOptions (..))
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions (..))
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowCounters (convertPeriodTypeToSeconds)
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.DriverSSN as QDriverSSN
import Tools.Error
import qualified Tools.Payment as TPayment

data PersonStripeInfo = PersonStripeInfo
  { personDob :: Maybe UTCTime,
    address :: Maybe Payment.Address,
    idNumber :: Maybe (EncryptedHashed Text),
    companyName :: Maybe Text,
    fleetType :: Maybe DFOI.FleetType,
    vatNumber :: Maybe Text,
    businessRegistrationNumber :: Maybe (EncryptedHashed Text)
  }

newtype PersonRegisterBankAccountLinkHandle = PersonRegisterBankAccountLinkHandle
  { fetchPersonStripeInfo :: Flow PersonStripeInfo
  }

defaultRefreshLimit :: APIRateLimitOptions
defaultRefreshLimit = APIRateLimitOptions {limit = 3, limitResetTimeInSec = 3600}

toRateLimitOptions :: DTC.TransporterConfig -> APIRateLimitOptions
toRateLimitOptions cfg =
  APIRateLimitOptions
    { limit = fromMaybe defaultRefreshLimit.limit cfg.stripeStatusRefreshCountThreshold,
      limitResetTimeInSec = maybe defaultRefreshLimit.limitResetTimeInSec fromWindow cfg.stripeStatusRefreshCountWindow
    }
  where
    fromWindow SlidingWindowOptions {..} = fromInteger (period * convertPeriodTypeToSeconds periodType)

refreshRateLimitKey :: Id Domain.Types.Person.Person -> Text
refreshRateLimitKey driverId = "BPP:Stripe:StatusRefresh:" <> driverId.getId <> ":hitsCount"

getPersonRegisterBankAccountLink ::
  PersonRegisterBankAccountLinkHandle ->
  Maybe DMPM.PaymentMode ->
  Maybe DIB.InitiatedBy ->
  Domain.Types.Person.Person ->
  Environment.Flow API.Types.UI.DriverOnboardingV2.BankAccountLinkResp
getPersonRegisterBankAccountLink h mbPaymentMode mbInitiatedBy person = do
  mPersonBankAccount <- runInReplica $ QDBA.findByPrimaryKey person.id
  paymentMode <- validatePaymentMode mbPaymentMode mPersonBankAccount
  now <- getCurrentTime
  case mPersonBankAccount of
    Just bankAccount -> do
      let currentlyDue = fromMaybe [] (bankAccount.requirements >>= (.currentlyDue))
      when (bankAccount.chargesEnabled && null currentlyDue) $
        throwError $ InvalidRequest "Bank account already enabled"
      refreshLink bankAccount paymentMode
    _ -> createAccount now paymentMode
  where
    refreshLink :: DDBA.DriverBankAccount -> DMPM.PaymentMode -> Environment.Flow API.Types.UI.DriverOnboardingV2.BankAccountLinkResp
    refreshLink bankAccount paymentMode = do
      resp <-
        TPayment.retryAccountLink person.merchantOperatingCityId (Just paymentMode) $
          Payment.RetryAccountLinkReq
            { accountId = bankAccount.accountId,
              returnUrlKey = show <$> mbInitiatedBy
            }
      accountUrl <- Kernel.Prelude.parseBaseUrl resp.accountUrl
      QDBA.updateAccountLink (Just accountUrl) (Just resp.accountUrlExpiry) person.id
      return $
        API.Types.UI.DriverOnboardingV2.BankAccountLinkResp
          { chargesEnabled = bankAccount.chargesEnabled,
            payoutsEnabled = bankAccount.payoutsEnabled,
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
      businessRegistrationNumber <- forM personStripeInfo.businessRegistrationNumber decrypt
      ssnLast4 <-
        if merchantOpCity.country == Context.USA
          then do
            driverSSN <- runInReplica $ QDriverSSN.findByDriverId person.id >>= fromMaybeM (DriverSSNNotFound person.id.getId)
            ssnNumber <- decrypt driverSSN.ssn
            return $ Just $ T.takeEnd 4 ssnNumber
          else return Nothing

      let businessType = maybe Payment.Individual castBusinessType personStripeInfo.fleetType
          mobileE164 = mobileCountryCode <> mobileNumber
      mbCompanyDetails <- case businessType of
        Payment.Company -> case personStripeInfo.companyName of
          Just companyName ->
            pure $
              Just
                Payment.CompanyConnectDetails
                  { name = companyName,
                    taxId = businessRegistrationNumber,
                    vatId = personStripeInfo.vatNumber,
                    address = personStripeInfo.address
                  }
          Nothing ->
            throwError $ InvalidRequest "Company name (fleetName) is required for BUSINESS_FLEET"
        _ -> pure Nothing
      let createAccountReq =
            Payment.ConnectAccountReq
              { country = merchantOpCity.country,
                email = person.email,
                dateOfBirth = DT.utctDay personDob,
                firstName = person.firstName,
                lastName = person.lastName,
                address = personStripeInfo.address,
                ssnLast4 = ssnLast4,
                idNumber,
                mobileNumber = mobileE164,
                businessType = Just businessType,
                companyDetails = mbCompanyDetails,
                returnUrlKey = show <$> mbInitiatedBy
              }
      resp <- TPayment.createConnectAccount person.merchantOperatingCityId (Just paymentMode) createAccountReq
      accountUrl <- Kernel.Prelude.parseBaseUrl resp.accountUrl
      let driverBankAccount =
            DDBA.DriverBankAccount
              { accountId = resp.accountId,
                chargesEnabled = resp.chargesEnabled,
                payoutsEnabled = Just resp.payoutsEnabled,
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
                nameAtBank = Nothing,
                requirements = resp.requirements,
                futureRequirements = resp.futureRequirements,
                lastSyncedAt = Just now
              }
      QDBA.create driverBankAccount
      QDBA.syncBankAccountToPool person.id resp.chargesEnabled (Just paymentMode)
      return $
        API.Types.UI.DriverOnboardingV2.BankAccountLinkResp
          { chargesEnabled = resp.chargesEnabled,
            payoutsEnabled = Just resp.payoutsEnabled,
            accountLink = accountUrl,
            accountUrlExpiry = resp.accountUrlExpiry,
            detailsSubmitted = resp.detailsSubmitted,
            paymentMode
          }

castBusinessType :: DFOI.FleetType -> Payment.BusinessType
castBusinessType = \case
  DFOI.NORMAL_FLEET -> Payment.Individual
  DFOI.RENTAL_FLEET -> Payment.Individual
  DFOI.BUSINESS_FLEET -> Payment.Company

validatePaymentMode :: Maybe DMPM.PaymentMode -> Maybe DDBA.DriverBankAccount -> Environment.Flow DMPM.PaymentMode
validatePaymentMode mbPaymentMode mbDriverBankAccount = do
  let paymentMode = fromMaybe DMPM.LIVE mbPaymentMode
  whenJust mbDriverBankAccount $ \driverBankAccount -> do
    let paymentMode' = fromMaybe DMPM.LIVE driverBankAccount.paymentMode
    unless (paymentMode == paymentMode') $
      throwError (InvalidRequest "Wrong payment mode")
  pure paymentMode

getPersonRegisterBankAccountStatus ::
  ( ServiceFlow m r,
    EsqDBReplicaFlow m r,
    Kernel.Storage.Hedis.HedisFlow m r,
    Kernel.Storage.Hedis.HedisLTSFlowEnv r
  ) =>
  Maybe Bool ->
  Id Domain.Types.Person.Person ->
  Id DMOC.MerchantOperatingCity ->
  m API.Types.UI.DriverOnboardingV2.BankAccountResp
getPersonRegisterBankAccountStatus mbForceRefresh personId merchantOpCityId = do
  bankAccount <- runInReplica $ QDBA.findByPrimaryKey personId >>= fromMaybeM (DriverBankAccountNotFound personId.getId)
  let paymentMode = fromMaybe DMPM.LIVE bankAccount.paymentMode
      forceRefresh = fromMaybe False mbForceRefresh
      currentlyDue = fromMaybe [] (bankAccount.requirements >>= (.currentlyDue))
      isBankAccountSuccessfullyLinked = bankAccount.chargesEnabled && null currentlyDue
  bankAccount' <-
    if not forceRefresh || isBankAccountSuccessfullyLinked
      then pure bankAccount
      else do
        rateLimitOpts <- getRateLimitOpts merchantOpCityId
        limited <- try @_ @SomeException (checkSlidingWindowLimitWithOptions (refreshRateLimitKey personId) rateLimitOpts)
        case limited of
          Left _ -> pure bankAccount
          Right _ -> do
            resp <- TPayment.getAccount merchantOpCityId (Just paymentMode) bankAccount.accountId
            QDBA.updateAccountStatus resp.chargesEnabled resp.payoutsEnabled resp.detailsSubmitted resp.requirements resp.futureRequirements personId
            pure
              bankAccount
                { DDBA.chargesEnabled = resp.chargesEnabled,
                  DDBA.payoutsEnabled = Just resp.payoutsEnabled,
                  DDBA.detailsSubmitted = resp.detailsSubmitted,
                  DDBA.requirements = resp.requirements,
                  DDBA.futureRequirements = resp.futureRequirements
                }
  pure $
    API.Types.UI.DriverOnboardingV2.BankAccountResp
      { chargesEnabled = bankAccount'.chargesEnabled,
        payoutsEnabled = bankAccount'.payoutsEnabled,
        detailsSubmitted = bankAccount'.detailsSubmitted,
        requirements = bankAccount'.requirements,
        futureRequirements = bankAccount'.futureRequirements,
        paymentMode
      }
  where
    getRateLimitOpts opCityId = do
      mbCfg <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = opCityId.getId}) Nothing
      pure $ maybe defaultRefreshLimit toRateLimitOptions mbCfg
