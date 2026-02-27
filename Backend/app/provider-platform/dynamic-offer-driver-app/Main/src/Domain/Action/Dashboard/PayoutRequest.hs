{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.Dashboard.PayoutRequest
  ( deleteVpa,
    updateVpa,
    refundRegistrationAmount,
    upsertScheduledPayoutConfig,
    UpdateScheduledPayoutConfigReq (..),
  )
where

import qualified Data.Text as T
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ScheduledPayoutConfig as DSPC
import qualified Domain.Types.VehicleCategory as DV
import qualified Environment
import EulerHS.Prelude hiding (id, readMaybe, sum)
import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.External.Payment.Interface.Types as KT
import qualified Kernel.External.Payout.Types as PT
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Payment.API.Payout (VerifyVpaFlow (..))
import qualified Lib.Payment.API.Payout.Types as PayoutTypes
import qualified Lib.Payment.Domain.Action as Payout
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Payout.Registration as Registration
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ScheduledPayoutConfig as QSPC
import qualified Tools.Payment as TPayment
import qualified Tools.Payout as TP

deleteVpa :: PayoutTypes.DeleteVpaReq -> Environment.Flow APISuccess
deleteVpa req = do
  let driverIds = (Id.Id <$> req.personIds) :: [Id.Id DP.Person]
  void $ QDI.updatePayoutVpaAndStatusByDriverIds Nothing Nothing driverIds
  pure Success

updateVpa :: PayoutTypes.UpdateVpaReq -> Environment.Flow APISuccess
updateVpa req = do
  let driverId = (Id.Id req.personId) :: Id.Id DP.Person
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let resolvedStatus = fromMaybe (defaultStatus req.verify) (parseVpaStatus req.vpaStatus)
  QDI.updatePayoutVpaAndStatus (Just req.vpa) (Just resolvedStatus) person.id
  pure Success
  where
    defaultStatus shouldVerify =
      if shouldVerify then DI.VERIFIED_BY_USER else DI.MANUALLY_ADDED

    parseVpaStatus = \case
      Nothing -> Nothing
      Just statusText -> readMaybe (T.unpack statusText)

instance VerifyVpaFlow Environment.Flow where
  verifyVpaForUpdate = verifyVpaForUpdateImpl

verifyVpaForUpdateImpl :: PayoutTypes.UpdateVpaReq -> Environment.Flow ()
verifyVpaForUpdateImpl req = do
  let driverId = (Id.Id req.personId) :: Id.Id DP.Person
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  paymentServiceName <- TPayment.decidePaymentService (DEMSC.PaymentService Payment.Juspay) person.clientSdkVersion person.merchantOperatingCityId
  let verifyVPAReq =
        KT.VerifyVPAReq
          { orderId = Nothing,
            customerId = Just person.id.getId,
            vpa = req.vpa
          }
      verifyVpaCall = TPayment.verifyVpa person.merchantId person.merchantOperatingCityId paymentServiceName (Just person.id.getId)
  resp <- withTryCatch "verifyVPAService:updateVpa" $ Payout.verifyVPAService verifyVPAReq verifyVpaCall
  case resp of
    Left e -> throwError $ InvalidRequest $ "VPA Verification Failed: " <> show e
    Right response ->
      unless (response.status == "VALID") $
        throwError $ InvalidRequest $ "Invalid VPA Updation: " <> show response

refundRegistrationAmount :: (BeamFlow Environment.Flow Environment.AppEnv) => Id.ShortId DM.Merchant -> Kernel.Types.Beckn.Context.City -> PayoutTypes.RefundRegAmountReq -> Environment.Flow APISuccess
refundRegistrationAmount merchantShortId opCity req = do
  let driverId = (Id.Id req.personId) :: Id.Id DP.Person
  driverInfo <- QDI.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)

  -- Eligibility check: VPA must be captured via webhook, refund not already done
  unless (driverInfo.payoutVpaStatus == Just DI.VIA_WEBHOOK && isNothing driverInfo.payoutRegAmountRefunded && isJust driverInfo.payoutVpa) $
    throwError $ InvalidRequest $ "Driver not eligible for refund | driver id: " <> show driverId

  -- Get the registration PaymentOrder ID
  registrationOrderId <- case driverInfo.payoutRegistrationOrderId of
    Just oid -> pure $ Id.Id oid
    Nothing -> throwError $ InvalidRequest $ "No registration order found for driver: " <> show driverId

  -- Get payout call
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  let vehicleCategory = DV.AUTO_CATEGORY
  payoutConfig <- CPC.findByPrimaryKey merchantOpCity.id vehicleCategory Nothing >>= fromMaybeM (PayoutConfigNotFound (show vehicleCategory) merchantOpCity.id.getId)
  payoutServiceName <- TP.decidePayoutService (DEMSC.PayoutService PT.Juspay) driver.clientSdkVersion driver.merchantOperatingCityId
  let createPayoutOrderCall = TP.createPayoutOrder merchant.id merchantOpCity.id payoutServiceName (Just driverId.getId)

  -- Delegate to lib — it looks up PaymentOrder for amount + VPA, checks idempotency
  logDebug $ "Refunding registration for driverId: " <> driverId.getId <> " | orderId: " <> registrationOrderId.getId
  mbResult <- Registration.refundRegistrationAmount registrationOrderId createPayoutOrderCall payoutConfig.remark payoutConfig.orderType (show merchantOpCity.city)

  case mbResult of
    Just _ -> do
      -- Mark refund on DriverInformation (domain concern)
      QDI.updatePayoutRegAmountRefunded (Just Registration.registrationAmount) driverId
    Nothing ->
      logDebug $ "Registration refund already exists for driver: " <> driverId.getId

  pure Success

--------------------------------------------------------------------------------
-- Scheduled Payout Config Admin API
--------------------------------------------------------------------------------

data UpdateScheduledPayoutConfigReq = UpdateScheduledPayoutConfigReq
  { payoutCategory :: DPayment.EntityName,
    isEnabled :: Maybe Bool,
    frequency :: Maybe DSPC.ScheduledPayoutFrequency,
    dayOfWeek :: Maybe Int,
    dayOfMonth :: Maybe Int,
    timeOfDay :: Maybe Text,
    batchSize :: Maybe Int,
    minimumPayoutAmount :: Maybe HighPrecMoney,
    maxRetriesPerDriver :: Maybe Int,
    vehicleCategory :: Maybe DV.VehicleCategory,
    remark :: Maybe Text,
    orderType :: Maybe Text,
    timeDiffFromUtc :: Maybe Seconds
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

upsertScheduledPayoutConfig ::
  Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  UpdateScheduledPayoutConfigReq ->
  Environment.Flow APISuccess
upsertScheduledPayoutConfig merchantShortId opCity req = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)

  mbExisting <- QSPC.findByMerchantOpCityIdAndCategory merchantOpCity.id req.payoutCategory
  case mbExisting of
    Just existing -> do
      now <- getCurrentTime
      let updated =
            existing
              { DSPC.isEnabled = fromMaybe existing.isEnabled req.isEnabled,
                DSPC.frequency = fromMaybe existing.frequency req.frequency,
                DSPC.dayOfWeek = req.dayOfWeek <|> existing.dayOfWeek,
                DSPC.dayOfMonth = req.dayOfMonth <|> existing.dayOfMonth,
                DSPC.timeOfDay = fromMaybe existing.timeOfDay req.timeOfDay,
                DSPC.batchSize = fromMaybe existing.batchSize req.batchSize,
                DSPC.minimumPayoutAmount = fromMaybe existing.minimumPayoutAmount req.minimumPayoutAmount,
                DSPC.maxRetriesPerDriver = fromMaybe existing.maxRetriesPerDriver req.maxRetriesPerDriver,
                DSPC.vehicleCategory = req.vehicleCategory <|> existing.vehicleCategory,
                DSPC.remark = req.remark <|> existing.remark,
                DSPC.orderType = fromMaybe existing.orderType req.orderType,
                DSPC.timeDiffFromUtc = fromMaybe existing.timeDiffFromUtc req.timeDiffFromUtc,
                DSPC.updatedAt = now
              }
      QSPC.updateByPrimaryKey updated
      logInfo $ "Updated ScheduledPayoutConfig for " <> show req.payoutCategory <> " in city " <> merchantOpCity.id.getId
    Nothing -> do
      now <- getCurrentTime
      let newConfig =
            DSPC.ScheduledPayoutConfig
              { DSPC.merchantId = merchant.id,
                DSPC.merchantOperatingCityId = merchantOpCity.id,
                DSPC.payoutCategory = req.payoutCategory,
                DSPC.isEnabled = fromMaybe False req.isEnabled,
                DSPC.frequency = fromMaybe DSPC.DAILY req.frequency,
                DSPC.dayOfWeek = req.dayOfWeek,
                DSPC.dayOfMonth = req.dayOfMonth,
                DSPC.timeOfDay = fromMaybe "02:00" req.timeOfDay,
                DSPC.batchSize = fromMaybe 50 req.batchSize,
                DSPC.minimumPayoutAmount = fromMaybe 10.0 req.minimumPayoutAmount,
                DSPC.maxRetriesPerDriver = fromMaybe 3 req.maxRetriesPerDriver,
                DSPC.vehicleCategory = req.vehicleCategory,
                DSPC.remark = req.remark,
                DSPC.orderType = fromMaybe "FULFILL_ONLY" req.orderType,
                DSPC.timeDiffFromUtc = fromMaybe 19800 req.timeDiffFromUtc,
                DSPC.createdAt = now,
                DSPC.updatedAt = now
              }
      QSPC.create newConfig
      logInfo $ "Created ScheduledPayoutConfig for " <> show req.payoutCategory <> " in city " <> merchantOpCity.id.getId
  pure Success
