{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.StclMembership (postSubmitApplication, postBuyAdditionalShares, putUpdateApplication, getMembership, stclMemberShipOrderStatusHandler) where

import qualified API.Types.UI.StclMembership as APITypes
import Data.List (partition, sortOn)
import Data.Ord (Down (..))
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantOperatingCity as MerchantOperatingCity
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person as Person
import qualified Domain.Types.StclMembership as Domain
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B (runInReplica)
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface as PaymentInterface
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.External.Payment.Interface.Types as PaymentTypes
import qualified Kernel.External.Payment.Types as PaymentService
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common (Money (..), toHighPrecMoney)
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Payment.Domain.Action as LibPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.Common as LibPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import qualified SharedLogic.Payment
import Storage.Beam.Payment ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.StclMembership as QStclMembership
import Tools.Auth
import qualified Utils.Common.Cac.KeyNameConstants as CCK

-- Fallback defaults used when the per-MOC TransporterConfig leaves the corresponding field unset.
-- The handler reads these via TransporterConfig and only falls back here if the column is NULL,
-- so changing them in DB does not require a redeploy.

-- Maximum total shares a single driver may hold across all SUBMITTED applications.
defaultMaxSharesPerDriver :: Int
defaultMaxSharesPerDriver = 5

-- Per-share price (rupees) used to compute the order amount when the client omits it.
defaultPricePerShare :: Int
defaultPricePerShare = 100

-- Stale window for PENDING top-up rows. A PENDING row older than this is treated as an
-- abandoned payment (driver closed the payment app and never came back) and retired as
-- REJECTED, so the driver can start a fresh top-up. Fresh PENDING rows still block new
-- requests so an in-flight payment can complete without races / double-spend.
defaultPendingStaleMinutes :: Int
defaultPendingStaleMinutes = 15

postSubmitApplication ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Person.Person),
      Kernel.Types.Id.Id Merchant.Merchant,
      Kernel.Types.Id.Id MerchantOperatingCity.MerchantOperatingCity
    ) ->
    APITypes.MembershipApplicationReq ->
    Environment.Flow PaymentTypes.CreateOrderResp
  )
postSubmitApplication (mbDriverId, merchantId, merchantOperatingCityId) req = do
  -- Extract and validate driver ID
  driverId <- mbDriverId & fromMaybeM (InvalidRequest "Driver ID not found in authentication context")

  -- Validate that request driverId matches authenticated driver
  let requestDriverId = Kernel.Types.Id.Id req.driverId
  unless (driverId == requestDriverId) $
    throwError $ InvalidRequest "Driver ID in request does not match authenticated driver"

  -- First-purchase endpoint: reject if the driver already has a SUBMITTED application.
  -- Top-up purchases must go through the dedicated buyAdditionalShares endpoint.
  existingApplications <- QStclMembership.findByDriverId driverId
  let hasSubmittedApplication = any (\app -> app.status == Domain.SUBMITTED) existingApplications
  when hasSubmittedApplication $
    throwError $ InvalidRequest "An application already exists for this driver"

  -- Get person details
  person <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)

  -- Decrypt mobile number for payment
  mbMobileNumber <- person.mobileNumber & fromMaybeM (InvalidRequest "Mobile number not found")
  decryptedMobile <- decrypt mbMobileNumber

  -- Generate order ID and short ID in backend
  orderId <- generateGUIDText
  orderShortId <- generateShortId
  let shortIdText = orderShortId.getShortId

  -- Get amount from request
  let amount = toHighPrecMoney req.amount

  -- Fetch gatewayReferenceId from merchant service config
  mbGatewayReferenceId <- do
    mbServiceConfig <- CQMSC.findByServiceAndCity (DMSC.MembershipPaymentService PaymentService.Juspay) merchantOperatingCityId
    case mbServiceConfig of
      Just serviceConfig -> case serviceConfig.serviceConfig of
        DMSC.MembershipPaymentServiceConfig paymentServiceConfig ->
          pure $ PaymentInterface.getGatewayReferenceId paymentServiceConfig
        _ -> pure Nothing
      Nothing -> pure Nothing

  -- Create PaymentTypes.CreateOrderReq
  nwAddress <- asks (.nwAddress)
  let createOrderReq =
        PaymentTypes.CreateOrderReq
          { orderId = orderId,
            orderShortId = shortIdText,
            amount = amount,
            customerId = driverId.getId,
            customerEmail = req.emailId,
            customerPhone = decryptedMobile,
            customerFirstName = Just req.firstName,
            customerLastName = Just req.lastName,
            createMandate = Nothing,
            mandateMaxAmount = Nothing,
            mandateFrequency = Nothing,
            mandateStartDate = Nothing,
            mandateEndDate = Nothing,
            metadataGatewayReferenceId = mbGatewayReferenceId,
            optionsGetUpiDeepLinks = Nothing,
            metadataExpiryInMins = Nothing,
            splitSettlementDetails = Nothing,
            webhookUrl = Just nwAddress,
            basket = Nothing,
            paymentRules = Nothing,
            autoRefundPostSuccess = Nothing,
            paymentFilter = Nothing,
            udf1 = Nothing
          }

  -- PaymentServiceType for createOrderService (STCL)
  let paymentServiceType = fromMaybe DOrder.STCL req.paymentServiceType

  -- Create payment order
  createOrderResp <- SharedLogic.Payment.createOrderV2 (driverId, merchantId, merchantOperatingCityId) createOrderReq (Just paymentServiceType)

  -- Encrypt sensitive fields for storage (aadharNumber from frontend is ignored - not stored)
  encryptedPAN <- encrypt req.panNumber
  encryptedMobile <- encrypt req.mobileNumber
  encryptedAccountNumber <- encrypt req.bankDetails.accountNumber
  encryptedIFSC <- encrypt req.bankDetails.ifscCode

  -- Generate application ID and record ID for membership record
  applicationId <- generateGUIDText
  now <- getCurrentTime

  -- Convert VehicleType enum to Text format for storage
  let vehicleTypeText = case req.vehicleInfo.vehicleType of
        APITypes.TwoWheeler -> "2Wheeler"
        APITypes.ThreeWheeler -> "3Wheeler"
        APITypes.FourWheeler -> "4Wheeler"

  -- Convert FuelType list to Text list
  let stripParentheses :: String -> String
      stripParentheses = filter (\c -> c `notElem` ['(', ')'])
  let fuelTypesText = map (T.pack . stripParentheses . show) req.vehicleInfo.fuelTypes

  -- Create domain type
  let membership =
        Domain.StclMembership
          { Domain.id = Kernel.Types.Id.Id orderId,
            Domain.driverId = driverId,
            Domain.applicationId = applicationId,
            Domain.shortId = Just shortIdText,
            Domain.firstName = req.firstName,
            Domain.lastName = req.lastName,
            Domain.memberCategory = req.memberCategory,
            Domain.numberOfShares = req.numberOfShares,
            Domain.dateOfBirth = req.dateOfBirth,
            Domain.panNumber = encryptedPAN,
            Domain.mobileNumber = encryptedMobile,
            Domain.emailId = req.emailId,
            Domain.fatherMotherName = req.fatherMotherName,
            Domain.addressStreetAddress1 = req.address.streetAddress1,
            Domain.addressStreetAddress2 = req.address.streetAddress2,
            Domain.addressCity = req.address.city,
            Domain.addressState = req.address.stateName,
            Domain.addressPostalCode = req.address.postalCode,
            Domain.addressProofType = req.addressProofType,
            Domain.addressProofImageId = req.addressProofImageId,
            Domain.bankName = req.bankDetails.bankName,
            Domain.bankBranch = req.bankDetails.branch,
            Domain.accountNumber = encryptedAccountNumber,
            Domain.ifscCode = encryptedIFSC,
            Domain.vehicleType = vehicleTypeText,
            Domain.fuelTypes = fuelTypesText,
            Domain.nomineeName = req.nomineeInfo.nomineeName,
            Domain.declarationPlace = req.declaration.place,
            Domain.declarationDate = req.declaration.date,
            Domain.declarationSignature = req.declaration.signature,
            Domain.termsAccepted = req.declaration.termsAccepted,
            Domain.status = Domain.PENDING,
            Domain.paymentStatus = Nothing,
            Domain.isAdditionalSharePurchase = Just False,
            Domain.applicationCount = Nothing,
            Domain.shareStartCount = Nothing,
            Domain.shareEndCount = Nothing,
            Domain.merchantId = merchantId,
            Domain.merchantOperatingCityId = merchantOperatingCityId,
            Domain.createdAt = now,
            Domain.updatedAt = now
          }

  -- Save to database
  QStclMembership.create membership

  -- Return PaymentTypes.CreateOrderResp from createOrderV2
  return createOrderResp

postBuyAdditionalShares ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Person.Person),
      Kernel.Types.Id.Id Merchant.Merchant,
      Kernel.Types.Id.Id MerchantOperatingCity.MerchantOperatingCity
    ) ->
    APITypes.TopUpSharesReq ->
    Environment.Flow PaymentTypes.CreateOrderResp
  )
postBuyAdditionalShares (mbDriverId, merchantId, merchantOperatingCityId) req = do
  driverId <- mbDriverId & fromMaybeM (InvalidRequest "Driver ID not found in authentication context")

  when (req.numberOfShares <= 0) $
    throwError $ InvalidRequest "numberOfShares must be greater than 0"

  -- Per-MOC tunables, with module-level defaults if stclConfig (or any inner field) is unset.
  transporterConfig <-
    getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOperatingCityId Nothing))
      >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCityId.getId)
  let stclCfg = transporterConfig.stclConfig
      maxSharesPerDriver = fromMaybe defaultMaxSharesPerDriver (stclCfg >>= (.maxSharesPerDriver))
      pricePerShare = fromMaybe defaultPricePerShare (stclCfg >>= (.pricePerShare))
      pendingStaleMinutes = fromMaybe defaultPendingStaleMinutes (stclCfg >>= (.pendingStaleMinutes))

  -- Per-driver Redis lock so concurrent top-ups for the same driver can't both pass the cap
  -- check or both see "no PENDING" and create racing payment orders. The lock spans validation,
  -- resume-or-create decision, payment-order creation, and PENDING row insert so a second
  -- request enters with the new/resumed row already visible via findByDriverId.
  Redis.withLockRedisAndReturnValue (QStclMembership.stclMembershipDriverLockKey driverId.getId) 60 $ do
    existingApplications <- QStclMembership.findByDriverId driverId
    let submittedApps = filter (\a -> a.status == Domain.SUBMITTED) existingApplications
        pendingApps = filter (\a -> a.status == Domain.PENDING) existingApplications

    -- Top-up requires an existing SUBMITTED application as the source of KYC/address/bank/etc.
    when (null submittedApps) $
      throwError $ InvalidRequest "No existing membership found for this driver. Submit a new application first."

    now <- getCurrentTime
    let staleThreshold = fromIntegral (pendingStaleMinutes * 60) :: Kernel.Prelude.NominalDiffTime
        (stalePending, freshPending) =
          partition (\m -> diffUTCTime now m.createdAt > staleThreshold) pendingApps
    -- Abandoned PENDING (driver closed the payment app and never came back): retire so it stops blocking.
    forM_ stalePending $ \m -> QStclMembership.updateStatus Domain.REJECTED m.id

    person <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
    mbMobileNumber <- person.mobileNumber & fromMaybeM (InvalidRequest "Mobile number not found")
    decryptedMobile <- decrypt mbMobileNumber

    mbGatewayReferenceId <- do
      mbServiceConfig <- CQMSC.findByServiceAndCity (DMSC.MembershipPaymentService PaymentService.Juspay) merchantOperatingCityId
      case mbServiceConfig of
        Just serviceConfig -> case serviceConfig.serviceConfig of
          DMSC.MembershipPaymentServiceConfig paymentServiceConfig ->
            pure $ PaymentInterface.getGatewayReferenceId paymentServiceConfig
          _ -> pure Nothing
        Nothing -> pure Nothing

    let paymentServiceType = fromMaybe DOrder.STCL req.paymentServiceType

    -- Resolve the requested amount up front so we can compare it against any in-flight order before deciding
    -- whether to resume that order or create a fresh one for the new intent.
    let resolvedReqAmount = toHighPrecMoney $ fromMaybe (Money (req.numberOfShares * pricePerShare)) req.amount

    -- Decide whether a fresh PENDING is actually resumable. We only resume if the new request matches the
    -- in-flight order on both numberOfShares and amount — otherwise the driver's intent has changed
    -- (e.g., they originally requested 1 share by mistake and now want 2) and silently replaying the old
    -- payment link would charge them for the wrong quantity. Mismatched / orphan PENDING is retired so the
    -- new-order flow below can proceed.
    mbResumable <- case freshPending of
      [] -> pure Nothing
      (existing : _) -> do
        mbOrder <- QOrder.findById (Kernel.Types.Id.Id existing.id.getId)
        case mbOrder of
          Just o
            | existing.numberOfShares == req.numberOfShares,
              o.amount == resolvedReqAmount ->
              pure (Just (existing, o))
          _ -> do
            QStclMembership.updateStatus Domain.REJECTED existing.id
            pure Nothing

    case mbResumable of
      Just (existing, existingOrder) -> do
        -- Same intent as the in-flight order: re-issue the same CreateOrderResp (same Juspay orderId) so
        -- the frontend resumes the existing payment screen. createOrderService recognises the orderId and
        -- returns the stored payment links — see Lib.Payment.Domain.Action.createOrderService.
        nwAddress <- asks (.nwAddress)
        let resumeReq =
              PaymentTypes.CreateOrderReq
                { orderId = existing.id.getId,
                  orderShortId = existingOrder.shortId.getShortId,
                  amount = existingOrder.amount,
                  customerId = driverId.getId,
                  customerEmail = existing.emailId,
                  customerPhone = decryptedMobile,
                  customerFirstName = Just existing.firstName,
                  customerLastName = Just existing.lastName,
                  createMandate = Nothing,
                  mandateMaxAmount = Nothing,
                  mandateFrequency = Nothing,
                  mandateStartDate = Nothing,
                  mandateEndDate = Nothing,
                  metadataGatewayReferenceId = mbGatewayReferenceId,
                  optionsGetUpiDeepLinks = Nothing,
                  metadataExpiryInMins = Nothing,
                  splitSettlementDetails = Nothing,
                  webhookUrl = Just nwAddress,
                  basket = Nothing,
                  paymentRules = Nothing,
                  autoRefundPostSuccess = Nothing,
                  paymentFilter = Nothing,
                  udf1 = Nothing
                }
        SharedLogic.Payment.createOrderV2 (driverId, merchantId, merchantOperatingCityId) resumeReq (Just paymentServiceType)
      Nothing -> do
        -- No PENDING (or the in-flight one was mismatched and just retired above): create a new top-up.
        let existingShares = sum (map Domain.numberOfShares submittedApps)
        when (existingShares + req.numberOfShares > maxSharesPerDriver) $
          throwError $
            InvalidRequest $
              "Total shares for driver cannot exceed " <> T.pack (show maxSharesPerDriver)
                <> ". Existing shares: "
                <> T.pack (show existingShares)
                <> ", requested: "
                <> T.pack (show req.numberOfShares)

        latest <- case sortOn (Down . Domain.createdAt) submittedApps of
          (m : _) -> pure m
          [] -> throwError $ InvalidRequest "No existing membership found for this driver."

        orderId <- generateGUIDText
        orderShortId <- generateShortId
        let shortIdText = orderShortId.getShortId
        applicationId <- generateGUIDText

        nwAddress <- asks (.nwAddress)

        let createOrderReq =
              PaymentTypes.CreateOrderReq
                { orderId = orderId,
                  orderShortId = shortIdText,
                  amount = resolvedReqAmount,
                  customerId = driverId.getId,
                  customerEmail = latest.emailId,
                  customerPhone = decryptedMobile,
                  customerFirstName = Just latest.firstName,
                  customerLastName = Just latest.lastName,
                  createMandate = Nothing,
                  mandateMaxAmount = Nothing,
                  mandateFrequency = Nothing,
                  mandateStartDate = Nothing,
                  mandateEndDate = Nothing,
                  metadataGatewayReferenceId = mbGatewayReferenceId,
                  optionsGetUpiDeepLinks = Nothing,
                  metadataExpiryInMins = Nothing,
                  splitSettlementDetails = Nothing,
                  webhookUrl = Just nwAddress,
                  basket = Nothing,
                  paymentRules = Nothing,
                  autoRefundPostSuccess = Nothing,
                  paymentFilter = Nothing,
                  udf1 = Nothing
                }

        createOrderResp <- SharedLogic.Payment.createOrderV2 (driverId, merchantId, merchantOperatingCityId) createOrderReq (Just paymentServiceType)

        -- New PENDING row inherits KYC/address/bank/vehicle/nominee/declaration from the latest SUBMITTED row.
        -- The share-allocation fields are left Nothing and will be populated by stclMemberShipOrderStatusHandler
        -- on payment success.
        let newMembership =
              latest
                { Domain.id = Kernel.Types.Id.Id orderId,
                  Domain.applicationId = applicationId,
                  Domain.shortId = Just shortIdText,
                  Domain.numberOfShares = req.numberOfShares,
                  Domain.status = Domain.PENDING,
                  Domain.paymentStatus = Nothing,
                  Domain.isAdditionalSharePurchase = Just True,
                  Domain.applicationCount = Nothing,
                  Domain.shareStartCount = Nothing,
                  Domain.shareEndCount = Nothing,
                  Domain.createdAt = now,
                  Domain.updatedAt = now
                }
        QStclMembership.create newMembership

        return createOrderResp

putUpdateApplication ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Person.Person),
      Kernel.Types.Id.Id Merchant.Merchant,
      Kernel.Types.Id.Id MerchantOperatingCity.MerchantOperatingCity
    ) ->
    APITypes.UpdateMembershipApplicationReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
putUpdateApplication (mbDriverId, _merchantId, _merchantOperatingCityId) req = do
  driverId' <- mbDriverId & fromMaybeM (InvalidRequest "Driver ID not found in authentication context")

  -- Edits apply to every SUBMITTED and PENDING allotment for this driver so dashboard queries don't
  -- surface stale KYC/address/bank values on older rows, and so an in-flight top-up payment carries
  -- the up-to-date details when it flips to SUBMITTED. The latest SUBMITTED row is used as the
  -- source for "keep existing" defaults when a field is omitted from the request — we require at
  -- least one SUBMITTED row (PENDING-only drivers haven't completed a purchase yet).
  allRows <- QStclMembership.findByDriverId driverId'
  let editableRows = filter (\m -> m.status == Domain.SUBMITTED || m.status == Domain.PENDING) allRows
      submittedSortedDesc = sortOn (Down . Domain.createdAt) (filter (\m -> m.status == Domain.SUBMITTED) editableRows)
  latest <- case submittedSortedDesc of
    [] -> throwError $ InvalidRequest "No membership application found for this driver"
    (m : _) -> pure m

  -- Bank details: validate confirmAccountNumber matches and re-encrypt account number/IFSC.
  (newBankBranch, newAccountNumber, newIfscCode) <- case req.bankDetails of
    Nothing -> pure (latest.bankBranch, latest.accountNumber, latest.ifscCode)
    Just bd -> do
      unless (bd.accountNumber == bd.confirmAccountNumber) $
        throwError $ InvalidRequest "Account number and confirm account number do not match"
      encAcc <- encrypt bd.accountNumber
      encIfsc <- encrypt bd.ifscCode
      pure (bd.branch, encAcc, encIfsc)

  let (newStreet1, newStreet2, newCity, newState, newPostal) = case req.address of
        Nothing ->
          ( latest.addressStreetAddress1,
            latest.addressStreetAddress2,
            latest.addressCity,
            latest.addressState,
            latest.addressPostalCode
          )
        Just a ->
          ( a.streetAddress1,
            a.streetAddress2,
            a.city,
            a.stateName,
            a.postalCode
          )

  let newVehicleType = case req.vehicleType of
        Nothing -> latest.vehicleType
        Just APITypes.TwoWheeler -> "2Wheeler"
        Just APITypes.ThreeWheeler -> "3Wheeler"
        Just APITypes.FourWheeler -> "4Wheeler"

  let newNomineeName = fromMaybe latest.nomineeName req.nomineeName
  let newAddressProofType = req.addressProofType <|> latest.addressProofType
  let newAddressProofImageId = req.addressProofImageId <|> latest.addressProofImageId

  now <- getCurrentTime
  forM_ editableRows $ \m ->
    QStclMembership.updateByPrimaryKey
      m
        { Domain.bankBranch = newBankBranch,
          Domain.accountNumber = newAccountNumber,
          Domain.ifscCode = newIfscCode,
          Domain.addressStreetAddress1 = newStreet1,
          Domain.addressStreetAddress2 = newStreet2,
          Domain.addressCity = newCity,
          Domain.addressState = newState,
          Domain.addressPostalCode = newPostal,
          Domain.addressProofType = newAddressProofType,
          Domain.addressProofImageId = newAddressProofImageId,
          Domain.vehicleType = newVehicleType,
          Domain.nomineeName = newNomineeName,
          Domain.updatedAt = now
        }
  pure Kernel.Types.APISuccess.Success

getMembership ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Person.Person),
      Kernel.Types.Id.Id Merchant.Merchant,
      Kernel.Types.Id.Id MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow APITypes.MembershipDetailsResp
  )
getMembership (mbDriverId, _merchantId, _merchantOperatingCityId) = do
  -- Extract and validate driver ID from authentication token
  driverId' <- mbDriverId & fromMaybeM (InvalidRequest "Driver ID not found in authentication context")

  -- Aggregate across all SUBMITTED allotments for this driver (top-ups create new rows).
  -- Canonical KYC/address/bank/vehicle/nominee fields are read from the latest allotment.
  rawMemberships <- QStclMembership.findByDriverIdAndStatus driverId' Domain.SUBMITTED
  let sortedDesc = sortOn (Down . Domain.createdAt) rawMemberships
  membership@Domain.StclMembership {..} <- case sortedDesc of
    [] -> throwError $ InvalidRequest "No membership application found for this driver"
    (m : _) -> pure m

  let totalShares = sum (map Domain.numberOfShares rawMemberships)
      shareAllotments =
        map
          ( \m ->
              APITypes.ShareAllotment
                { APITypes.applicationId = Domain.applicationId m,
                  APITypes.numberOfShares = Domain.numberOfShares m,
                  APITypes.applicationCount = Domain.applicationCount m,
                  APITypes.shareStartCount = Domain.shareStartCount m,
                  APITypes.shareEndCount = Domain.shareEndCount m,
                  APITypes.submittedAt = Domain.updatedAt m
                }
          )
          sortedDesc

  -- Helper function to mask sensitive data (show last 4 digits with XXXX prefix)
  let maskSensitiveData :: Kernel.Prelude.Text -> Kernel.Prelude.Text
      maskSensitiveData value =
        let len = T.length value
            last4 = if len >= 4 then T.takeEnd 4 value else value
            xCount = max 0 (len - 4)
         in T.replicate xCount "X" <> last4

  -- Decrypt and mask sensitive fields
  decryptedPAN <- decrypt membership.panNumber
  decryptedMobile <- decrypt membership.mobileNumber
  decryptedAccountNumber <- decrypt membership.accountNumber
  decryptedIFSC <- decrypt membership.ifscCode

  let maskedPAN = maskSensitiveData decryptedPAN
      maskedMobile = maskSensitiveData decryptedMobile
      maskedAccountNumber = maskSensitiveData decryptedAccountNumber
      maskedIFSC = maskSensitiveData decryptedIFSC

  -- Convert VehicleType Text back to enum
  vehicleTypeEnum <- case membership.vehicleType of
    "2Wheeler" -> pure APITypes.TwoWheeler
    "3Wheeler" -> pure APITypes.ThreeWheeler
    "4Wheeler" -> pure APITypes.FourWheeler
    _ -> throwError $ InvalidRequest $ "Invalid vehicle type: " <> membership.vehicleType

  -- Convert FuelType Text list back to enum list
  let parseFuelType :: Kernel.Prelude.Text -> Environment.Flow APITypes.FuelType
      parseFuelType fuelText = case fuelText of
        "EV" -> pure APITypes.EV
        "Petrol" -> pure APITypes.Petrol
        "Diesel" -> pure APITypes.Diesel
        "CNG" -> pure APITypes.CNG
        "Other" -> pure APITypes.Other
        _ -> throwError $ InvalidRequest $ "Invalid fuel type: " <> fuelText
  fuelTypesEnum <- traverse parseFuelType membership.fuelTypes

  -- Build response
  return $
    APITypes.MembershipDetailsResp
      { APITypes.id = Kernel.Types.Id.getId membership.id,
        APITypes.driverId = Kernel.Types.Id.getId membership.driverId,
        APITypes.panNumber = maskedPAN,
        APITypes.mobileNumber = maskedMobile,
        APITypes.address =
          APITypes.Address
            { APITypes.streetAddress1 = membership.addressStreetAddress1,
              APITypes.streetAddress2 = membership.addressStreetAddress2,
              APITypes.city = membership.addressCity,
              APITypes.stateName = membership.addressState,
              APITypes.postalCode = membership.addressPostalCode
            },
        APITypes.numberOfShares = totalShares,
        APITypes.shareAllotments = shareAllotments,
        -- Deprecated legacy fields, populated from the latest allotment for backward compatibility.
        APITypes.applicationCount = membership.applicationCount,
        APITypes.shareStartCount = membership.shareStartCount,
        APITypes.shareEndCount = membership.shareEndCount,
        APITypes.bankDetails =
          APITypes.BankDetails
            { APITypes.bankName = membership.bankName,
              APITypes.branch = membership.bankBranch,
              APITypes.accountNumber = maskedAccountNumber,
              APITypes.ifscCode = maskedIFSC
            },
        APITypes.vehicleInfo =
          APITypes.VehicleInfo
            { APITypes.vehicleType = vehicleTypeEnum,
              APITypes.fuelTypes = fuelTypesEnum
            },
        APITypes.nomineeInfo =
          APITypes.NomineeInfo
            { APITypes.nomineeName = membership.nomineeName,
              APITypes.nomineeAadhar = Nothing
            },
        APITypes.declaration =
          APITypes.Declaration
            { APITypes.place = membership.declarationPlace,
              APITypes.date = membership.declarationDate,
              APITypes.signature = membership.declarationSignature,
              ..
            },
        ..
      }

-- Handle STCL membership order status updates
stclMemberShipOrderStatusHandler ::
  ( EsqDBFlow m r,
    MonadFlow m,
    CacheFlow m r,
    Redis.HedisFlow m r
  ) =>
  LibPayment.PaymentStatusResp ->
  Id DOrder.PaymentOrder ->
  m ()
stclMemberShipOrderStatusHandler paymentStatusResp paymentOrderId = do
  logInfo $ "STCL Membership Order Status Handler - Payment Order ID: " <> show paymentOrderId
  logInfo $ "STCL Membership Order Status Handler - Payment Status Response: " <> show paymentStatusResp

  -- Extract payment status string
  let paymentStatusText = case paymentStatusResp of
        LibPayment.PaymentStatus {status} -> Just $ show status
        LibPayment.MandatePaymentStatus {status} -> Just $ show status
        _ -> Nothing
  logInfo $ "Payment Status Text: " <> show paymentStatusText

  -- Find StclMembership by orderId (orderId is the same as membership.id)
  -- Convert Id PaymentOrder to Id StclMembership using the underlying Text
  let membershipId = Kernel.Types.Id.Id (paymentOrderId.getId)
  logInfo $ "Looking up membership with ID: " <> show membershipId
  mbMembership <- QStclMembership.findById membershipId

  -- If not found by ID, try looking up by shortId (from payment order)
  mbMembership' <- case mbMembership of
    Just _ -> pure mbMembership
    Nothing -> do
      -- Get payment order to find shortId
      paymentOrder <- QOrder.findById paymentOrderId
      case paymentOrder of
        Just order -> do
          logInfo $ "Membership not found by ID, trying shortId: " <> show order.shortId.getShortId
          QStclMembership.findByShortId (Just order.shortId.getShortId)
        Nothing -> do
          logError $ "Payment order not found: " <> show paymentOrderId
          pure Nothing

  case mbMembership' of
    Just membership -> do
      logInfo $ "Found membership: " <> show membership.id <> ", current status: " <> show membership.status
      -- Monotonic guard: once SUBMITTED, never regress regardless of replayed or out-of-order webhooks
      if membership.status == Domain.SUBMITTED
        then logInfo $ "Membership already SUBMITTED, skipping update (idempotent)"
        else do
          let newStatus = case paymentStatusResp of
                LibPayment.PaymentStatus {status}
                  | status == PaymentTypes.CHARGED -> Domain.SUBMITTED
                  | otherwise -> Domain.PENDING
                LibPayment.MandatePaymentStatus {status}
                  | status == PaymentTypes.CHARGED -> Domain.SUBMITTED
                  | otherwise -> Domain.PENDING
                _ -> Domain.PENDING
          logInfo $ "Updating membership to status: " <> show newStatus <> ", paymentStatus: " <> show paymentStatusText
          -- When transitioning to SUBMITTED, update application count and share range FIRST (before status update)
          -- so findLatestSubmittedMembership doesn't return the current record.
          -- Guard against current status to ensure counts are only incremented once.
          when (newStatus == Domain.SUBMITTED && membership.status /= Domain.SUBMITTED) $ do
            QStclMembership.updateApplicationAndShareCounts membership.id membership.numberOfShares
            logInfo $ "Updated application and share counts for SUBMITTED membership"
          QStclMembership.updateStatusAndPaymentStatus newStatus paymentStatusText membership.id
          logInfo $ "Successfully updated membership"
    Nothing -> do
      logError $ "Membership not found for payment order ID: " <> show paymentOrderId <> ", membership ID: " <> show membershipId
