{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.StclMembership (postSubmitApplication, getMembership, stclMemberShipOrderStatusHandler) where

import qualified API.Types.UI.StclMembership as APITypes
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
import Kernel.Types.Common (Money (..), toHighPrecMoney)
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as LibPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.Common as LibPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import Storage.Beam.Payment ()
import qualified SharedLogic.Payment
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.StclMembership as QStclMembership
import Tools.Auth

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

  -- Check for existing applications to prevent duplicates
  -- If any existing application has status SUBMITTED, throw error
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
            basket = Nothing
          }

  -- PaymentServiceType for createOrderService (STCL)
  let paymentServiceType = fromMaybe DOrder.STCL req.paymentServiceType

  -- Create payment order
  createOrderResp <- SharedLogic.Payment.createOrderV2 (driverId, merchantId, merchantOperatingCityId) createOrderReq (Just paymentServiceType)

  -- Encrypt sensitive fields for storage
  encryptedAadhar <- encrypt req.aadharNumber
  encryptedPAN <- encrypt req.panNumber
  encryptedMobile <- encrypt req.mobileNumber
  encryptedAccountNumber <- encrypt req.bankDetails.accountNumber
  encryptedIFSC <- encrypt req.bankDetails.ifscCode
  encryptedNomineeAadhar <- encrypt req.nomineeInfo.nomineeAadhar

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
            Domain.aadharNumber = encryptedAadhar,
            Domain.panNumber = encryptedPAN,
            Domain.mobileNumber = encryptedMobile,
            Domain.emailId = req.emailId,
            Domain.fatherMotherName = req.fatherMotherName,
            Domain.addressStreetAddress1 = req.address.streetAddress1,
            Domain.addressStreetAddress2 = req.address.streetAddress2,
            Domain.addressCity = req.address.city,
            Domain.addressState = req.address.stateName,
            Domain.addressPostalCode = req.address.postalCode,
            Domain.bankName = req.bankDetails.bankName,
            Domain.bankBranch = req.bankDetails.branch,
            Domain.accountNumber = encryptedAccountNumber,
            Domain.ifscCode = encryptedIFSC,
            Domain.vehicleType = vehicleTypeText,
            Domain.fuelTypes = fuelTypesText,
            Domain.nomineeName = req.nomineeInfo.nomineeName,
            Domain.nomineeAadhar = encryptedNomineeAadhar,
            Domain.declarationPlace = req.declaration.place,
            Domain.declarationDate = req.declaration.date,
            Domain.declarationSignature = req.declaration.signature,
            Domain.termsAccepted = req.declaration.termsAccepted,
            Domain.status = Domain.PENDING,
            Domain.paymentStatus = Nothing,
            Domain.merchantId = merchantId,
            Domain.merchantOperatingCityId = merchantOperatingCityId,
            Domain.createdAt = now,
            Domain.updatedAt = now
          }

  -- Save to database
  QStclMembership.create membership

  -- Return PaymentTypes.CreateOrderResp from createOrderV2
  return createOrderResp

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

  -- Query database
  memberships <- QStclMembership.findByDriverId driverId'
  membership@Domain.StclMembership {..} <- case memberships of
    [] -> throwError $ InvalidRequest "No membership application found for this driver"
    (m : _) -> pure m

  -- Helper function to mask sensitive data (show last 4 digits with XXXX prefix)
  let maskSensitiveData :: Kernel.Prelude.Text -> Kernel.Prelude.Text
      maskSensitiveData value =
        let len = T.length value
            last4 = if len >= 4 then T.takeEnd 4 value else value
            xCount = max 0 (len - 4)
         in T.replicate xCount "X" <> last4

  -- Decrypt and mask sensitive fields
  decryptedAadhar <- decrypt membership.aadharNumber
  decryptedPAN <- decrypt membership.panNumber
  decryptedMobile <- decrypt membership.mobileNumber
  decryptedAccountNumber <- decrypt membership.accountNumber
  decryptedIFSC <- decrypt membership.ifscCode
  decryptedNomineeAadhar <- decrypt membership.nomineeAadhar

  let maskedAadhar = maskSensitiveData decryptedAadhar
      maskedPAN = maskSensitiveData decryptedPAN
      maskedMobile = maskSensitiveData decryptedMobile
      maskedAccountNumber = maskSensitiveData decryptedAccountNumber
      maskedIFSC = maskSensitiveData decryptedIFSC
      maskedNomineeAadhar = maskSensitiveData decryptedNomineeAadhar

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
        APITypes.aadharNumber = maskedAadhar,
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
              APITypes.nomineeAadhar = maskedNomineeAadhar
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
    CacheFlow m r
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
      let newStatus = case paymentStatusResp of
            LibPayment.PaymentStatus {status}
              | status == PaymentTypes.CHARGED -> Domain.SUBMITTED
              | otherwise -> Domain.PENDING
            LibPayment.MandatePaymentStatus {status}
              | status == PaymentTypes.CHARGED -> Domain.SUBMITTED
              | otherwise -> Domain.PENDING
            _ -> Domain.PENDING
      logInfo $ "Updating membership to status: " <> show newStatus <> ", paymentStatus: " <> show paymentStatusText
      -- Update membership with new status and payment status
      -- updateStatusAndPaymentStatus generates updatedAt internally
      QStclMembership.updateStatusAndPaymentStatus newStatus paymentStatusText membership.id
      logInfo $ "Successfully updated membership"
    Nothing -> do
      logError $ "Membership not found for payment order ID: " <> show paymentOrderId <> ", membership ID: " <> show membershipId
