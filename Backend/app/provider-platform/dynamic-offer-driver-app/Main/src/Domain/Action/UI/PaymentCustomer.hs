{-# OPTIONS_GHC -Wwarn=unused-imports #-}

-- | Gateway customer for the driver app's in-app UPI flow.
--
-- The SDK needs a customerId plus a short-lived client auth token before it can
-- list the driver's UPI apps / VPAs, and that pair is not part of a create-order
-- response. rider-app exposes the same thing as @GET \/payment\/customer@
-- ('Domain.Action.UI.RidePayment.getPaymentCustomer').
--
-- The token is persisted in @payment_customer@ and only refetched from juspay when
-- it has (nearly) expired, or when the driver has been moved to a different juspay
-- merchant since it was minted.
module Domain.Action.UI.PaymentCustomer (getPaymentCustomer) where

import qualified API.Types.UI.PaymentCustomer as APIT
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantServiceConfig
import qualified Domain.Types.PaymentCustomer as DPaymentCustomer
import qualified Domain.Types.Person
import qualified Domain.Types.Plan
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B (runInReplica)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import qualified Storage.Queries.PaymentCustomer as QPaymentCustomer
import qualified Storage.Queries.Person as QP
import Tools.Error
import qualified Tools.Payment as TPayment

-- | Safety margin on the stored token: it is treated as spent this long before its
--   real expiry, so the SDK is never handed a token that dies mid-handshake.
clientAuthTokenSkew :: Kernel.Prelude.NominalDiffTime
clientAuthTokenSkew = 60

getPaymentCustomer ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Domain.Types.Plan.ServiceNames ->
    Environment.Flow APIT.PaymentCustomerResp
  )
getPaymentCustomer (mbPersonId, merchantId, merchantOpCityId) mbServiceName = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  driver <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let serviceName = fromMaybe Domain.Types.Plan.YATRI_SUBSCRIPTION mbServiceName
  subscriptionConfig <-
    CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOpCityId Nothing serviceName
      >>= fromMaybeM (NoSubscriptionConfigForService merchantOpCityId.getId $ show serviceName)
  paymentServiceName <- TPayment.decidePaymentService subscriptionConfig.paymentServiceName driver.clientSdkVersion driver.merchantOperatingCityId
  mbCustomer <- QPaymentCustomer.findByDriverIdAndServiceName driverId serviceName
  now <- getCurrentTime
  if isUsable paymentServiceName now mbCustomer
    then do
      logDebug $ "getPaymentCustomer: stored client auth token still valid for driver " <> driverId.getId
      pure $ mkPaymentCustomerResp driverId (mbCustomer >>= (.clientAuthToken)) (mbCustomer >>= (.clientAuthTokenExpiry))
    else do
      let lockKey = "PaymentCustomer:Refresh:" <> driverId.getId <> ":" <> show serviceName
      Redis.withLockRedisAndReturnValue lockKey 60 $ do
        mbLocked <- QPaymentCustomer.findByDriverIdAndServiceName driverId serviceName
        now' <- getCurrentTime
        if isUsable paymentServiceName now' mbLocked
          then pure $ mkPaymentCustomerResp driverId (mbLocked >>= (.clientAuthToken)) (mbLocked >>= (.clientAuthTokenExpiry))
          else do
            driverPhone <- driver.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
            let createCustomerReq =
                  Payment.CreateCustomerReq
                    { email = driver.email,
                      name = Just driver.firstName,
                      lastName = driver.lastName,
                      phone = driverPhone,
                      objectReferenceId = driverId.getId,
                      mobileCountryCode = driver.mobileCountryCode,
                      optionsGetClientAuthToken = Just True
                    }
            resp <- TPayment.getCustomerOrCreateCustomer merchantId merchantOpCityId paymentServiceName (Just driverId.getId) createCustomerReq
            case mbLocked of
              Just _ -> QPaymentCustomer.updateClientAuthToken resp.clientAuthToken resp.clientAuthTokenExpiry resp.customerId paymentServiceName driverId serviceName
              Nothing -> QPaymentCustomer.create =<< buildPaymentCustomer driverId serviceName paymentServiceName resp
            pure $ mkPaymentCustomerResp driverId resp.clientAuthToken resp.clientAuthTokenExpiry
  where
    isUsable ::
      Domain.Types.MerchantServiceConfig.ServiceName ->
      Kernel.Prelude.UTCTime ->
      Kernel.Prelude.Maybe DPaymentCustomer.PaymentCustomer ->
      Bool
    isUsable expectedService at = \case
      Nothing -> False
      Just customer ->
        customer.paymentServiceName == expectedService
          && maybe False (\expiry -> diffUTCTime expiry at > clientAuthTokenSkew) customer.clientAuthTokenExpiry

    buildPaymentCustomer ::
      Kernel.Types.Id.Id Domain.Types.Person.Person ->
      Domain.Types.Plan.ServiceNames ->
      Domain.Types.MerchantServiceConfig.ServiceName ->
      Payment.CreateCustomerResp ->
      Environment.Flow DPaymentCustomer.PaymentCustomer
    buildPaymentCustomer forDriverId forServiceName mintedBy custResp = do
      at <- getCurrentTime
      pure
        DPaymentCustomer.PaymentCustomer
          { driverId = forDriverId,
            serviceName = forServiceName,
            paymentServiceName = mintedBy,
            customerId = custResp.customerId,
            clientAuthToken = custResp.clientAuthToken,
            clientAuthTokenExpiry = custResp.clientAuthTokenExpiry,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOpCityId,
            createdAt = at,
            updatedAt = at
          }

mkPaymentCustomerResp ::
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  APIT.PaymentCustomerResp
mkPaymentCustomerResp driverId clientAuthToken clientAuthTokenExpiry =
  APIT.PaymentCustomerResp
    { customerId = driverId.getId,
      clientAuthToken = clientAuthToken,
      clientAuthTokenExpiry = clientAuthTokenExpiry
    }
