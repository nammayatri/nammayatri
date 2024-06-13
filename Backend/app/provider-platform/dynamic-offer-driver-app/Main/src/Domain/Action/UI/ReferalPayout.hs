{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.ReferalPayout where

import qualified API.Types.UI.ReferalPayout
import Data.OpenApi (ToSchema)
import Data.Text hiding (map)
import qualified Domain.Types.DailyStats as DS
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import qualified Kernel.External.Payment.Juspay.Types.CreateOrder
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
-- import qualified Kernel.External.Payment.Interface.Types as Payment
-- import qualified Kernel.External.Payment.Types as Payment
import Servant hiding (throwError)
-- import qualified Lib.Payment.Domain.Action as DPayment
-- import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC
import qualified Storage.Queries.DailyStats as QDS
import qualified Storage.Queries.DriverInformation as DrInfo
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as PersonQuery
import Tools.Auth
import Tools.Error

getReferralEarnings ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.ReferalPayout.ReferralEarningsReq ->
    Environment.Flow API.Types.UI.ReferalPayout.ReferralEarningsRes
  )
getReferralEarnings (mbPersonId, _merchantId, merchantOpCityId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  -- person <- runInReplica $ PersonQuery.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let dates = [req.fromDate .. req.toDate]
  earnings <- catMaybes <$> (forM dates $ \date -> QDS.findByDriverIdAndDate personId date)
  driverStats <- runInReplica $ QDriverStats.findById personId >>= fromMaybeM (PersonNotFound personId.getId) -- throw error or not ? handle maybe then
  dInfo <- runInReplica $ DrInfo.findByPrimaryKey personId >>= fromMaybeM DriverInfoNotFound
  payoutConfig <- CPC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (InternalError "Payout config not present")
  let dailyEarnings = map parseDailyEarnings earnings
  return $
    API.Types.UI.ReferalPayout.ReferralEarningsRes
      { totalReferralCount = driverStats.totalReferralCounts,
        dailyEarnings = dailyEarnings,
        vpaId = dInfo.payoutVpa,
        orderId = Just "test", -- TODO: PAYMENTS
        orderStatus = Nothing, --Just "SUCCESS" -- check status of last order , if orderId is there
        referralRewardAmountPerRide = payoutConfig.referralRewardAmountPerRide
      }
  where
    parseDailyEarnings earning =
      API.Types.UI.ReferalPayout.DailyEarning
        { earnings = earning.referralEarnings,
          activatedItems = earning.activatedValidRides,
          earningDate = earning.merchantLocalDate,
          referrals = earning.referralCounts,
          status = earning.payoutStatus,
          payoutOrderId = Nothing,
          payoutOrderStatus = Nothing
        }

postDeleteVpa ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postDeleteVpa (mbPersonId, _merchantId, _merchantOpCityId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  driverInfo <- runInReplica $ DrInfo.findByPrimaryKey personId >>= fromMaybeM DriverInfoNotFound
  unless (isJust driverInfo.payoutVpa) $ throwError (InvalidRequest "Vpa Id does not Exists")
  void $ DrInfo.updatePayoutVpa Nothing personId -- Deleting the prev VPA (We can get this in payout order history)
  pure Kernel.Types.APISuccess.Success

getPayoutRegistration ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler Kernel.External.Payment.Juspay.Types.CreateOrder.CreateOrderResp
  )
getPayoutRegistration (mbPersonId, _merchantId, merchantOpCityId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  payoutConfig <- CPC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (InternalError "Payout config not present")
  -- DPayment.createOrderService
  throwError $ InvalidRequest "Not found"

-- customerEmail <- person.email & fromMaybeM (PersonFieldNotPresent "email") >>= decrypt
-- customerPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
-- let createOrderReq =
--       Payment.CreateOrderReq
--         { orderId = "",
--           orderShortId = "", -- should be Alphanumeric with character length less than 18.
--           amount = payoutConfig.,
--           customerId = person.id.getId,
--           customerEmail,
--           customerPhone,
--           customerFirstName = person.firstName,
--           customerLastName = person.lastName,
--           createMandate = Nothing,
--           mandateMaxAmount = Nothing,
--           mandateFrequency = Nothing,
--           mandateStartDate = Nothing,
--           mandateEndDate = Nothing,
--           optionsGetUpiDeepLinks = Nothing,
--           metadataExpiryInMins = Nothing,
--           metadataGatewayReferenceId = Nothing
--         }

-- let commonMerchantId = cast @DM.Merchant @DPayment.Merchant merchantId
--     commonPersonId = cast @DP.Person @DPayment.Person personId
--     createOrderCall = Payment.createOrder merchantId person.merchantOperatingCityId Nothing Payment.Normal -- api call
-- DPayment.createOrderService commonMerchantId commonPersonId createOrderReq createOrderCall >>= fromMaybeM (InternalError "Order expired please try again")
