{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.ReferalPayout where

import qualified API.Types.UI.ReferalPayout
import Data.OpenApi (ToSchema)
import Data.Text hiding (map)
import qualified Domain.Action.UI.Driver as DD
import qualified Domain.Types.DailyStats as DS
import qualified Domain.Types.DriverFee as DFee
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import qualified Domain.Types.Plan as DPlan
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import qualified Kernel.External.Payment.Juspay.Types.CreateOrder
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import qualified Kernel.Types.Price (Currency (..))
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC
import qualified Storage.Queries.DailyStats as QDS
import qualified Storage.Queries.DriverInformation as DrInfo
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as PersonQuery
import qualified Storage.Queries.Vehicle as QVeh
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
  let dates = [req.fromDate .. req.toDate]
  earnings <- catMaybes <$> (forM dates $ \date -> QDS.findByDriverIdAndDate personId date)
  driverStats <- runInReplica $ QDriverStats.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  dInfo <- runInReplica $ DrInfo.findByPrimaryKey personId >>= fromMaybeM DriverInfoNotFound
  vehicle <- QVeh.findById personId >>= fromMaybeM (DriverWithoutVehicle personId.getId)
  payoutConfig <- CPC.findByPrimaryKey merchantOpCityId vehicle.variant >>= fromMaybeM (InternalError "Payout config not present")
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
          payoutOrderId = earning.payoutOrderId,
          payoutOrderStatus = show <$> earning.payoutOrderStatus
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
    Environment.Flow Kernel.External.Payment.Juspay.Types.CreateOrder.CreateOrderResp
  )
getPayoutRegistration (mbPersonId, merchantId, merchantOpCityId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  vehicle <- QVeh.findById personId >>= fromMaybeM (DriverWithoutVehicle personId.getId)
  payoutConfig <- CPC.findByPrimaryKey merchantOpCityId vehicle.variant >>= fromMaybeM (InternalError "Payout config not present")
  unless payoutConfig.isPayoutEnabled $ throwError $ InvalidRequest "Payout Registration is Not Enabled"
  let (fee, cgst, sgst) = (1, 0, 0) -- add in payoutConfig
  clearDuesRes <- DD.clearDriverFeeWithCreate (personId, merchantId, merchantOpCityId) DPlan.YATRI_SUBSCRIPTION (fee, cgst, sgst) DFee.PAYOUT_REGISTRATION INR Nothing
  pure clearDuesRes.orderResp
