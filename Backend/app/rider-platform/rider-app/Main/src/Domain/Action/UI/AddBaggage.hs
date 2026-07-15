module Domain.Action.UI.AddBaggage
  ( postAddBaggageConfirm,
    AddBaggageConfirmReq (..),
  )
where

import qualified Beckn.ACL.Update as ACL
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingStatus as DBS
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Domain.Types.Trip (isRideOtpTrip)
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified SharedLogic.CallBPP as CallBPP
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQRC
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Booking as QRB
import Tools.Error

newtype AddBaggageConfirmReq = AddBaggageConfirmReq
  { numberOfLuggages :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

-- | Confirms a baggage addition by sending a BECKN update to BPP.
-- BPP recalculates fare against booking's existing fare policy with the new luggage count
-- and ships the new fare back via on_update.
postAddBaggageConfirm ::
  (Id DP.Person, Id DM.Merchant) ->
  Id DBooking.Booking ->
  AddBaggageConfirmReq ->
  Flow APISuccess
postAddBaggageConfirm (_personId, merchantId) bookingId req = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  unless (booking.status == DBS.NEW || booking.status == DBS.CONFIRMED) $
    throwError $ AddBaggageInvalidBookingStatus (show booking.status)
  unless (maybe False isRideOtpTrip booking.tripCategory) $
    throwError AddBaggageNotSupported
  -- NOTE: BAP Booking has no numberOfLuggages column today, so no same-count check.
  -- BPP handler is idempotent — replays will recompute the same fare.
  when (req.numberOfLuggages < 0) $
    throwError AddBaggageNegativeCount

  riderCfg <-
    getConfig (RiderConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) (Just (CQRC.findByMerchantOperatingCityId booking.merchantOperatingCityId))
      >>= fromMaybeM (RiderConfigNotFound booking.merchantOperatingCityId.getId)
  whenJust riderCfg.maxNumberOfLuggages $ \maxN ->
    when (req.numberOfLuggages > maxN) $ throwError (AddBaggageExceedsMax maxN)

  bppBookingId <- booking.bppBookingId & fromMaybeM (InvalidRequest "BPP booking ID not found")
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  msgId <- generateGUID

  let updateBuildReq =
        ACL.UpdateBuildReq
          { bppBookingId = bppBookingId,
            bppId = booking.providerId,
            bppUrl = booking.providerUrl,
            transactionId = booking.transactionId,
            merchant = merchant,
            messageId = msgId,
            city = merchant.defaultCity,
            details =
              ACL.UAddBaggageBuildReqDetails
                ACL.AddBaggageBuildReqDetails
                  { numberOfLuggages = req.numberOfLuggages
                  }
          }
  -- Optimistic BAP update is skipped. The BPP is authoritative for the fare;
  -- BAP mirrors via on_update after BPP confirms.
  updateMsg <- ACL.buildUpdateReq updateBuildReq
  void $ CallBPP.updateV2 booking.providerUrl updateMsg
  pure Success
