-- | Confirming FRFS legs whose fare is fully covered by a pass.
-- A fully pass-covered leg has no payment order and no @FRFSTicketBookingPayment@ row, so nothing
-- in the payment pipeline will ever drive it to confirm. A standalone booking is therefore confirmed
-- inline at booking time (see @SharedLogic.FRFSConfirm.postFrfsQuoteV2ConfirmUtil@).
-- A leg inside a multimodal journey must NOT be confirmed inline: its siblings may still be awaiting
-- payment, and confirming early issues a real ticket (and spends a pass trip) for a journey the rider
-- may abandon at the payment step. Journey legs are instead confirmed here, from exactly two places:
--   * "Lib.JourneyModule.Base" — right after all legs are confirmed, when the journey has no payable
--     leg at all (every leg pass-covered, so no payment will ever arrive to trigger anything).
--   * "SharedLogic.FRFSStatus" — when the journey's payment succeeds.
-- Both call sites are idempotent: only bookings still in 'NEW' are acted on, and the status flips to
-- 'CONFIRMING' before the BPP call.
module SharedLogic.FRFSPassConfirm
  ( confirmPassCoveredLegs,
    confirmPassCoveredLegsOfJourney,
    confirmOne,
  )
where

import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils (frfsVehicleCategoryToBecknVehicleCategory)
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBooking
import qualified ExternalBPP.CallAPI.Confirm as CallExternalBPP
import qualified ExternalBPP.CallAPI.Types as CallExternalBPP
import Kernel.External.Encryption
import Kernel.External.MasterCloudForward (HasMasterCloudForwarder)
import Kernel.Prelude
import Kernel.Types.Version (CloudType)
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import SharedLogic.FRFSFareCalculator (mkCategoryPriceItemFromQuoteCategories, mkFareParameters)
import qualified SharedLogic.FRFSPassOverride as FRFSPassOverride
import SharedLogic.FRFSUtils (getAllJourneyFrfsBookings)
import qualified SharedLogic.FRFSUtils as FRFSUtils
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.BecknConfig (BecknConfigDimensions (..))
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.Person as QP
import Tools.Error

-- | Confirm every fully pass-covered leg of the journey this booking belongs to.
-- No-op for a standalone booking: that path confirms inline at booking time.
confirmPassCoveredLegsOfJourney ::
  ( CallExternalBPP.FRFSConfirmFlow m r c,
    HasField "blackListedJobs" r [Text],
    HasField "cloudType" r (Maybe CloudType),
    HasMasterCloudForwarder r
  ) =>
  DFRFSTicketBooking.FRFSTicketBooking ->
  m ()
confirmPassCoveredLegsOfJourney booking = do
  (mbJourneyId, allJourneyBookings) <- getAllJourneyFrfsBookings booking
  whenJust mbJourneyId $ \_ -> confirmPassCoveredLegs allJourneyBookings

confirmPassCoveredLegs ::
  ( CallExternalBPP.FRFSConfirmFlow m r c,
    HasField "blackListedJobs" r [Text],
    HasField "cloudType" r (Maybe CloudType),
    HasMasterCloudForwarder r
  ) =>
  [DFRFSTicketBooking.FRFSTicketBooking] ->
  m ()
confirmPassCoveredLegs bookings = do
  let coveredLegs =
        filter
          ( \b ->
              FRFSPassOverride.isFullyPassCovered b.overriddenAmount
                && b.status `elem` [DFRFSTicketBooking.NEW, DFRFSTicketBooking.PAYMENT_PENDING, DFRFSTicketBooking.APPROVED]
          )
          bookings
  forM_ coveredLegs $ \booking ->
    void $ withTryCatch "FRFSPassConfirm:confirmLeg" (confirmOne booking)

confirmOne ::
  ( CallExternalBPP.FRFSConfirmFlow m r c,
    HasField "blackListedJobs" r [Text],
    HasField "cloudType" r (Maybe CloudType),
    HasMasterCloudForwarder r
  ) =>
  DFRFSTicketBooking.FRFSTicketBooking ->
  m ()
confirmOne booking = do
  -- Resolved before the claim because claimBookingForConfirm writes validTill, and that needs
  -- bapConfig.confirmTTLSec. These are all cached lookups on a row we already hold.
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantDoesNotExist booking.merchantId.getId)
  merchantOperatingCity <- CQMOC.findById booking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
  let becknVehicleCategory = frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType
  bapConfig <-
    getOneConfig
      (BecknConfigDimensions {merchantOperatingCityId = merchantOperatingCity.id.getId, merchantId = merchant.id.getId, domain = Just (show Spec.FRFS), vehicleCategory = Just becknVehicleCategory, becknProtocol = Nothing})
      (Just (maybeToList <$> CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchant.id (show Spec.FRFS) becknVehicleCategory))
      >>= fromMaybeM (InternalError "Beckn Config not found")
  now <- getCurrentTime
  let validTill = addUTCTime (maybe 60 intToNominalDiffTime bapConfig.confirmTTLSec) now
  mbClaimed <- FRFSUtils.claimBookingForConfirm booking.id validTill
  case mbClaimed of
    Nothing -> logInfo $ "FRFSPassConfirm: not claiming leg (already confirming, or a payment webhook holds the booking) bookingId=" <> booking.id.getId
    Just latest -> do
      -- Everything past the claim runs under this, because the claim has already moved the booking
      -- to CONFIRMING: a throw from any of the lookups below -- rider, quote categories, decrypt --
      -- would otherwise leave the leg CONFIRMING with no failure_reason and nothing to resolve it. A
      -- pass-only journey has no later payment to retrigger the confirm, so frfsBookingStatus would
      -- only fail it once validTill passed, and only if the rider happened to poll.
      afterClaim <- withTryCatch "FRFSPassConfirm:afterClaim" $ do
        rider <- QP.findById latest.riderId >>= fromMaybeM (PersonNotFound latest.riderId.getId)
        quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId latest.quoteId
        let mRiderName = rider.firstName <&> (\fName -> rider.lastName & maybe fName (\lName -> fName <> " " <> lName))
        mRiderNumber <- mapM decrypt rider.mobileNumber
        void $ QFRFSTicketBooking.updateOnInitDone (Just True) latest.id
        let repricedTotal = (mkFareParameters (mkCategoryPriceItemFromQuoteCategories quoteCategories)).totalPrice
            repriced = latest {DFRFSTicketBooking.totalPrice = repricedTotal}
        void $ QFRFSTicketBooking.updateTotalPriceById repricedTotal latest.id
        logInfo $ "FRFSPassConfirm: confirming pass-covered leg bookingId=" <> latest.id.getId
        -- CallExternalBPP.confirm returns Left for an error it handled, but THROWS on a transport
        -- or decode failure -- and confirmLeg's withTryCatch swallows that, so the two writes below
        -- never ran and the leg was left in CONFIRMING with an empty failure_reason, forever. That
        -- is the single largest bucket of stuck-CONFIRMING bookings in production, so fold a throw
        -- into the same Left path: every failure gets a reason and a terminal status.
        --
        -- Safe to mark FAILED even if the BPP did issue a ticket we never saw the response for:
        -- OnConfirm sets CONFIRMED unconditionally, so a late on_confirm still wins and debits the
        -- pass. The trip is only debited there, so nothing is lost on this path either way.
        confirmResp <-
          withTryCatch "FRFSPassConfirm:bppConfirm" (CallExternalBPP.confirm merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) repriced quoteCategories repriced.isSingleMode) >>= \case
            Right resp -> pure resp
            Left err -> pure (Left ("BPP confirm threw: " <> show err))
        case confirmResp of
          Left err -> do
            void $ QFRFSTicketBooking.updateFailureReasonById (Just err) latest.id
            void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED latest.id
          Right _ -> pure ()

      case afterClaim of
        Right () -> pure ()
        Left err -> do
          logError $ "FRFSPassConfirm: leg failed after the claim bookingId=" <> latest.id.getId <> " err=" <> show err
          void $ QFRFSTicketBooking.updateFailureReasonById (Just ("Pass leg confirm failed: " <> show err)) latest.id
          void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED latest.id
      FRFSUtils.releasePaymentSuccessLock latest.id
