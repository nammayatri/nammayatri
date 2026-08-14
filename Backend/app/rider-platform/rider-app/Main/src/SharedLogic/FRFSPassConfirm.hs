-- | Confirming FRFS legs whose fare is fully covered by a pass.
--
-- A fully pass-covered leg has no payment order and no @FRFSTicketBookingPayment@ row, so nothing
-- in the payment pipeline will ever drive it to confirm. A standalone booking is therefore confirmed
-- inline at booking time (see @SharedLogic.FRFSConfirm.postFrfsQuoteV2ConfirmUtil@).
--
-- A leg inside a multimodal journey must NOT be confirmed inline: its siblings may still be awaiting
-- payment, and confirming early issues a real ticket (and spends a pass trip) for a journey the rider
-- may abandon at the payment step. Journey legs are instead confirmed here, from exactly two places:
--
--   * "Lib.JourneyModule.Base" — right after all legs are confirmed, when the journey has no payable
--     leg at all (every leg pass-covered, so no payment will ever arrive to trigger anything).
--   * "SharedLogic.FRFSStatus" — when the journey's payment succeeds.
--
-- Both call sites are idempotent: only bookings still in 'NEW' are acted on, and the status flips to
-- 'CONFIRMING' before the BPP call.
module SharedLogic.FRFSPassConfirm
  ( confirmPassCoveredLegs,
    confirmPassCoveredLegsOfJourney,
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
import qualified SharedLogic.FRFSPassOverride as FRFSPassOverride
import SharedLogic.FRFSUtils (getAllJourneyFrfsBookings)
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

-- | Confirm the fully pass-covered bookings in the given set, skipping anything already past 'NEW'.
-- Each leg is confirmed independently; one leg failing does not abort the others.
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
          (\b -> FRFSPassOverride.isFullyPassCovered b.overriddenAmount && b.status == DFRFSTicketBooking.NEW)
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
  -- Re-read: a sibling leg's status call may have confirmed this one between the filter and here.
  latest <- QFRFSTicketBooking.findById booking.id >>= fromMaybeM (InvalidRequest $ "Invalid booking id " <> booking.id.getId)
  if latest.status /= DFRFSTicketBooking.NEW
    then logInfo $ "FRFSPassConfirm: leg no longer NEW, skipping bookingId=" <> latest.id.getId <> " status=" <> show latest.status
    else do
      merchant <- CQM.findById latest.merchantId >>= fromMaybeM (MerchantDoesNotExist latest.merchantId.getId)
      merchantOperatingCity <- CQMOC.findById latest.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound latest.merchantOperatingCityId.getId)
      rider <- QP.findById latest.riderId >>= fromMaybeM (PersonNotFound latest.riderId.getId)
      let becknVehicleCategory = frfsVehicleCategoryToBecknVehicleCategory latest.vehicleType
      bapConfig <-
        getOneConfig
          (BecknConfigDimensions {merchantOperatingCityId = merchantOperatingCity.id.getId, merchantId = merchant.id.getId, domain = Just (show Spec.FRFS), vehicleCategory = Just becknVehicleCategory})
          (Just (maybeToList <$> CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchant.id (show Spec.FRFS) becknVehicleCategory))
          >>= fromMaybeM (InternalError "Beckn Config not found")
      quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId latest.quoteId
      now <- getCurrentTime
      let validTill = addUTCTime (maybe 60 intToNominalDiffTime bapConfig.confirmTTLSec) now
          mRiderName = rider.firstName <&> (\fName -> rider.lastName & maybe fName (\lName -> fName <> " " <> lName))
      mRiderNumber <- mapM decrypt rider.mobileNumber
      void $ QFRFSTicketBooking.updateValidTillById validTill latest.id
      void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.CONFIRMING latest.id
      void $ QFRFSTicketBooking.updateOnInitDone (Just True) latest.id
      logInfo $ "FRFSPassConfirm: confirming pass-covered leg bookingId=" <> latest.id.getId
      spent <- FRFSPassOverride.spendTripForBooking rider latest
      if not spent
        then do
          void $ QFRFSTicketBooking.updateFailureReasonById (Just "Pass has no trips remaining") latest.id
          void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED latest.id
        else do
          confirmResp <- CallExternalBPP.confirm merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) latest quoteCategories Nothing
          case confirmResp of
            Left err -> do
              void $ QFRFSTicketBooking.updateFailureReasonById (Just err) latest.id
              void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED latest.id
              void $ withTryCatch "FRFSPassConfirm:releaseTrip" (FRFSPassOverride.releasePassOverrideTripOnFailure latest)
            Right _ -> pure ()
