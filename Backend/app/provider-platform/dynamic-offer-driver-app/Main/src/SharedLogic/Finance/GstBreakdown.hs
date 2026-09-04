module SharedLogic.Finance.GstBreakdown
  ( computeGstBreakdownForRideOwner,
    computeGstBreakdownForPerson,
    computeGstBreakdownGSTIN,
  )
where

import Control.Applicative ((<|>))
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.FleetOwnerInformation as DFOI
import qualified Domain.Types.Location as DLocation
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id (Id)
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, logError, logWarning)
import qualified Lib.Finance.Invoice.Interface as Finance
import qualified Lib.Finance.Utils.GstBreakdown as Finance
import qualified SharedLogic.DriverIdentityInfo as DIInfo
import qualified Storage.Queries.DriverIdentityInfo as QDII
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.FleetOwnerInformation as QFOI

-- | Ride-earning GST: compare driver/fleet residence proof vs booking pickup location.
--   If 'ride.fleetOwnerId' is set, use the fleet owner's addressState; otherwise the driver's.
computeGstBreakdownForRideOwner ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  DTC.GstBreakup ->
  DLocation.Location -> -- booking.fromLocation (pickup)
  DRide.Ride ->
  Maybe DFOI.FleetOwnerInformation -> -- fleet info from above caller (avoid double queries)
  Maybe DI.DriverInformation -> -- driver info from above caller (avoid double queries)
  HighPrecMoney ->
  m (Maybe Finance.GstAmountBreakdown)
computeGstBreakdownForRideOwner gstBreakup fromLocation ride mbFleetInfoCached mbDriverInfoCached totalGst = do
  let (mbFleetOwnerId, driverId) = (ride.fleetOwnerId, ride.driverId)
  mbResidenceState <- case mbFleetOwnerId of
    Just fleetOwnerId -> resolveFleetOwnerAddressState fleetOwnerId mbFleetInfoCached
    Nothing -> resolveDriverAddressState driverId mbDriverInfoCached
  when (isNothing mbResidenceState) $
    logWarning $
      "GST breakdown: missing counterparty addressState for pickupLocationId="
        <> fromLocation.id.getId
        <> "; falling back to intra-state CGST/SGST"
  pure $
    computeGstBreakdownByState
      gstBreakup
      mbResidenceState
      fromLocation.address.state
      totalGst

-- | Subscription / prepaid GST: counterparty residence proof vs platform PoB (merchant operating city).
--   Not used for rides — the payer is 'personId' with an explicit 'isFleetOwner' flag.
computeGstBreakdownForPerson ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  DTC.GstBreakup ->
  DMOC.MerchantOperatingCity ->
  Id DP.Person -> -- paying person
  Bool -> -- isFleetOwner
  Maybe DFOI.FleetOwnerInformation -> -- fleet info from above caller (avoid double queries)
  Maybe DI.DriverInformation -> -- driver info from above caller (avoid double queries)
  HighPrecMoney ->
  m (Maybe Finance.GstAmountBreakdown)
computeGstBreakdownForPerson gstBreakup merchantOperatingCity personId isFleetOwner mbFleetInfoCached mbDriverInfoCached totalGst = do
  mbReceiverState <-
    if isFleetOwner
      then resolveFleetOwnerAddressState personId mbFleetInfoCached
      else resolveDriverAddressState personId mbDriverInfoCached
  when (isNothing mbReceiverState) $
    logWarning $
      "GST breakdown: missing counterparty addressState for merchantOpCityId="
        <> merchantOperatingCity.id.getId
        <> "; falling back to intra-state CGST/SGST"
  let supplierState = Just $ show merchantOperatingCity.state
  pure $
    computeGstBreakdownByState
      gstBreakup
      supplierState
      mbReceiverState
      totalGst

-- | Determine GST jurisdiction by comparing supplier vs receiver state,
--   then split the total GST.
--   Falls back to intra-state CGST/SGST when states cannot be compared.
computeGstBreakdownByState ::
  DTC.GstBreakup ->
  Maybe Text -> -- supplier state
  Maybe Text -> -- receiver state
  HighPrecMoney -> -- totalGst
  Maybe Finance.GstAmountBreakdown
computeGstBreakdownByState gstBreakup supplierState receiverState =
  Finance.computeGstBreakdownFromRates (toGstRateBreakup jurisdiction gstBreakup)
  where
    jurisdiction =
      fromMaybe Finance.IntraState $
        Finance.compareIndianState supplierState receiverState

-- | Determine GST jurisdiction by comparing the first 2 characters (state code)
--   of the seller and buyer GSTINs, then split the total GST accordingly.
--   Falls back to intra-state CGST/SGST when either GSTIN is missing
--   or too short to extract a state code.
computeGstBreakdownGSTIN ::
  DTC.GstBreakup ->
  Maybe Text -> -- seller (supplier) GSTIN
  Maybe Text -> -- buyer (receiver) GSTIN
  HighPrecMoney -> -- totalGst
  Maybe Finance.GstAmountBreakdown
computeGstBreakdownGSTIN gstBreakup sellerGstin buyerGstin =
  Finance.computeGstBreakdownFromRates (toGstRateBreakup jurisdiction gstBreakup)
  where
    jurisdiction =
      fromMaybe Finance.IntraState $
        Finance.compareIndianGstinStateCode sellerGstin buyerGstin

resolveFleetOwnerAddressState ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DP.Person ->
  Maybe DFOI.FleetOwnerInformation -> -- fleet info from above caller (avoid double queries)
  m (Maybe Text)
resolveFleetOwnerAddressState fleetOwnerId mbFleetInfoCached = do
  mbFleetInfo <- maybe (QFOI.findByPrimaryKey fleetOwnerId) (pure . Just) mbFleetInfoCached
  case mbFleetInfo of
    Nothing -> do
      logError $
        "GST breakdown: FleetOwnerInformation not found for fleetOwnerId="
          <> fleetOwnerId.getId
          <> "; expected when fleetOwnerId / isFleetOwner is set"
      pure Nothing
    Just fleetInfo -> pure $ show <$> fleetInfo.addressState

resolveDriverAddressState ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DP.Person ->
  Maybe DI.DriverInformation -> -- driver info from above caller (avoid double queries)
  m (Maybe Text)
resolveDriverAddressState driverId mbDriverInfoCached = do
  mbDriverInfo <- maybe (QDI.findById driverId) (pure . Just) mbDriverInfoCached
  case mbDriverInfo of
    Nothing -> do
      logError $
        "GST breakdown: DriverInformation not found for driverId="
          <> driverId.getId
          <> "; expected for active driver"
      pure Nothing
    Just driverInfo -> do
      mbDriverIdentityInfo <- QDII.findByDriverId driverId
      let identityInfo = DIInfo.getIdentityInfo mbDriverIdentityInfo driverInfo
      pure $ show <$> identityInfo.addressState

toGstRateBreakup :: Finance.GstJurisdiction -> DTC.GstBreakup -> Finance.GstRateBreakup
toGstRateBreakup Finance.IntraState gstBreakup =
  Finance.IntraStateBreakup $
    Finance.GstRateIntraStateBreakup
      { cgstRate = gstBreakup.cgstPercentage,
        sgstRate = gstBreakup.sgstPercentage
      }
toGstRateBreakup Finance.InterState gstBreakup =
  Finance.InterStateBreakup $
    Finance.GstRateInterStateBreakup
      { igstRate = gstBreakup.igstPercentage <|> ((+) <$> gstBreakup.cgstPercentage <*> gstBreakup.sgstPercentage)
      }
