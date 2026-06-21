{-# OPTIONS_GHC -Wno-unused-imports #-}

-- Growth › Vehicle Branding — business-logic handler (plan v6 §7 B4/B5/B6).
--
-- UNVERIFIED DRAFT. Depends on generated modules that only exist after `, run-generator`
-- (API.Types.UI.VehicleBranding, Domain.Types.VehicleBranding, Storage.Queries.VehicleBranding).
-- It CANNOT be compiled here (no nix/cabal). Treat as a faithful skeleton of the logic:
-- generate the specs, then `cabal build dynamic-offer-driver-app` and resolve signature/import
-- drift before relying on it.
--
-- Two reuse points to confirm on first build:
--   1. Proof upload reuses Domain.Action.UI.DriverOnboarding.Image helpers (createPath / mkImage).
--      Those are currently module-local — export them, or extract to SharedLogic, then import here.
--   2. Driver-by-mobile resolution (D9) is left as a TODO; capturedById = the token principal,
--      driverMobile is always persisted. Wire the lookup when finalised.

module Domain.Action.UI.VehicleBranding
  ( postVehicleBrandingCreate,
    getVehicleBrandingList,
    postVehicleBrandingUpdateStatus,
    getVehicleBrandingMetrics,
  )
where

import qualified API.Types.UI.VehicleBranding as API
import AWS.S3 as S3
import qualified Data.List as List
import qualified Data.Text as T
import qualified Domain.Action.UI.DriverOnboarding.Image as Image
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.Image as DImage
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleBranding as DVB
import qualified Environment
import EulerHS.Prelude hiding (id)
import EulerHS.Types (base64Decode)
import qualified Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Documents (VerificationStatus (..))
import Kernel.Types.Error
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common (fromMaybeM, generateGUID, getCurrentTime, throwError)
import Kernel.Utils.Logging (logError, logInfo)
import qualified Storage.Queries.Image as QImage
import qualified Storage.Queries.VehicleBranding as QVB
import qualified Storage.Queries.VehicleBrandingExtra as QVBE
import qualified Storage.Queries.VehicleRegistrationCertificate as QRC

type AuthInfo =
  ( Kernel.Prelude.Maybe (Id.Id DP.Person),
    Id.Id DM.Merchant,
    Id.Id DMOC.MerchantOperatingCity
  )

normaliseReg :: Text -> Text
normaliseReg = T.toUpper . T.filter (/= ' ')

------------------------------------------------------------------------------------------------
-- Create (B4/B5): resolve principal + RC, upload proof, dedupe, insert PENDING row.
------------------------------------------------------------------------------------------------
postVehicleBrandingCreate :: AuthInfo -> API.VehicleBrandingCreateReq -> Environment.Flow API.VehicleBrandingCreateRes
postVehicleBrandingCreate (mbPersonId, merchantId, merchantOpCityId) req = do
  capturedById <- fromMaybeM (InvalidRequest "Missing principal for vehicle branding capture") mbPersonId
  let registrationNo = normaliseReg req.registrationNo
  when (registrationNo == "") $ throwError (InvalidRequest "registrationNo is required")

  -- size-validate base64 (mirrors Image.hs)
  _ <- fromMaybeM (InvalidRequest "Failed to decode base64 image") $ base64Decode req.image

  -- resolve RC (vehicle_unmatched flag if absent)
  mbRc <- QRC.findLastVehicleRCWrapper registrationNo
  let mbRcId = (.id) <$> mbRc

  -- upload proof through the reused image->S3 pipeline (imageType = VehicleBranding)
  -- NOTE: production should mirror Image.hs exactly (Redis lock + withTryCatch around S3.put).
  imagePath <- Image.createPath capturedById.getId merchantId.getId DVC.VehicleBranding req.fileExtension
  _ <- S3.put (T.unpack imagePath) req.image
  imageEntity <- Image.mkImage capturedById merchantId (Just merchantOpCityId) imagePath DVC.VehicleBranding ((.getId) <$> mbRcId) Nothing Nothing Nothing
  QImage.create imageEntity

  -- dedupe: existing VALID branding for same RC + type
  existing <- maybe (pure []) (\rcId -> QVB.findByRcIdAndBrandingType (Just rcId) req.brandingType) mbRcId
  let isDuplicate = any (\r -> r.verificationStatus == VALID) existing
      isVehicleUnmatched = isNothing mbRcId
      isGpsMissing = isNothing req.lat || isNothing req.lon

  id <- Id.Id <$> generateGUID
  now <- getCurrentTime
  let row =
        DVB.VehicleBranding
          { id,
            registrationNo,
            rcId = mbRcId,
            driverId = Nothing, -- TODO(D9): resolve driver by req.driverMobile / RC
            driverMobile = req.driverMobile,
            capturedById,
            category = req.category,
            brandingType = req.brandingType,
            brandingSize = req.brandingSize,
            vehicleRating = req.vehicleRating,
            imageId = Just imageEntity.id,
            lat = req.lat,
            lon = req.lon,
            gpsAccuracyM = req.gpsAccuracyM,
            notes = req.notes,
            isGpsMissing = Just isGpsMissing,
            isVehicleUnmatched = Just isVehicleUnmatched,
            isDuplicate = Just isDuplicate,
            verificationStatus = PENDING,
            verifiedById = Nothing,
            rejectReason = Nothing,
            merchantId,
            merchantOperatingCityId = merchantOpCityId,
            createdAt = now,
            updatedAt = now
          }
  QVB.create row
  logInfo $ "Created VehicleBranding " <> id.getId <> " for " <> registrationNo
  pure $
    API.VehicleBrandingCreateRes
      { id,
        verificationStatus = PENDING,
        flags = buildFlags isGpsMissing isVehicleUnmatched isDuplicate
      }

buildFlags :: Bool -> Bool -> Bool -> [Text]
buildFlags gps unmatched dup =
  catMaybes
    [ if gps then Just "gps_missing" else Nothing,
      if unmatched then Just "vehicle_unmatched" else Nothing,
      if dup then Just "duplicate" else Nothing
    ]

------------------------------------------------------------------------------------------------
-- List (B6)
------------------------------------------------------------------------------------------------
getVehicleBrandingList ::
  AuthInfo ->
  Kernel.Prelude.Maybe VerificationStatus ->
  Kernel.Prelude.Maybe DVB.BrandingCategory ->
  Kernel.Prelude.Maybe DVB.BrandingType ->
  Kernel.Prelude.Maybe Text ->
  Kernel.Prelude.Maybe UTCTime ->
  Kernel.Prelude.Maybe UTCTime ->
  Kernel.Prelude.Maybe Int ->
  Kernel.Prelude.Maybe Int ->
  Environment.Flow API.VehicleBrandingListRes
getVehicleBrandingList (_, merchantId, merchantOpCityId) mbStatus mbCategory mbType mbReg mbFrom mbTo mbLimit mbOffset = do
  let flt =
        (QVBE.emptyFilter merchantId merchantOpCityId)
          { QVBE.verificationStatus = mbStatus,
            QVBE.category = mbCategory,
            QVBE.brandingType = mbType,
            QVBE.registrationNo = normaliseReg <$> mbReg,
            QVBE.from = mbFrom,
            QVBE.to = mbTo,
            QVBE.limit = mbLimit Kernel.Prelude.<|> Just 50,
            QVBE.offset = mbOffset
          }
  rows <- QVBE.findAllForVehicleBranding flt
  pure $ API.VehicleBrandingListRes {records = map toRecord rows, totalCount = length rows}

toRecord :: DVB.VehicleBranding -> API.VehicleBrandingRecord
toRecord r =
  API.VehicleBrandingRecord
    { id = r.id,
      registrationNo = r.registrationNo,
      rcId = (.getId) <$> r.rcId,
      driverMobile = r.driverMobile,
      category = r.category,
      brandingType = r.brandingType,
      brandingSize = r.brandingSize,
      vehicleRating = r.vehicleRating,
      imageId = r.imageId,
      lat = r.lat,
      lon = r.lon,
      gpsAccuracyM = r.gpsAccuracyM,
      notes = r.notes,
      flags = buildFlags (fromMaybe False r.isGpsMissing) (fromMaybe False r.isVehicleUnmatched) (fromMaybe False r.isDuplicate),
      verificationStatus = r.verificationStatus,
      rejectReason = r.rejectReason,
      createdAt = r.createdAt
    }

------------------------------------------------------------------------------------------------
-- Approve / Reject (B4 / D11)
------------------------------------------------------------------------------------------------
postVehicleBrandingUpdateStatus :: AuthInfo -> Id.Id DVB.VehicleBranding -> API.UpdateBrandingStatusReq -> Environment.Flow APISuccess
postVehicleBrandingUpdateStatus (mbPersonId, _, _) brandingId reqBody = do
  _ <- QVB.findById brandingId >>= fromMaybeM (InvalidRequest "Vehicle branding record not found")
  now <- getCurrentTime
  case reqBody of
    API.Approve -> QVB.updateVerificationStatus VALID mbPersonId Nothing now brandingId
    API.Reject info -> QVB.updateVerificationStatus INVALID mbPersonId (Just info.reason) now brandingId
  pure Success

------------------------------------------------------------------------------------------------
-- Metrics (B6) — aggregated in Haskell over the chosen window (v1 volumes are small).
------------------------------------------------------------------------------------------------
getVehicleBrandingMetrics ::
  AuthInfo ->
  Kernel.Prelude.Maybe UTCTime ->
  Kernel.Prelude.Maybe UTCTime ->
  Kernel.Prelude.Maybe DVB.BrandingCategory ->
  Kernel.Prelude.Maybe DVB.BrandingType ->
  Environment.Flow API.VehicleBrandingMetricsRes
getVehicleBrandingMetrics (_, merchantId, merchantOpCityId) mbFrom mbTo mbCategory mbType = do
  let base = (QVBE.emptyFilter merchantId merchantOpCityId) {QVBE.category = mbCategory, QVBE.brandingType = mbType}
  windowRows <- QVBE.findAllForVehicleBranding base {QVBE.from = mbFrom, QVBE.to = mbTo}
  allValid <- QVBE.findAllForVehicleBranding base {QVBE.verificationStatus = Just VALID}
  let cumulativeVerifiedVehicles = length . List.nub $ map (.registrationNo) allValid
      countWith p = length (filter p windowRows)
  pure $
    API.VehicleBrandingMetricsRes
      { cumulativeVerifiedVehicles,
        branded30d = length windowRows,
        verified30d = countWith (\r -> r.verificationStatus == VALID),
        pending = countWith (\r -> r.verificationStatus == PENDING),
        rejected30d = countWith (\r -> r.verificationStatus == INVALID),
        byCategory = tally (show . (.category)) windowRows,
        byBrandingType = tally (show . (.brandingType)) windowRows,
        byCity = [] -- single-op-city scope here; populate when aggregating across cities
      }

tally :: (DVB.VehicleBranding -> Text) -> [DVB.VehicleBranding] -> [API.BrandingBreakdownRow]
tally keyOf rows =
  map (\grp -> API.BrandingBreakdownRow {key = keyOf (List.head grp), count = length grp})
    . List.groupBy (\a b -> keyOf a == keyOf b)
    . List.sortOn keyOf
    $ rows
