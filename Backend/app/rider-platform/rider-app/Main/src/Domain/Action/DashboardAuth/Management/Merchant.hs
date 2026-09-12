{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Hand-written handlers for the direct-dashboard merchant/operating-city
-- creation routes.
--
-- These take a KML file as multipart upload, which provider-dashboard converted
-- to a PostGIS geometry before forwarding the call, alongside its own
-- dashboard-database bookkeeping (a new merchant row, and the merchant's
-- supported city list). The generated @API.Action.DashboardAuth@ handler serves
-- the multipart request and calls these functions, so the browser keeps its
-- existing contract instead of being asked for a pre-computed geometry.
--
-- Mirrors @processMerchantCreateRequest@ in provider-dashboard's
-- @Domain.Action.{PLATFORM}.Management.Merchant@. The city STD-code mapping it
-- also wrote is left to the domain handler, which validates and appends the same
-- mapping itself.
module Domain.Action.DashboardAuth.Management.Merchant
  ( postMerchantConfigMerchantCreate,
    postMerchantConfigOperatingCityCreate,
    postMerchantSpecialLocationUpsert,
    postMerchantSpecialLocationGatesUpsert,
  )
where

import qualified Dashboard.Common.Merchant
import qualified Data.Text as T
import qualified Domain.Action.Dashboard.Merchant
import qualified Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.Merchant as DDashboardMerchant
import qualified Environment
import Kernel.Beam.Functions (runInDashboardDb)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Geometry (getGeomFromKML)
import qualified Lib.GateInfo.Geometry as GGeom
import qualified Lib.Types.SpecialLocation
import "lib-dashboard" Storage.Beam.SchemaInstances ()
import qualified "lib-dashboard" Storage.Queries.Merchant as QDashboardMerchant
import Tools.Auth.DashboardUserAuth

-- | @\/config\/merchant\/create@: may also create the dashboard merchant row.
postMerchantConfigMerchantCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.CreateMerchantOperatingCityReq -> Environment.Flow Dashboard.Common.Merchant.CreateMerchantOperatingCityRes)
postMerchantConfigMerchantCreate a4 a3 _a2 a1 = processMerchantCreateRequest a4 a3 True a1

-- | @\/config\/operatingCity\/create@: a city for an existing merchant only.
postMerchantConfigOperatingCityCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Merchant.CreateMerchantOperatingCityReq -> Environment.Flow Dashboard.Common.Merchant.CreateMerchantOperatingCityRes)
postMerchantConfigOperatingCityCreate a4 a3 _a2 a1 = processMerchantCreateRequest a4 a3 False a1

processMerchantCreateRequest ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Bool ->
  Dashboard.Common.Merchant.CreateMerchantOperatingCityReq ->
  Environment.Flow Dashboard.Common.Merchant.CreateMerchantOperatingCityRes
processMerchantCreateRequest merchantShortId opCity canCreateMerchant req@Dashboard.Common.Merchant.CreateMerchantOperatingCityReq {..} = do
  let dashboardMerchantShortId = Kernel.Types.Id.ShortId merchantShortId.getShortId
  baseMerchant <-
    runInDashboardDb (QDashboardMerchant.findByShortId dashboardMerchantShortId)
      >>= fromMaybeM (InvalidRequest $ "Merchant not found with shortId " <> merchantShortId.getShortId)
  geom <- getGeomFromKML req.file >>= fromMaybeM (InvalidRequest "Cannot convert KML to Geom.")
  geomGeoJson <- GGeom.getGeoJsonFromKML req.file >>= fromMaybeM (InvalidRequest "Cannot convert KML to GeoJSON.")
  now <- getCurrentTime
  -- Resolve the target merchant WITHOUT persisting it yet, and reject illegal city/create usage upfront.
  -- mbNewMerchant is Just only when a new dashboard merchant row still needs to be created.
  (merchant, mbNewMerchant) <-
    case (merchantData, canCreateMerchant) of
      (Just merchantD, True) ->
        runInDashboardDb (QDashboardMerchant.findByShortId (Kernel.Types.Id.ShortId merchantD.shortId)) >>= \case
          Nothing -> do
            let newMerchant = buildMerchant now merchantD baseMerchant
            return (newMerchant, Just newMerchant)
          Just existingMerchant -> return (existingMerchant, Nothing)
      (Just merchantD, False) -> throwError (InvalidRequest $ "Merchant Cannot be created using city/create: " <> merchantD.shortId)
      (Nothing, _) -> return (baseMerchant, Nothing)
  -- Create the operating city first: it validates the exophone before any write, so a duplicate
  -- exophone blocks the dashboard merchant creation below instead of leaving an orphaned row.
  res <-
    Domain.Action.Dashboard.Merchant.postMerchantConfigOperatingCityCreate
      merchantShortId
      opCity
      Dashboard.Common.Merchant.CreateMerchantOperatingCityReqT {geom = T.pack geom, geomGeoJson = geomGeoJson, ..}
  runInDashboardDb $ do
    whenJust mbNewMerchant QDashboardMerchant.create
    unless (req.city `elem` merchant.supportedOperatingCities) $
      QDashboardMerchant.updateSupportedOperatingCities merchant.shortId (merchant.supportedOperatingCities <> [req.city])
  pure res
  where
    buildMerchant now merchantD baseMerchant =
      DDashboardMerchant.Merchant
        { id = Kernel.Types.Id.Id merchantD.subscriberId,
          shortId = Kernel.Types.Id.ShortId merchantD.shortId,
          defaultOperatingCity = req.city,
          supportedOperatingCities = [req.city],
          serverNames = baseMerchant.serverNames,
          domain = baseMerchant.domain,
          website = baseMerchant.website,
          authToken = baseMerchant.authToken,
          createdAt = now,
          enabled = Just req.enableForMerchant,
          requireAdminApprovalForFleetOnboarding = baseMerchant.requireAdminApprovalForFleetOnboarding,
          verifyFleetWhileLogin = baseMerchant.verifyFleetWhileLogin,
          hasFleetMemberHierarchy = baseMerchant.hasFleetMemberHierarchy,
          isStrongNameCheckRequired = baseMerchant.isStrongNameCheckRequired,
          singleActiveSessionOnly = baseMerchant.singleActiveSessionOnly,
          trackLoginLogoutForRoles = baseMerchant.trackLoginLogoutForRoles,
          adminEmailDomains = baseMerchant.adminEmailDomains
        }

-- | @\/specialLocation\/upsert@: the KML file is optional here; only convert one
-- when the caller sent it, exactly as provider-dashboard's @mkGeom@ did.
postMerchantSpecialLocationUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation) -> Dashboard.Common.Merchant.UpsertSpecialLocationReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantSpecialLocationUpsert a5 a4 _a3 a2 req@Dashboard.Common.Merchant.UpsertSpecialLocationReq {..} = do
  geom <- maybe (pure Nothing) mkGeom req.file
  Domain.Action.Dashboard.Merchant.postMerchantSpecialLocationUpsert a5 a4 a2 Dashboard.Common.Merchant.UpsertSpecialLocationReqT {geom = geom, ..}

-- | @\/specialLocation\/{specialLocationId}\/gates\/upsert@: same optional KML.
postMerchantSpecialLocationGatesUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Dashboard.Common.Merchant.UpsertSpecialLocationGateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantSpecialLocationGatesUpsert a5 a4 _a3 a2 req@Dashboard.Common.Merchant.UpsertSpecialLocationGateReq {..} = do
  geom <- maybe (pure Nothing) mkGeom req.file
  Domain.Action.Dashboard.Merchant.postMerchantSpecialLocationGatesUpsert a5 a4 a2 Dashboard.Common.Merchant.UpsertSpecialLocationGateReqT {geom = geom, ..}

mkGeom :: FilePath -> Environment.Flow (Kernel.Prelude.Maybe Kernel.Prelude.Text)
mkGeom kmlFile = do
  result <- getGeomFromKML kmlFile >>= fromMaybeM (InvalidRequest "Cannot convert KML to Geom.")
  pure $ Just $ T.pack result
