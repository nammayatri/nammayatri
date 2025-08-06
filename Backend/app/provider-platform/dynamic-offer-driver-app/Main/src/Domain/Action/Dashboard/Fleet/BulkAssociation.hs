{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Domain.Action.Dashboard.Fleet.BulkAssociation
  ( BulkFleetAssociationAPI,
    BulkFleetAssociationRow (..),
    BulkFleetAssociationResult (..),
    bulkFleetAssociationHandler,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.Endpoints.Driver as API
import qualified Dashboard.Common ()
import Data.Aeson ()
import qualified Data.Csv as Csv
import Data.Either (partitionEithers)
import Data.OpenApi ()
import Data.Text (splitOn)
import qualified Data.Text as T
import qualified Domain.Action.Dashboard.Fleet.Driver as FleetDriver
import qualified Domain.Action.UI.Registration as DReg
import qualified Domain.Types.FleetBadgeType as DFBT
import qualified Domain.Types.FleetRouteAssociation as FleetRouteAssociation
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Route as DRoute
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleRouteMapping as DVRM
import Environment (Flow)
import EulerHS.Prelude (toList)
import Kernel.Beam.Functions (findAllWithOptionsKV)
import Kernel.External.Encryption (encrypt, getDbHash)
import Kernel.Prelude hiding (toList)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error (GenericError (..), PersonError (PersonNotFound))
import Kernel.Types.Id (Id (..), ShortId)
import Kernel.Utils.Common (fromMaybeM, generateGUID, getCurrentTime, logTagInfo)
import qualified Sequelize as Se
import Servant
import Servant.Multipart
import SharedLogic.Merchant (findMerchantByShortId)
import SharedLogic.WMB (linkFleetBadge, validateBadgeAssignment)
import qualified Storage.Beam.FleetDriverAssociation as BeamFDVA
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.FleetDriverAssociation as QFDV
import qualified Storage.Queries.FleetRouteAssociation as QFRA
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Route as QRoute
import qualified Storage.Queries.VehicleRouteMapping as QVRM

-- Driver details for bulk association
data DriverDetails = DriverDetails
  { driverName :: Text,
    driverPhoneNumber :: Text,
    driverOnboardingVehicleCategory :: Maybe DVC.VehicleCategory,
    fleetPhoneNo :: Maybe Text
  }

-- Get or create a driver record
fetchOrCreatePerson :: DMOC.MerchantOperatingCity -> DriverDetails -> Flow (DP.Person, Bool)
fetchOrCreatePerson moc req_ = do
  let authData =
        DReg.AuthReq
          { mobileNumber = Just $ driverPhoneNumber req_,
            mobileCountryCode = Just "+91",
            merchantId = moc.merchantId.getId,
            merchantOperatingCity = Just moc.city,
            email = Nothing,
            name = Just $ driverName req_,
            identifierType = Just DP.DEVICEID,
            registrationLat = Nothing,
            registrationLon = Nothing
          }
  mobileHash <- getDbHash (driverPhoneNumber req_)
  mb <- QP.findByMobileNumberAndMerchantAndRole "+91" mobileHash moc.merchantId DP.DRIVER
  case mb of
    Nothing -> do
      person <- DReg.createDriverWithDetails authData Nothing Nothing Nothing Nothing Nothing moc.merchantId moc.id True
      pure (person, True)
    Just person -> do
      QP.updateName (driverName req_) person.id
      pure (person {DP.firstName = driverName req_}, False)

-- CSV row type
data BulkFleetAssociationRow = BulkFleetAssociationRow
  { fleet_owner_mobile_number :: Text,
    device_identifier :: Text,
    device_identifier_type :: Text,
    vehicle_number :: Text,
    route_codes :: Text,
    driver_badge_name :: Maybe Text,
    conductor_badge_name :: Maybe Text,
    driver_badge_identifier :: Maybe Text,
    driver_identifier_type :: Maybe Text,
    conductor_badge_identifier :: Maybe Text,
    conductor_identifier_type :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance Csv.FromNamedRecord BulkFleetAssociationRow

instance Csv.DefaultOrdered BulkFleetAssociationRow

-- Per-row result type
data BulkFleetAssociationResult = BulkFleetAssociationResult
  { rowNumber :: Int,
    fleetOwnerId :: Maybe Text,
    vehicleId :: Maybe Text,
    driverId :: Maybe Text,
    routeAssociationId :: Maybe Text,
    badgeAssociationId :: Maybe Text,
    status :: Text,
    errorMsg :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

type BulkFleetAssociationAPI =
  "bulk-associate"
    :> MultipartForm Mem (MultipartData Mem)
    :> Post '[JSON] [BulkFleetAssociationResult]

bulkFleetAssociationHandler :: ShortId DM.Merchant -> Context.City -> MultipartData Mem -> Flow [BulkFleetAssociationResult]
bulkFleetAssociationHandler merchantShortId opCity multipartData = do
  let files' = files multipartData
  case files' of
    (file : _) -> do
      let csvData = fdPayload file
      case Csv.decodeByName csvData of
        Left err -> pure [BulkFleetAssociationResult 0 Nothing Nothing Nothing Nothing Nothing "failure" (Just $ "CSV parse error: " <> toText err)]
        Right (_, rows) -> do
          results <- forM (zip [1 ..] (toList rows)) $ \(i, row) -> handleBulkRow merchantShortId opCity i row
          pure results
    [] -> pure [BulkFleetAssociationResult 0 Nothing Nothing Nothing Nothing Nothing "failure" (Just "No file uploaded")]

-- Helper to build AddVehicleReq from CSV row
buildAddVehicleReqFromRow :: BulkFleetAssociationRow -> API.AddVehicleReq
buildAddVehicleReqFromRow row =
  API.AddVehicleReq
    { registrationNo = row.vehicle_number,
      vehicleTags = Nothing,
      make = "",
      model = "",
      vehicleClass = "",
      mYManufacturing = Nothing,
      fuelType = Nothing,
      capacity = Nothing,
      airConditioned = Nothing,
      colour = "",
      dateOfRegistration = Nothing,
      vehicleModelYear = Nothing,
      energyType = Nothing,
      driverName = Nothing,
      imageId = Nothing,
      vehicleCategory = Nothing,
      oxygen = Nothing,
      ventilator = Nothing
    }

handleBulkRow :: ShortId DM.Merchant -> Context.City -> Int -> BulkFleetAssociationRow -> Flow BulkFleetAssociationResult
handleBulkRow merchantShortId opCity i row = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.findByMerchantIdAndCity (merchant.id) opCity >>= fromMaybeM (InternalError "Merchant operating city not found")
  let phoneNo = row.fleet_owner_mobile_number
      mbCountryCode = Nothing
      mbRole = Nothing

  let mobileCountryCode = fromMaybe "+91" mbCountryCode
  phoneNumberHash <- getDbHash phoneNo
  let role = DP.FLEET_OWNER -- Always use FLEET_OWNER for bulk fleet association

  -- Only proceed if fleet owner exists
  entityDetails <- QP.findByMobileNumberAndMerchantAndRole mobileCountryCode phoneNumberHash merchant.id role >>= fromMaybeM (PersonNotFound phoneNo)

  -- Set the fleet owner ID for the vehicle addition
  let mbFleetOwnerId = Just entityDetails.id.getId
      requestorId = Just $ getId entityDetails.id -- Fleet owner is the requestor

  -- Use device identifier as driver phone number
  let driverPhoneNo = row.device_identifier

  -- Check if driver exists and get their details
  driverPhoneNumberHash <- getDbHash driverPhoneNo
  mbDriver <- QP.findByMobileNumberAndMerchantAndRole mobileCountryCode driverPhoneNumberHash merchant.id DP.DRIVER

  case mbDriver of
    Just driver -> do
      -- Check fleet-driver association
      let driverId = driver.id
          fleetOwnerId = entityDetails.id.getId
      logTagInfo "BulkAssociation" $ "Looking for association with: driverId=" <> show driverId <> ", fleetOwnerId=" <> show fleetOwnerId
      mbAssoc <- QFDV.findByDriverIdAndFleetOwnerId driverId fleetOwnerId True
      logTagInfo "BulkAssociation" $ "Driver found: " <> show driver.id <> ", Fleet association: " <> show (isJust mbAssoc)
      case mbAssoc of
        Just assoc -> logTagInfo "BulkAssociation" $ "Association details: isActive=" <> show assoc.isActive <> ", fleetOwnerId=" <> show assoc.fleetOwnerId <> ", associatedTill=" <> show assoc.associatedTill
        Nothing -> do
          logTagInfo "BulkAssociation" $ "No active association found"
          -- Create a new fleet-driver association defaulting to BUS through 2099
          QFDV.createFleetDriverAssociationIfNotExists driverId entityDetails.id Nothing DVC.BUS True
          logTagInfo "BulkAssociation" $ "Created fleet-driver association for driver: " <> show driverId <> ", fleetOwnerId: " <> show fleetOwnerId
          -- Try to find any association (without time condition) to debug
          now <- getCurrentTime
          logTagInfo "BulkAssociation" $ "Current time: " <> show now
          -- Let's check what associations exist without the time condition
          logTagInfo "BulkAssociation" $ "Checking for any association with driverId=" <> show driverId <> ", fleetOwnerId=" <> show fleetOwnerId
          -- Try to find association without the time condition by using a different approach
          let assocQuery = [Se.And [Se.Is BeamFDVA.driverId $ Se.Eq (driverId.getId), Se.Is BeamFDVA.fleetOwnerId $ Se.Eq fleetOwnerId, Se.Is BeamFDVA.isActive $ Se.Eq True]]
          allAssocs <- findAllWithOptionsKV assocQuery (Se.Desc BeamFDVA.createdAt) Nothing Nothing
          logTagInfo "BulkAssociation" $ "All associations found (without time condition): " <> show (length allAssocs)
          forM_ allAssocs $ \assoc -> do
            logTagInfo "BulkAssociation" $ "Association: id=" <> show assoc.id <> ", isActive=" <> show assoc.isActive <> ", associatedTill=" <> show assoc.associatedTill
    Nothing -> do
      logTagInfo "BulkAssociation" $ "Driver not found with phone: " <> driverPhoneNo
      -- Create the driver record
      let driverName = "Driver " <> driverPhoneNo
          driverDetails = DriverDetails driverName driverPhoneNo (Just DVC.BUS) Nothing
      (person, _) <- fetchOrCreatePerson merchantOpCityId driverDetails
      let driverId = person.id
      -- Create fleet-driver association
      QFDV.createFleetDriverAssociationIfNotExists driverId entityDetails.id Nothing DVC.BUS True

  -- Add vehicle using postDriverFleetAddVehicle (this will create driver if needed)
  logTagInfo "BulkAssociation" $ "Adding vehicle with driver: " <> driverPhoneNo
  vehicleResult <-
    try @_ @SomeException $
      FleetDriver.postDriverFleetAddVehicle
        merchantShortId
        opCity
        driverPhoneNo
        (fromMaybe "" requestorId)
        mbFleetOwnerId
        mbCountryCode
        mbRole
        (Just True)
        (buildAddVehicleReqFromRow row)

  case vehicleResult of
    Left err -> pure $ BulkFleetAssociationResult i Nothing Nothing Nothing Nothing Nothing "failure" (Just $ show err)
    Right _ -> do
      let routeCodes = splitOn "," row.route_codes
      routeResults <- fmap partitionEithers $
        forM routeCodes $ \routeCode -> do
          routeM <- QRoute.findByRouteCode routeCode
          case routeM of
            Nothing -> pure $ Left $ "Route with code " <> routeCode <> " not found"
            Just routeInfo -> do
              now <- getCurrentTime
              assocId <- generateGUID
              let assoc =
                    FleetRouteAssociation.FleetRouteAssociation
                      { id = assocId,
                        fleetOwnerId = entityDetails.id,
                        routeCode = routeCode,
                        merchantId = merchant.id,
                        merchantOperatingCityId = merchantOpCityId.id,
                        createdAt = now,
                        updatedAt = now
                      }
              res' <- try @_ @SomeException $ QFRA.create assoc
              case res' of
                Left err -> do
                  logTagInfo "BulkAssociation" $ "Failed to create fleet route association: " <> show err
                  pure $ Left $ "Failed to create fleet route association"
                Right _ -> do
                  buildVehicleRouteMapping entityDetails.id merchant.id merchantOpCityId.id routeCode routeInfo row.vehicle_number
                  pure $ Right routeCode
      let (routeErrors, successfulRoutes) = routeResults
      if null routeErrors
        then do
          when (isJust row.driver_badge_name) $ do
            driverBadge <- validateBadgeAssignment entityDetails.id merchant.id merchantOpCityId.id entityDetails.id.getId (fromJust row.driver_badge_name) DFBT.DRIVER
            linkFleetBadge entityDetails.id merchant.id merchantOpCityId.id entityDetails.id.getId driverBadge DFBT.DRIVER
          when (isJust row.conductor_badge_name) $ do
            conductorBadge <- validateBadgeAssignment entityDetails.id merchant.id merchantOpCityId.id entityDetails.id.getId (fromJust row.conductor_badge_name) DFBT.CONDUCTOR
            linkFleetBadge entityDetails.id merchant.id merchantOpCityId.id entityDetails.id.getId conductorBadge DFBT.CONDUCTOR
          -- To check if driver or merchant has to be associated with the fleet, for fleet badges
          pure $ BulkFleetAssociationResult i (Just $ entityDetails.id.getId) (Just $ T.intercalate "," successfulRoutes) (Just $ show entityDetails.id) Nothing Nothing "success" Nothing
        else pure $ BulkFleetAssociationResult i (Just $ entityDetails.id.getId) Nothing (Just $ show entityDetails.id) Nothing Nothing "failure" (Just $ T.intercalate ", " routeErrors)

buildVehicleRouteMapping :: Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> DRoute.Route -> Text -> Flow ()
buildVehicleRouteMapping fleetOwnerId merchantId merchantOpCityId routeCode _ vehicleNumber = do
  now <- getCurrentTime
  vehicleNumberHash <- getDbHash vehicleNumber
  encryptedVehicleNumber <- encrypt vehicleNumber
  let mapping =
        DVRM.VehicleRouteMapping
          { blocked = False,
            routeCode = routeCode,
            fleetOwnerId = fleetOwnerId,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOpCityId,
            createdAt = now,
            updatedAt = now,
            vehicleNumber = encryptedVehicleNumber
          }
  res' <- try @_ @SomeException $ QVRM.upsert mapping vehicleNumberHash
  case res' of
    Left err -> logTagInfo "BulkAssociation" $ "Failed to create vehicle route mapping: " <> show err
    Right _ -> pure ()
