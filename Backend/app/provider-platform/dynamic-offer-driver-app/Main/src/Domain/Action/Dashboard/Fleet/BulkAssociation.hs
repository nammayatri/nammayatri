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
import qualified Data.ByteString.Lazy as LBS
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
import Kernel.External.Encryption (encrypt, getDbHash)
import Kernel.Prelude hiding (toList)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error (GenericError (..), PersonError (PersonNotFound))
import Kernel.Types.Id (Id (..), ShortId)
import Kernel.Utils.Common (fromMaybeM, generateGUID, getCurrentTime, logTagError, logTagInfo, logTagWarning)
import Servant
import Servant.Multipart
import SharedLogic.Merchant (findMerchantByShortId)
import SharedLogic.WMB (linkFleetBadge', validateBadgeAssignment)
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
fetchOrCreatePerson :: DMOC.MerchantOperatingCity -> DriverDetails -> Text -> Flow (DP.Person, Bool)
fetchOrCreatePerson moc req_ identifierType = do
  let authData =
        DReg.AuthReq
          { mobileNumber = Just $ driverPhoneNumber req_,
            mobileCountryCode = Just "+91",
            merchantId = moc.merchantId.getId,
            merchantOperatingCity = Just moc.city,
            email = Nothing,
            name = Just $ driverName req_,
            identifierType = case identifierType of
              "DEVICEID" -> Just DP.DEVICEID
              "MOBILENUMBER" -> Just DP.MOBILENUMBER
              _ -> Just DP.DEVICEID, -- fallback
            registrationLat = Nothing,
            registrationLon = Nothing
          }

  -- Find existing person based on identifier type
  mb <- case identifierType of
    "MOBILENUMBER" -> do
      mobileHash <- getDbHash (driverPhoneNumber req_)
      QP.findByMobileNumberAndMerchantAndRole "+91" mobileHash moc.merchantId DP.DRIVER
    "DEVICEID" -> do
      -- For DEVICEID, treat the identifier as a phone number for person lookup
      mobileHash <- getDbHash (driverPhoneNumber req_)
      QP.findByMobileNumberAndMerchantAndRole "+91" mobileHash moc.merchantId DP.DRIVER
    _ -> do
      logTagWarning "BulkAssociation" $ "Unknown identifier type: " <> identifierType <> ", treating as phone number"
      mobileHash <- getDbHash (driverPhoneNumber req_)
      QP.findByMobileNumberAndMerchantAndRole "+91" mobileHash moc.merchantId DP.DRIVER

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

-- Validate CSV row data
validateRow :: BulkFleetAssociationRow -> Either Text ()
validateRow row = do
  when (T.null row.fleet_owner_mobile_number) $ Left "Fleet owner mobile number is required"
  when (T.null row.device_identifier) $ Left "Device identifier is required"
  when (T.null row.vehicle_number) $ Left "Vehicle number is required"
  when (T.null row.route_codes) $ Left "Route codes are required"
  when (T.null row.device_identifier_type) $ Left "Device identifier type is required"
  pure ()

bulkFleetAssociationHandler :: ShortId DM.Merchant -> Context.City -> MultipartData Mem -> Flow [BulkFleetAssociationResult]
bulkFleetAssociationHandler merchantShortId opCity multipartData = do
  logTagInfo "BulkAssociation" $ "Starting bulk association for merchant: " <> show merchantShortId <> ", city: " <> show opCity

  let files' = files multipartData
  case files' of
    (file : _) -> do
      let csvData = fdPayload file
          fileName = fdFileName file

      logTagInfo "BulkAssociation" $ "Processing file: " <> show fileName <> " with size: " <> show (LBS.length csvData) <> " bytes"

      case Csv.decodeByName csvData of
        Left err -> do
          logTagError "BulkAssociation" $ "CSV parse error: " <> show err
          pure [BulkFleetAssociationResult 0 Nothing Nothing Nothing Nothing Nothing "failure" (Just $ "CSV parse error: " <> toText err)]
        Right (_, rows) -> do
          logTagInfo "BulkAssociation" $ "Successfully parsed " <> show (length rows) <> " rows from CSV"

          -- Validate all rows first
          let validationResults =
                zip [1 ..] (toList rows) <&> \(i, row) ->
                  case validateRow row of
                    Left err -> Left (i, err)
                    Right _ -> Right (i, row)

          let (invalidRows, validRows) = partitionEithers validationResults

          when (not (null invalidRows)) $ do
            logTagWarning "BulkAssociation" $ "Found " <> show (length invalidRows) <> " invalid rows: " <> show invalidRows

          -- Process valid rows
          validResults <- forM validRows $ \(i, row) -> handleBulkRow merchantShortId opCity i row

          -- Create error results for invalid rows
          let invalidResults =
                invalidRows <&> \(i, err) ->
                  BulkFleetAssociationResult i Nothing Nothing Nothing Nothing Nothing "validation_failed" (Just err)

          let allResults = invalidResults <> validResults
          logTagInfo "BulkAssociation" $
            "Completed bulk association. Total rows: " <> show (length allResults)
              <> ", Success: "
              <> show (length (filter (\r -> r.status == "success") allResults))
              <> ", Failed: "
              <> show (length (filter (\r -> r.status /= "success") allResults))

          pure allResults
    [] -> do
      logTagError "BulkAssociation" "No file uploaded"
      pure [BulkFleetAssociationResult 0 Nothing Nothing Nothing Nothing Nothing "failure" (Just "No file uploaded")]

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
  logTagInfo "BulkAssociation" $
    "Processing row " <> show i <> ": fleet_owner=" <> row.fleet_owner_mobile_number
      <> ", vehicle="
      <> row.vehicle_number
      <> ", routes="
      <> row.route_codes

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
  logTagInfo "BulkAssociation" $ "Found fleet owner: " <> show entityDetails.id.getId

  -- Set the fleet owner ID for the vehicle addition
  let mbFleetOwnerId = Just entityDetails.id.getId
      requestorId = Just $ getId entityDetails.id -- Fleet owner is the requestor

  -- Use device identifier as driver identifier (not necessarily phone number)
  let deviceIdentifier = row.device_identifier
      deviceIdentifierType = row.device_identifier_type

  -- Always create/find the device person first
  logTagInfo "BulkAssociation" $ "Processing device: " <> deviceIdentifier <> " with type: " <> deviceIdentifierType

  -- Check if device exists and get their details
  mbDevice <- do
    deviceHash <- getDbHash deviceIdentifier
    QP.findByMobileNumberAndMerchantAndRole mobileCountryCode deviceHash merchant.id DP.DRIVER

  devicePersonId <- case mbDevice of
    Just device -> do
      let devId = device.id
      let fleetOwnerId = entityDetails.id.getId
      logTagInfo "BulkAssociation" $ "Found existing device: " <> show devId <> " for fleet owner: " <> show fleetOwnerId
      mbAssoc <- QFDV.findByDriverIdAndFleetOwnerId devId fleetOwnerId True
      case mbAssoc of
        Nothing -> do
          logTagInfo "BulkAssociation" $ "Creating fleet device association for device: " <> show devId
          QFDV.createFleetDriverAssociationIfNotExists devId entityDetails.id Nothing DVC.BUS True
        Just _ -> logTagInfo "BulkAssociation" $ "Fleet device association already exists for device: " <> show devId
      pure devId
    Nothing -> do
      let deviceName = "Device: " <> deviceIdentifier
          deviceDetails = DriverDetails deviceName deviceIdentifier (Just DVC.BUS) Nothing
      logTagInfo "BulkAssociation" $ "Creating new device: " <> deviceName <> " with " <> deviceIdentifierType <> ": " <> deviceIdentifier
      (person, _) <- fetchOrCreatePerson merchantOpCityId deviceDetails "DEVICEID"
      QFDV.createFleetDriverAssociationIfNotExists person.id entityDetails.id Nothing DVC.BUS True
      pure person.id

  -- Create/find driver person (only if CSV has driver data)
  driverPersonId <- case (row.driver_badge_identifier, row.driver_badge_name, row.driver_identifier_type) of
    (Just driverIdentifier, Just driverName, Just driverIdentifierType) -> do
      logTagInfo "BulkAssociation" $ "Processing driver with " <> driverIdentifierType <> ": " <> driverIdentifier <> " and name: " <> driverName

      mbDriver <- case driverIdentifierType of
        "MOBILENUMBER" -> do
          driverHash <- getDbHash driverIdentifier
          QP.findByMobileNumberAndMerchantAndRole mobileCountryCode driverHash merchant.id DP.DRIVER
        _ -> do
          logTagWarning "BulkAssociation" $ "Unknown driver identifier type: " <> driverIdentifierType <> ", treating as phone number"
          driverHash <- getDbHash driverIdentifier
          QP.findByMobileNumberAndMerchantAndRole mobileCountryCode driverHash merchant.id DP.DRIVER

      case mbDriver of
        Just driver -> do
          let drvId = driver.id
          let fleetOwnerId = entityDetails.id.getId
          logTagInfo "BulkAssociation" $ "Found existing driver: " <> show drvId <> " for fleet owner: " <> show fleetOwnerId
          mbAssoc <- QFDV.findByDriverIdAndFleetOwnerId drvId fleetOwnerId True
          case mbAssoc of
            Nothing -> do
              logTagInfo "BulkAssociation" $ "Creating fleet driver association for driver: " <> show drvId
              QFDV.createFleetDriverAssociationIfNotExists drvId entityDetails.id Nothing DVC.BUS True
            Just _ -> logTagInfo "BulkAssociation" $ "Fleet driver association already exists for driver: " <> show drvId
          pure (Just drvId)
        Nothing -> do
          let driverDetails = DriverDetails ("Driver: " <> driverName) driverIdentifier (Just DVC.BUS) Nothing
          logTagInfo "BulkAssociation" $ "Creating new driver: " <> ("Driver: " <> driverName) <> " with " <> driverIdentifierType <> ": " <> driverIdentifier
          (person, _) <- fetchOrCreatePerson merchantOpCityId driverDetails "MOBILENUMBER"
          QFDV.createFleetDriverAssociationIfNotExists person.id entityDetails.id Nothing DVC.BUS True
          pure (Just person.id)
    _ -> do
      logTagInfo "BulkAssociation" $ "No driver data provided for vehicle " <> row.vehicle_number <> ", skipping driver creation"
      pure Nothing

  -- Conductor handling (only if CSV has conductor data)
  conductorPersonId <- case (row.conductor_badge_identifier, row.conductor_badge_name, row.conductor_identifier_type) of
    (Just conductorIdentifier, Just conductorName, Just conductorIdentifierType) -> do
      logTagInfo "BulkAssociation" $ "Processing conductor with " <> conductorIdentifierType <> ": " <> conductorIdentifier <> " and name: " <> conductorName

      mbConductor <- case conductorIdentifierType of
        "MOBILENUMBER" -> do
          conductorHash <- getDbHash conductorIdentifier
          QP.findByMobileNumberAndMerchantAndRole mobileCountryCode conductorHash merchant.id DP.DRIVER
        _ -> do
          logTagWarning "BulkAssociation" $ "Unknown conductor identifier type: " <> conductorIdentifierType <> ", treating as phone number"
          conductorHash <- getDbHash conductorIdentifier
          QP.findByMobileNumberAndMerchantAndRole mobileCountryCode conductorHash merchant.id DP.DRIVER

      case mbConductor of
        Just conductor -> do
          logTagInfo "BulkAssociation" $ "Found existing conductor: " <> show conductor.id
          pure (Just conductor.id)
        Nothing -> do
          let conductorDetails = DriverDetails ("Conductor: " <> conductorName) conductorIdentifier (Just DVC.BUS) Nothing
          logTagInfo "BulkAssociation" $ "Creating new conductor: " <> ("Conductor: " <> conductorName) <> " with " <> conductorIdentifierType <> ": " <> conductorIdentifier
          (person, _) <- fetchOrCreatePerson merchantOpCityId conductorDetails "MOBILENUMBER"
          pure (Just person.id)
    _ -> do
      logTagInfo "BulkAssociation" $ "No conductor data provided for vehicle " <> row.vehicle_number <> ", skipping conductor creation"
      pure Nothing

  -- Add vehicle using postDriverFleetAddVehicle (this will create driver if needed)
  logTagInfo "BulkAssociation" $ "Adding vehicle " <> row.vehicle_number <> " with device: " <> deviceIdentifier
  vehicleResult <-
    try @_ @SomeException $
      FleetDriver.postDriverFleetAddVehicle
        merchantShortId
        opCity
        deviceIdentifier
        (fromMaybe "" requestorId)
        mbFleetOwnerId
        mbCountryCode
        mbRole
        (buildAddVehicleReqFromRow row)

  case vehicleResult of
    Left err -> do
      logTagError "BulkAssociation" $ "Failed to add vehicle " <> row.vehicle_number <> " for row " <> show i <> ": " <> show err
      pure $ BulkFleetAssociationResult i (Just $ entityDetails.id.getId) Nothing (Just $ show devicePersonId) Nothing Nothing "failure" (Just $ "Vehicle addition failed: " <> show err)
    Right _ -> do
      logTagInfo "BulkAssociation" $ "Successfully added vehicle " <> row.vehicle_number <> " for row " <> show i

      let routeCodes = splitOn "," row.route_codes
      logTagInfo "BulkAssociation" $ "Processing " <> show (length routeCodes) <> " routes for vehicle " <> row.vehicle_number

      routeResults <- fmap partitionEithers $
        forM routeCodes $ \routeCode -> do
          routeM <- QRoute.findByRouteCode routeCode
          case routeM of
            Nothing -> do
              logTagWarning "BulkAssociation" $ "Route with code " <> routeCode <> " not found for vehicle " <> row.vehicle_number
              pure $ Left $ "Route with code " <> routeCode <> " not found"
            Just routeInfo -> do
              logTagInfo "BulkAssociation" $ "Found route " <> routeCode <> " for vehicle " <> row.vehicle_number
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
                  logTagError "BulkAssociation" $ "Failed to create fleet route association for route " <> routeCode <> " and vehicle " <> row.vehicle_number <> ": " <> show err
                  pure $ Left $ "Failed to create fleet route association for route " <> routeCode
                Right _ -> do
                  logTagInfo "BulkAssociation" $ "Created fleet route association for route " <> routeCode <> " and vehicle " <> row.vehicle_number
                  buildVehicleRouteMapping entityDetails.id merchant.id merchantOpCityId.id routeCode routeInfo row.vehicle_number
                  pure $ Right routeCode
      let (routeErrors, successfulRoutes) = routeResults

      if null routeErrors
        then do
          logTagInfo "BulkAssociation" $ "All routes processed successfully for vehicle " <> row.vehicle_number <> ". Routes: " <> T.intercalate ", " successfulRoutes

          -- Handle driver badge
          when (isJust row.driver_badge_name) $ do
            logTagInfo "BulkAssociation" $ "Processing driver badge: " <> fromJust row.driver_badge_name <> " for vehicle " <> row.vehicle_number
            driverBadge <- validateBadgeAssignment (fromJust driverPersonId) merchant.id merchantOpCityId.id entityDetails.id.getId (fromJust row.driver_badge_name) DFBT.DRIVER
            linkFleetBadge' (fromJust driverPersonId) merchant.id merchantOpCityId.id entityDetails.id.getId driverBadge DFBT.DRIVER (Just True)

          -- Handle conductor badge
          when (isJust row.conductor_badge_name && isJust conductorPersonId) $ do
            logTagInfo "BulkAssociation" $ "Processing conductor badge: " <> fromJust row.conductor_badge_name <> " for vehicle " <> row.vehicle_number
            conductorBadge <- validateBadgeAssignment (fromJust conductorPersonId) merchant.id merchantOpCityId.id entityDetails.id.getId (fromJust row.conductor_badge_name) DFBT.CONDUCTOR
            linkFleetBadge' (fromJust conductorPersonId) merchant.id merchantOpCityId.id entityDetails.id.getId conductorBadge DFBT.CONDUCTOR (Just True)

          pure $
            BulkFleetAssociationResult
              i
              (Just $ entityDetails.id.getId)
              (Just $ row.vehicle_number)
              (Just $ show devicePersonId)
              (Just $ T.intercalate ", " successfulRoutes)
              Nothing
              "success"
              Nothing
        else do
          logTagWarning "BulkAssociation" $ "Some routes failed for vehicle " <> row.vehicle_number <> ". Errors: " <> T.intercalate ", " routeErrors
          pure $
            BulkFleetAssociationResult
              i
              (Just $ entityDetails.id.getId)
              (Just $ row.vehicle_number) -- Use actual vehicle number
              (Just $ show devicePersonId)
              (Just $ T.intercalate ", " successfulRoutes) -- Show successful routes
              Nothing
              "partial_success" -- Better status for partial success
              (Just $ "Route errors: " <> T.intercalate ", " routeErrors)

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
    Left err -> logTagError "BulkAssociation" $ "Failed to create vehicle route mapping for route " <> routeCode <> " and vehicle " <> vehicleNumber <> ": " <> show err
    Right _ -> logTagInfo "BulkAssociation" $ "Successfully created vehicle route mapping for route " <> routeCode <> " and vehicle " <> vehicleNumber
