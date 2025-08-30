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
import Control.Applicative (optional)
import qualified Dashboard.Common ()
import Data.Aeson ()
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum, isDigit)
import Data.Csv ((.:))
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

-- Parse route codes from various CSV formats
parseRouteCodes :: Text -> [Text]
parseRouteCodes rawText =
  let cleaned = T.strip rawText
   in if T.null cleaned
        then []
        else
          let -- Remove outer quotes if present: "..." -> ...
              unquoted =
                if T.length cleaned >= 2 && T.head cleaned == '"' && T.last cleaned == '"'
                  then T.drop 1 $ T.dropEnd 1 cleaned
                  else cleaned

              -- Handle different array formats
              codes = case () of
                _ | T.null unquoted -> []
                -- Format: ["ACV1L-U","ACV1L-D"] or [ACV1L-U,ACV1L-D]
                _
                  | T.length unquoted >= 2 && T.head unquoted == '[' && T.last unquoted == ']' ->
                    let inner = T.drop 1 $ T.dropEnd 1 unquoted
                     in if T.any (== '"') inner
                          then -- Has quotes: "ACV1L-U","ACV1L-D"
                            map (T.strip . T.filter (/= '"')) $ splitOn "," inner
                          else -- No quotes: ACV1L-U,ACV1L-D
                            map T.strip $ splitOn "," inner
                -- Format: ACV1L-U,ACV1L-D (simple comma-separated)
                _ -> map T.strip $ splitOn "," unquoted
           in filter (not . T.null) codes

-- Device identifier validation based on type
validateDeviceIdentifier :: Text -> Text -> Either Text Text
validateDeviceIdentifier identifierType deviceId =
  let cleaned = T.strip deviceId
      len = T.length cleaned
   in case () of
        _ | T.null cleaned -> Left "Device identifier cannot be empty"
        _ -> case identifierType of
          "MOBILENUMBER" -> validateMobileNumber cleaned len
          "DEVICEID" -> validateDeviceId cleaned len
          "EMAIL" -> validateEmail cleaned len
          "AADHAAR" -> validateAadhaar cleaned len
          _ -> Left $ "Unsupported identifier type: " <> identifierType
  where
    validateMobileNumber mobile len =
      case () of
        _ | T.null mobile -> Left "Mobile number cannot be empty"
        _ | len /= 10 -> Left "Mobile number must be exactly 10 digits"
        _ | not (T.all isDigit mobile) -> Left "Mobile number can only contain digits"
        _ | T.head mobile `notElem` ['6', '7', '8', '9'] -> Left "Mobile number must start with 6, 7, 8, or 9"
        _ -> Right mobile

    validateDeviceId dId len =
      case () of
        _ | T.null dId -> Left "Device ID cannot be empty"
        _ | len < 10 -> Left "Device ID must be at least 10 characters"
        _ | len > 20 -> Left "Device ID cannot exceed 20 characters"
        _ | not (T.all isDigit dId) -> Left "Device ID can only contain letters and numbers"
        _ -> Right dId

    validateEmail email len =
      case () of
        _ | T.null email -> Left "Email cannot be empty"
        _ | not (T.elem '@' email) -> Left "Email must contain @ symbol"
        _ | len < 5 -> Left "Email must be at least 5 characters"
        _ | len > 100 -> Left "Email cannot exceed 100 characters"
        _ -> Right email

    validateAadhaar aadhaar len =
      case () of
        _ | T.null aadhaar -> Left "Aadhaar number cannot be empty"
        _ | len /= 12 -> Left "Aadhaar number must be exactly 12 digits"
        _ | not (T.all isDigit aadhaar) -> Left "Aadhaar number can only contain digits"
        _ -> Right aadhaar

validateVehicleNumber :: Text -> Either Text Text
validateVehicleNumber vehicleNum =
  let cleaned = T.strip vehicleNum
      len = T.length cleaned
   in case () of
        _ | T.null cleaned -> Left "Vehicle number cannot be empty"
        _ | len < 1 || len > 11 -> Left "Vehicle number must be between 1 and 11 characters"
        _ | not (T.all isAlphaNum cleaned) -> Left "Vehicle number can only contain letters and numbers"
        _ -> Right cleaned

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

instance Csv.FromNamedRecord BulkFleetAssociationRow where
  parseNamedRecord r =
    BulkFleetAssociationRow
      <$> r .: "fleet_owner_mobile_number"
      <*> r .: "device_identifier"
      <*> r .: "device_identifier_type"
      <*> r .: "vehicle_number"
      <*> r .: "route_codes"
      <*> optional (r .: "driver_badge_name")
      <*> optional (r .: "conductor_badge_name")
      <*> optional (r .: "driver_badge_identifier")
      <*> optional (r .: "driver_identifier_type")
      <*> optional (r .: "conductor_badge_identifier")
      <*> optional (r .: "conductor_identifier_type")

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
  -- Validate device identifier type is valid
  case row.device_identifier_type of
    "MOBILENUMBER" -> pure ()
    "AADHAAR" -> pure ()
    "EMAIL" -> pure ()
    "DEVICEID" -> pure ()
    invalid -> Left $ "Invalid device identifier type: '" <> invalid <> "'. Valid values are: MOBILENUMBER, AADHAAR, EMAIL, DEVICEID"
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
          pure [BulkFleetAssociationResult 0 Nothing Nothing Nothing Nothing Nothing "failure" (Just ("CSV parse error: " <> toText err))]
        Right (_, rows) -> do
          logTagInfo "BulkAssociation" $ "Successfully parsed " <> show (length rows) <> " rows from CSV"

          -- Validate all rows first
          let validationResults =
                zip [1 ..] (toList rows) <&> \(i, row) ->
                  case validateRow row of
                    Left err -> Left (i, err)
                    Right _ -> Right (i, row)

          let (invalidRows, validRows) = partitionEithers validationResults

          unless (null invalidRows) $ do
            logTagWarning "BulkAssociation" $ "Found " <> show (length invalidRows) <> " invalid rows: " <> show invalidRows

          -- Process valid rows with exception handling
          validResults <- forM validRows $ \(i, row) -> do
            handleBulkRow merchantShortId opCity i row `catch` \(e :: SomeException) ->
              pure $ BulkFleetAssociationResult i Nothing Nothing Nothing Nothing Nothing "failure" (Just $ "Unexpected error processing row: " <> show e)

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
      (person, _) <- fetchOrCreatePerson merchantOpCityId deviceDetails deviceIdentifierType
      QFDV.createFleetDriverAssociationIfNotExists person.id entityDetails.id Nothing DVC.BUS True
      pure person.id

  -- Create/find driver person (only if CSV has driver data)
  driverPersonId <- case (row.driver_badge_identifier, row.driver_badge_name, row.driver_identifier_type) of
    (Just driverIdentifier, Just driverName, Just driverIdentifierType) -> do
      mbDriver <- case driverIdentifierType of
        "MOBILENUMBER" -> do
          driverHash <- getDbHash driverIdentifier
          QP.findByMobileNumberAndMerchantAndRole mobileCountryCode driverHash merchant.id DP.DRIVER
        _ -> do
          driverHash <- getDbHash driverIdentifier
          QP.findByMobileNumberAndMerchantAndRole mobileCountryCode driverHash merchant.id DP.DRIVER

      case mbDriver of
        Just driver -> do
          let drvId = driver.id
          let fleetOwnerId = entityDetails.id.getId
          mbAssoc <- QFDV.findByDriverIdAndFleetOwnerId drvId fleetOwnerId True
          case mbAssoc of
            Nothing -> QFDV.createFleetDriverAssociationIfNotExists drvId entityDetails.id Nothing DVC.BUS True
            Just _ -> pure ()
          pure (Just drvId)
        Nothing -> do
          let driverDetails = DriverDetails ("Driver: " <> driverName) driverIdentifier (Just DVC.BUS) Nothing
          (person, _) <- fetchOrCreatePerson merchantOpCityId driverDetails "MOBILENUMBER"
          QFDV.createFleetDriverAssociationIfNotExists person.id entityDetails.id Nothing DVC.BUS True
          pure (Just person.id)
    _ -> pure Nothing

  -- Conductor handling (only if CSV has conductor data)
  conductorPersonId <- case (row.conductor_badge_identifier, row.conductor_badge_name, row.conductor_identifier_type) of
    (Just conductorIdentifier, Just conductorName, Just conductorIdentifierType) -> do
      mbConductor <- case conductorIdentifierType of
        "MOBILENUMBER" -> do
          conductorHash <- getDbHash conductorIdentifier
          QP.findByMobileNumberAndMerchantAndRole mobileCountryCode conductorHash merchant.id DP.DRIVER
        _ -> do
          conductorHash <- getDbHash conductorIdentifier
          QP.findByMobileNumberAndMerchantAndRole mobileCountryCode conductorHash merchant.id DP.DRIVER

      case mbConductor of
        Just conductor -> pure (Just conductor.id)
        Nothing -> do
          let conductorDetails = DriverDetails ("Conductor: " <> conductorName) conductorIdentifier (Just DVC.BUS) Nothing
          (person, _) <- fetchOrCreatePerson merchantOpCityId conductorDetails "MOBILENUMBER"
          pure (Just person.id)
    _ -> pure Nothing

  -- Validate device identifier BEFORE any database operations
  case validateDeviceIdentifier row.device_identifier_type row.device_identifier of
    Left deviceError -> pure $ BulkFleetAssociationResult i (Just $ entityDetails.id.getId) Nothing (Just $ show entityDetails.id) Nothing Nothing "failure" (Just $ "Device validation failed: " <> deviceError)
    Right _ -> do
      -- Validate vehicle number BEFORE any database operations
      case validateVehicleNumber row.vehicle_number of
        Left vehicleError -> pure $ BulkFleetAssociationResult i (Just $ entityDetails.id.getId) Nothing (Just $ show entityDetails.id) Nothing Nothing "failure" (Just $ "Vehicle number validation failed: " <> vehicleError)
        Right _ -> do
          -- Validate all routes BEFORE any database operations
          let routeCodes = parseRouteCodes row.route_codes
          routeValidationResults <- fmap partitionEithers $
            forM routeCodes $ \routeCode -> do
              routeM <- QRoute.findByRouteCode routeCode
              case routeM of
                Nothing -> pure $ Left $ "Route '" <> routeCode <> "' does not exist in system"
                Just routeInfo -> pure $ Right (routeCode, routeInfo)

          let (routeErrors, validatedRoutes) = routeValidationResults
          if not (null routeErrors)
            then pure $ BulkFleetAssociationResult i (Just $ entityDetails.id.getId) Nothing (Just $ show entityDetails.id) Nothing Nothing "failure" (Just $ "Route validation failed: " <> T.intercalate ", " routeErrors)
            else do
              -- All validations passed, proceed with vehicle creation
              -- Vehicle creation (wrapped in exception handling)
              ( do
                  _vehicleResult <-
                    FleetDriver.postDriverFleetAddVehicle
                      merchantShortId
                      opCity
                      deviceIdentifier
                      (fromMaybe "" requestorId)
                      mbFleetOwnerId
                      mbCountryCode
                      mbRole
                      (buildAddVehicleReqFromRow row)

                  -- Vehicle creation succeeded, proceed with route and badge associations
                  -- Create route associations using pre-validated routes with idempotency check
                  routeResults <-
                    forM validatedRoutes $ \(routeCode, routeInfo) -> do
                      -- Check if fleet route association already exists (idempotent)
                      existingAssoc <- QFRA.findByFleetOwnerRouteAndCity entityDetails.id merchantOpCityId.id routeCode
                      case existingAssoc of
                        Just _ -> do
                          -- Association already exists, skip creation but still create vehicle route mapping
                          buildVehicleRouteMapping entityDetails.id merchant.id merchantOpCityId.id routeCode routeInfo row.vehicle_number
                          pure $ Right routeCode
                        Nothing ->
                          -- Create new association with exception handling
                          ( do
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
                              QFRA.create assoc
                              buildVehicleRouteMapping entityDetails.id merchant.id merchantOpCityId.id routeCode routeInfo row.vehicle_number
                              pure (Right routeCode)
                          )
                            `catch` \(e :: SomeException) ->
                              pure $ Left $ "Failed to create fleet-route association for route '" <> routeCode <> "': " <> show e
                  let (routeAssocErrors, successfulRoutes) = partitionEithers routeResults

                  -- Handle badge assignments with proper error reporting
                  badgeErrors <-
                    catMaybes
                      <$> sequence
                        [ -- Driver badge
                          if isJust row.driver_badge_name && isJust driverPersonId
                            then
                              ( do
                                  driverBadge <- validateBadgeAssignment (fromJust driverPersonId) merchant.id merchantOpCityId.id entityDetails.id.getId (fromJust row.driver_badge_name) DFBT.DRIVER
                                  linkFleetBadge' (fromJust driverPersonId) merchant.id merchantOpCityId.id entityDetails.id.getId driverBadge DFBT.DRIVER (Just True)
                                  pure Nothing
                              )
                                `catch` \(e :: SomeException) ->
                                  pure $ Just $ "Driver badge assignment failed for '" <> fromJust row.driver_badge_name <> "': " <> show e
                            else pure Nothing,
                          -- Conductor badge
                          if isJust row.conductor_badge_name && isJust conductorPersonId
                            then
                              ( do
                                  conductorBadge <- validateBadgeAssignment (fromJust conductorPersonId) merchant.id merchantOpCityId.id entityDetails.id.getId (fromJust row.conductor_badge_name) DFBT.CONDUCTOR
                                  linkFleetBadge' (fromJust conductorPersonId) merchant.id merchantOpCityId.id entityDetails.id.getId conductorBadge DFBT.CONDUCTOR (Just True)
                                  pure Nothing
                              )
                                `catch` \(e :: SomeException) ->
                                  pure $ Just $ "Conductor badge assignment failed for '" <> fromJust row.conductor_badge_name <> "': " <> show e
                            else pure Nothing
                        ]

                  let allErrors = routeAssocErrors ++ badgeErrors
                  if null allErrors
                    then
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
                    else
                      pure $
                        BulkFleetAssociationResult
                          i
                          (Just $ entityDetails.id.getId)
                          (if null routeAssocErrors then Just $ row.vehicle_number else Nothing)
                          (Just $ show devicePersonId)
                          (if null routeAssocErrors then Just $ T.intercalate ", " successfulRoutes else Nothing)
                          Nothing
                          (if null routeAssocErrors then "partial_success" else "failure")
                          (Just $ T.intercalate "; " allErrors)
                )
                `catch` \(e :: SomeException) ->
                  pure $ BulkFleetAssociationResult i (Just $ entityDetails.id.getId) Nothing (Just $ show devicePersonId) Nothing Nothing "failure" (Just $ "Vehicle creation failed: " <> show e)

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
  ( QVRM.upsert mapping vehicleNumberHash
      >> logTagInfo "BulkAssociation" ("Successfully created vehicle route mapping for route " <> routeCode <> " and vehicle " <> vehicleNumber)
    )
    `catch` \(e :: SomeException) ->
      logTagError "BulkAssociation" $ "Failed to create vehicle route mapping for route " <> routeCode <> " and vehicle " <> vehicleNumber <> ": " <> show e
