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
import Control.Applicative ((<|>))
import qualified Dashboard.Common as Common
import Data.Aeson ()
import qualified Data.Csv as Csv
import Data.Either (partitionEithers)
import Data.OpenApi ()
import qualified Data.Text as T
import qualified Domain.Action.Dashboard.Fleet.Driver as FleetDriver
import qualified Domain.Action.UI.Registration as Registration
import qualified Domain.Types.FleetBadgeType as DFBT
import qualified Domain.Types.FleetRouteAssociation as FleetRouteAssociation
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Domain.Types.VehicleVariant (VehicleVariant (..))
import qualified Domain.Types.VehicleVariant as DVV (castVehicleVariantToVehicleCategory)
import Environment (Flow)
import Kernel.External.Encryption (getDbHash)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id (Id (..), ShortId, getId)
import Kernel.Utils.Common (generateGUID, getCurrentTime, logTagInfo)
import Servant
import Servant.Multipart
import SharedLogic.Merchant (findMerchantByShortId)
import SharedLogic.WMB (linkFleetBadge, validateBadgeAssignment)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.FleetDriverAssociationExtra as FDV
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.FleetRouteAssociation as QFRA
import qualified Storage.Queries.Person as QP

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
      let csvData = fdPayload file -- strict ByteString
      case Csv.decodeByName csvData of
        Left err -> pure [BulkFleetAssociationResult 0 Nothing Nothing Nothing Nothing Nothing "failure" (Just $ "CSV parse error: " <> toText err)]
        Right (_, rows) -> do
          results <- forM (zip [1 ..] (toList rows)) $ \(i, row) -> handleBulkRow merchantShortId opCity i row
          pure results
    [] -> pure [BulkFleetAssociationResult 0 Nothing Nothing Nothing Nothing Nothing "failure" (Just "No file uploaded")]

-- Helper to build AuthReq from CSV row
buildDriverReqFromRow :: BulkFleetAssociationRow -> Registration.AuthReq
buildDriverReqFromRow row =
  Registration.AuthReq
    { mobileNumber = Just row.device_identifier,
      mobileCountryCode = Just "+91",
      merchantId = "", -- Fill this in before calling createDriverWithDetails
      merchantOperatingCity = Nothing,
      email = Nothing,
      name = Just $ fromMaybe row.device_identifier row.driver_badge_name,
      identifierType = case row.device_identifier_type of
        "DEVICEID" -> Just DP.DEVICEID
        "AADHAAR" -> Just DP.AADHAAR
        "EMAIL" -> Just DP.EMAIL
        _ -> Just DP.DEVICEID,
      registrationLat = Nothing,
      registrationLon = Nothing
    }

-- Helper to build AddVehicleReq from CSV row
buildAddVehicleReqFromRow :: BulkFleetAssociationRow -> API.AddVehicleReq
buildAddVehicleReqFromRow row =
  API.AddVehicleReq
    { registrationNo = (getField @"vehicle_number" row),
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
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  -- 1. Fleet owner lookup
  let countryCode = "+91"
  mobileNumberHash <- getDbHash row.fleet_owner_mobile_number
  mPerson <- QP.findByMobileNumberAndMerchantAndRole countryCode mobileNumberHash merchant.id DP.FLEET_OWNER
  mFleetOwnerRaw <- case mPerson of
    Just person -> QFOI.findByPrimaryKey person.id
    Nothing -> pure Nothing
  case mFleetOwnerRaw of
    Nothing -> pure $ BulkFleetAssociationResult i Nothing Nothing Nothing Nothing Nothing "failure" (Just "Fleet owner not found")
    Just fleetOwner -> do
      let fleetOwnerPersonId = getId fleetOwner.fleetOwnerPersonId -- Get the Text ID
      let fleetOwnerId = fleetOwner.fleetOwnerPersonId -- Keep the Id Person type
      -- 2. Create/find driver with device_identifier as mobileNumber and device_identifier_type as identifierType
      let deviceId = row.device_identifier
      let deviceIdType = case row.device_identifier_type of
            "DEVICEID" -> Just DP.DEVICEID
            "AADHAAR" -> Just DP.AADHAAR
            "EMAIL" -> Just DP.EMAIL
            _ -> Just DP.DEVICEID -- fallback
      deviceIdHash <- getDbHash deviceId
      mDriver <- QP.findByMobileNumberAndMerchantAndRole countryCode deviceIdHash merchant.id DP.DRIVER
      driverId <- case mDriver of
        Just driver -> pure driver.id
        Nothing -> do
          let driverReq =
                (buildDriverReqFromRow row)
                  { Registration.mobileNumber = Just deviceId,
                    Registration.identifierType = deviceIdType,
                    Registration.merchantId = getId merchant.id,
                    Registration.merchantOperatingCity = Just opCity
                  }
          driver <- Registration.createDriverWithDetails driverReq Nothing Nothing Nothing Nothing Nothing merchant.id merchantOpCityId False
          pure driver.id

      -- Create fleet-driver association if it doesn't exist
      logTagInfo "BulkAssociation" $ "Creating fleet-driver association for driver: " <> getId driverId <> " and fleet: " <> fleetOwnerPersonId
      void $ FDV.createFleetDriverAssociationIfNotExists driverId (Id fleetOwnerPersonId :: Id DP.Person) Nothing (DVV.castVehicleVariantToVehicleCategory TAXI) True

      -- 3. Add vehicle with device ID as phone number, identifier type as per CSV, no OTP
      let phoneNo = row.device_identifier -- Use the device identifier from CSV
      let requestorId = fleetOwnerPersonId -- Use fleet owner's ID for authorization
      let mbFleetOwnerId = Just fleetOwnerPersonId -- Use fleet owner's ID for validation
      let mbCountryCode = Just "+91"
      let mbRole = Just Common.DRIVER -- Use DRIVER since we want to look up the device ID as a driver
      logTagInfo "BulkAssociation" $ "Adding vehicle with parameters:"
      vehicleResult <- tryAny $ FleetDriver.postDriverFleetAddVehicle merchantShortId opCity phoneNo requestorId mbFleetOwnerId mbCountryCode mbRole (buildAddVehicleReqFromRow row)
      case vehicleResult of
        Left err -> pure $ BulkFleetAssociationResult i (Just fleetOwnerPersonId) Nothing (Just $ show driverId) Nothing Nothing "failure" (Just $ "Vehicle error: " <> show err)
        Right _ -> do
          now <- getCurrentTime
          -- 4. Route association using route_codes from CSV
          let routeCodes = T.splitOn "," $ getField @"route_codes" row
          routeResults <- forM routeCodes $ \routeCode -> do
            assocId <- generateGUID
            let assoc =
                  FleetRouteAssociation.FleetRouteAssociation
                    { FleetRouteAssociation.id = Id assocId,
                      FleetRouteAssociation.fleetOwnerId = fleetOwnerId, -- Use Id Person type
                      FleetRouteAssociation.merchantId = merchant.id,
                      FleetRouteAssociation.merchantOperatingCityId = merchantOpCityId,
                      FleetRouteAssociation.routeCode = routeCode,
                      FleetRouteAssociation.createdAt = now,
                      FleetRouteAssociation.updatedAt = now
                    }
            res <- tryAny $ QFRA.create assoc
            case res of
              Left err -> pure $ Left $ "Route association  error for " <> routeCode <> ": " <> show err
              Right _ -> pure $ Right routeCode

          case partitionEithers routeResults of
            ([], successfulRoutes) -> do
              -- 5. Badge association using badge identifier and type from CSV
              badgeResult <- tryAny $ do
                when (isJust row.driver_badge_name) $ do
                  driverBadge <- validateBadgeAssignment driverId merchant.id merchantOpCityId fleetOwnerPersonId (fromJust row.driver_badge_name) DFBT.DRIVER
                  linkFleetBadge driverId merchant.id merchantOpCityId fleetOwnerPersonId driverBadge DFBT.DRIVER
                when (isJust row.conductor_badge_name) $ do
                  conductorBadge <- validateBadgeAssignment driverId merchant.id merchantOpCityId fleetOwnerPersonId (fromJust row.conductor_badge_name) DFBT.CONDUCTOR
                  linkFleetBadge driverId merchant.id merchantOpCityId fleetOwnerPersonId conductorBadge DFBT.CONDUCTOR
              case badgeResult of
                Left err -> pure $ BulkFleetAssociationResult i (Just fleetOwnerPersonId) Nothing (Just $ show driverId) Nothing Nothing "failure" (Just $ "Badge error: " <> show err)
                Right _ -> pure $ BulkFleetAssociationResult i (Just fleetOwnerPersonId) (Just row.vehicle_number) (Just $ show driverId) (Just $ T.intercalate "," successfulRoutes) (row.driver_badge_name <|> row.conductor_badge_name) "success" Nothing
            (errors, _) -> pure $ BulkFleetAssociationResult i (Just fleetOwnerPersonId) Nothing (Just $ show driverId) Nothing Nothing "failure" (Just $ T.intercalate ", " errors)
