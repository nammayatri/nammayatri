{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Common.BulkUpload
  ( DriverDetails (..),
    ParsedVehicleRow,
    addIndividualOrFleetSeekerDrivers,
    castCommonOnboardingAsToDomain,
    checkAndUpdateAddDriverLimit,
    fetchOrCreatePerson,
    processDriverRows,
    readDriversCsv,
    readVehiclesCsv,
    validateDriverName,
  )
where

import qualified "this" API.Types.ProviderPlatform.Fleet.Driver as Common
import qualified "this" API.Types.ProviderPlatform.Management.DriverRegistration as DRegCommon
import qualified "this" API.Types.ProviderPlatform.Management.Endpoints.Driver as MgmtDriver
import Control.Applicative (optional)
import qualified "lib-dashboard" Dashboard.Common as DC
import qualified "lib-dashboard" Dashboard.Common.Driver
import qualified "this" Dashboard.ProviderPlatform.Management.Driver as DashDriver
import Data.Csv
import qualified Data.Text as T
import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Action.UI.Registration as DReg
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.FleetBadgeType as DFBT
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Route as DRoute
import qualified Domain.Types.TransporterConfig as DTCConfig
import qualified Domain.Types.VehicleCategory as DVC
import Environment
import EulerHS.Prelude ((<|>))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import Kernel.Utils.Validation
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified SharedLogic.DriverOnboarding.Common as SOnbCommon
import qualified SharedLogic.DriverOnboarding.OnboardingComms as SOnboardingComms
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.MobileNumberValidation as MobileValidation
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Route as CQRoute
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Csv as Csv
import Tools.Encryption
import Tools.Error

---------------------------------------------------------------------
-- Drivers
---------------------------------------------------------------------

data CreateDriversCSVRow = CreateDriversCSVRow
  { driverName :: Maybe Text,
    driverPhoneNumber :: Text,
    driverOnboardingVehicleCategory :: Maybe Text,
    fleetPhoneNo :: Maybe Text,
    badgeName :: Maybe Text,
    badgeRank :: Maybe Text,
    badgeType :: Maybe Text,
    driverPhoneCountryCode :: Maybe Text,
    fleetPhoneCountryCode :: Maybe Text,
    firstName :: Maybe Text,
    lastName :: Maybe Text,
    email :: Maybe Text,
    gender :: Maybe Text
  }

data DriverDetails = DriverDetails
  { rowNumber :: Int,
    driverName :: Maybe Text,
    driverPhoneNumber :: Text,
    driverOnboardingVehicleCategory :: Maybe DVC.VehicleCategory,
    fleetPhoneNo :: Maybe Text,
    badgeName :: Maybe Text,
    badgeRank :: Maybe Text,
    badgeType :: Maybe DFBT.FleetBadgeType,
    driverPhoneCountryCode :: Maybe Text,
    fleetPhoneCountryCode :: Maybe Text,
    firstName :: Maybe Text,
    lastName :: Maybe Text,
    email :: Maybe Text,
    gender :: Maybe DP.Gender
  }

instance FromNamedRecord CreateDriversCSVRow where
  parseNamedRecord r =
    CreateDriversCSVRow
      <$> optional (r .: "driver_name")
      <*> r .: "driver_phone_number"
      <*> optional (r .: "driver_onboarding_vehicle_category")
      <*> optional (r .: "fleet_phone_no")
      <*> optional (r .: "badge_name")
      <*> optional (r .: "badge_rank")
      <*> optional (r .: "badge_type")
      <*> optional (r .: "driver_phone_country_code")
      <*> optional (r .: "fleet_phone_country_code")
      <*> optional (r .: "first_name")
      <*> optional (r .: "last_name")
      <*> optional (r .: "email")
      <*> optional (r .: "gender")

castCommonOnboardingAsToDomain :: Dashboard.Common.Driver.OnboardingAs -> DI.OnboardingAs
castCommonOnboardingAsToDomain Dashboard.Common.Driver.FLEET_DRIVER = DI.FLEET_DRIVER
castCommonOnboardingAsToDomain Dashboard.Common.Driver.INDIVIDUAL = DI.INDIVIDUAL

addIndividualOrFleetSeekerDrivers :: ShortId DM.Merchant -> Context.City -> Dashboard.Common.Driver.OnboardingAs -> FilePath -> Flow DRegCommon.AddDriversResp
addIndividualOrFleetSeekerDrivers merchantShortId opCity onboardingAs file = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
  driverDetails <- readDriversCsv transporterConfig file
  (processedDrivers, unprocessedEntities) <-
    processDriverRows "process:addIndividualOrFleetSeekerDrivers" driverDetails (\phoneNo err -> "Unable to add Driver (" <> phoneNo <> "): " <> err) $ \req_ -> do
      whenJust req_.fleetPhoneNo $ \_ -> throwError (InvalidRequest "Fleet phone number is not applicable for adding individual or fleet-seeking drivers")
      validateDriverName req_.driverName transporterConfig.isDriverNameMandatoryInBulkUpload transporterConfig.isStrongNameCheckRequired
      let driverCountryCode = fromMaybe (P.getCountryMobileCode merchantOpCity.country) req_.driverPhoneCountryCode
      MobileValidation.validateMobileNumber transporterConfig req_.driverPhoneNumber driverCountryCode merchantOpCity.country
      (person, isNew) <- fetchOrCreatePerson merchantOpCity req_
      -- Only the OTP flow may change an existing driver's type, so bulk leaves existing numbers untouched.
      unless isNew $ throwError (InvalidRequest "Driver already exists")
      checkAndUpdateAddDriverLimit merchantOpCity.id transporterConfig person.id
      SOnboardingComms.setOnboardingAs transporterConfig person (castCommonOnboardingAsToDomain onboardingAs)
      fork "Sending onboarding link SMS to Driver" $
        SOnbCommon.sendOnboardingLinkSms merchantOpCity transporterConfig person Nothing
      pure person.id
  pure $
    DRegCommon.AddDriversResp
      { processedDrivers = map (\p -> DRegCommon.ProcessedDriverInfo {driverId = p.driverId, phoneNo = p.phoneNo}) processedDrivers,
        unprocessedEntities
      }

readDriversCsv :: DTCConfig.TransporterConfig -> FilePath -> Flow [DriverDetails]
readDriversCsv transporterConfig file = do
  driverDetails <- Csv.readCsv @CreateDriversCSVRow @DriverDetails file parseDriverInfo
  let maxAddDriversCsvRows = fromMaybe 100 (transporterConfig.limitsConfig >>= (.maxAddDriversCsvRows))
  when (length driverDetails > maxAddDriversCsvRows) $ throwError $ MaxDriversLimitExceeded maxAddDriversCsvRows
  when (not (null driverDetails) && all (\d -> T.null d.driverPhoneNumber) driverDetails) $
    throwError (InvalidRequest "No valid mobile numbers found in the uploaded file. Please check the file format.")
  pure driverDetails

processDriverRows :: Text -> [DriverDetails] -> (Text -> Text -> Text) -> (DriverDetails -> Flow (Id DP.Person)) -> Flow ([Common.ProcessedDriverInfo], [Text])
processDriverRows tag driverDetails mkFailure func =
  foldlM
    ( \(processed, unprocessed) driverDetail ->
        if T.null driverDetail.driverPhoneNumber
          then return (processed, unprocessed <> ["Row " <> show driverDetail.rowNumber <> ": Mobile number is required"])
          else
            withTryCatch
              tag
              (func driverDetail)
              >>= \case
                Left err -> return (processed, unprocessed <> [mkFailure driverDetail.driverPhoneNumber (T.pack (displayException err))])
                Right driverPersonId ->
                  return (processed <> [Common.ProcessedDriverInfo {driverId = cast @DP.Person @Common.Driver driverPersonId, phoneNo = driverDetail.driverPhoneNumber}], unprocessed)
    )
    ([], [])
    driverDetails

mkAddDriverCounterKey :: Id DMOC.MerchantOperatingCity -> Id DP.Person -> Text
mkAddDriverCounterKey merchantOpCityId driverId = "Fleet:AddDriverCount:" <> driverId.getId <> ":" <> merchantOpCityId.getId

checkAndUpdateAddDriverLimit :: Id DMOC.MerchantOperatingCity -> DTCConfig.TransporterConfig -> Id DP.Person -> Flow ()
checkAndUpdateAddDriverLimit merchantOpCityId transporterConfig driverId =
  whenJust transporterConfig.addDriverCountWindow $ \window -> do
    let key = mkAddDriverCounterKey merchantOpCityId driverId
    whenJust transporterConfig.addDriverCountThreshold $ \threshold -> do
      addDriverCount <- Redis.withNonCriticalCrossAppRedis $ SWC.getCurrentWindowCount key window
      when (addDriverCount >= fromIntegral threshold) $ do
        logInfo $ "Add driver rate limit hit for driver " <> driverId.getId <> " with count " <> show addDriverCount
        throwError (HitsLimitError (fromIntegral window.period * fromIntegral (SWC.convertPeriodTypeToSeconds window.periodType) :: Int))
    Redis.withNonCriticalCrossAppRedis $ SWC.incrementWindowCount key window

validateDriverName :: Maybe Text -> Bool -> Bool -> Flow ()
validateDriverName mbDriverName isMandatory isStrongNameCheckRequired = do
  case mbDriverName of
    Nothing -> when isMandatory $ throwError $ InvalidRequest "Driver name is required"
    Just driverName -> do
      let validateFn = if isStrongNameCheckRequired then DashDriver.validateUpdateDriverNameReq else DashDriver.validateUpdateDriverNameReqWithLooseCheck
      result <- try (void $ runRequestValidation validateFn (MgmtDriver.UpdateDriverNameReq {firstName = driverName, middleName = Nothing, lastName = Nothing}))
      case result of
        Left (_ :: SomeException) ->
          throwError . InvalidRequest $
            if isStrongNameCheckRequired
              then "Driver name should not contain numbers and should have at least 1 letter and at most 50 letters"
              else "Driver name must be 1 to 50 characters: letters, digits, spaces and ' \x2019 - . , & / ( ) + are allowed"
        Right _ -> pure ()

parseDriverInfo :: Int -> CreateDriversCSVRow -> Flow DriverDetails
parseDriverInfo idx row = do
  let firstName :: Maybe Text = Csv.cleanMaybeCSVField idx (fromMaybe "" row.firstName) "First name"
      lastName :: Maybe Text = Csv.cleanMaybeCSVField idx (fromMaybe "" row.lastName) "Last name"
      email :: Maybe Text = Csv.cleanMaybeCSVField idx (fromMaybe "" row.email) "Email"
      gender :: Maybe DP.Gender = Csv.readMaybeCSVField idx (fromMaybe "" row.gender) "Gender"
      -- first_name/last_name win over the combined column; the combined form stays filled
      -- either way since SMS, badges and name validation read it.
      fullName = firstName <&> \fn -> maybe fn (\ln -> fn <> " " <> ln) lastName
      driverName :: Maybe Text = fullName <|> (row.driverName >>= \name -> Csv.cleanMaybeCSVField idx name "Driver name")
      -- Do not hard-fail the whole upload on a blank phone; keep the row (with an empty
      -- phone) so it can be reported per-row in unprocessedEntities during processing.
      driverPhoneNumber :: Text = fromMaybe "" (Csv.cleanMaybeCSVField idx row.driverPhoneNumber "Mobile number")
      driverOnboardingVehicleCategory :: Maybe DVC.VehicleCategory = Csv.readMaybeCSVField idx (fromMaybe "" row.driverOnboardingVehicleCategory) "Onboarding Vehicle Category"
      fleetPhoneNo :: Maybe Text = Csv.cleanMaybeCSVField idx (fromMaybe "" row.fleetPhoneNo) "Fleet number"
      badgeName :: Maybe Text = Csv.readMaybeCSVField idx (fromMaybe "" row.badgeName) "Badge name"
      badgeRank :: Maybe Text = Csv.readMaybeCSVField idx (fromMaybe "" row.badgeRank) "Badge rank"
      badgeType :: Maybe DFBT.FleetBadgeType = Csv.readMaybeCSVField idx (fromMaybe "" row.badgeType) "Badge type"
      driverPhoneCountryCode :: Maybe Text = DCommon.appendPlusInMobileCountryCode $ Csv.cleanMaybeCSVField idx (fromMaybe "" row.driverPhoneCountryCode) "Driver phone country code"
      fleetPhoneCountryCode :: Maybe Text = DCommon.appendPlusInMobileCountryCode $ Csv.cleanMaybeCSVField idx (fromMaybe "" row.fleetPhoneCountryCode) "Fleet phone country code"
  pure $
    DriverDetails
      { rowNumber = idx + 2, -- +1 for the header row, +1 to make the 0-based index human-readable (matches the CSV line)
        driverName = driverName,
        driverPhoneNumber = driverPhoneNumber,
        driverOnboardingVehicleCategory = driverOnboardingVehicleCategory,
        fleetPhoneNo = fleetPhoneNo,
        badgeName = badgeName,
        badgeRank = badgeRank,
        badgeType = badgeType,
        driverPhoneCountryCode = driverPhoneCountryCode,
        fleetPhoneCountryCode = fleetPhoneCountryCode,
        firstName = firstName,
        lastName = lastName,
        email = email,
        gender = gender
      }

fetchOrCreatePerson :: DMOC.MerchantOperatingCity -> DriverDetails -> Flow (DP.Person, Bool)
fetchOrCreatePerson moc req_ = do
  let mobileCountryCode = fromMaybe (P.getCountryMobileCode moc.country) req_.driverPhoneCountryCode
  let authData =
        DReg.AuthReq
          { mobileNumber = Just req_.driverPhoneNumber,
            mobileCountryCode = Just mobileCountryCode,
            merchantId = moc.merchantId.getId,
            merchantOperatingCity = Just moc.city,
            email = Nothing,
            name = req_.driverName,
            identifierType = Just DP.MOBILENUMBER,
            registrationLat = Nothing,
            registrationLon = Nothing,
            otpChannel = Nothing,
            password = Nothing,
            employeeId = Nothing,
            isOperatorReq = Nothing
          }
  mobileNumberHash <- getDbHash req_.driverPhoneNumber
  QPerson.findByMobileNumberAndMerchantAndRole mobileCountryCode mobileNumberHash moc.merchantId DP.DRIVER
    >>= \case
      Nothing -> do
        whenJust req_.email $ \reqEmail -> do
          runRequestValidation (\e -> validateField "email" e P.email) reqEmail
          mbExistingByEmail <- QPerson.findByEmailAndMerchantIdAndRole (Just reqEmail) moc.merchantId DP.DRIVER
          when (isJust mbExistingByEmail) $ throwError (EmailAlreadyLinked reqEmail)
        cloudType <- asks (.cloudType)
        person <- DReg.createDriverWithDetails authData Nothing Nothing Nothing Nothing Nothing Nothing cloudType moc.merchantId moc.id True req_.firstName req_.lastName req_.email req_.gender
        let isNew = True in pure (person, isNew)
      Just person -> do
        let isNew = False in pure (person, isNew)

---------------------------------------------------------------------
-- Vehicles
---------------------------------------------------------------------

data VehicleDetailsCSVRow = VehicleDetailsCSVRow
  { registrationNo :: Text,
    airConditioned :: Text,
    routeCodes :: Maybe Text,
    vehicleCategory :: Maybe Text,
    fleetPhoneNo :: Maybe Text,
    driverPhoneNo :: Maybe Text,
    countryCode :: Maybe Text,
    manufacturer :: Maybe Text,
    model :: Maybe Text,
    colour :: Maybe Text,
    vehicleClass :: Maybe Text,
    vehicleModelYear :: Maybe Text
  }
  deriving (Show)

instance FromNamedRecord VehicleDetailsCSVRow where
  parseNamedRecord r =
    VehicleDetailsCSVRow
      <$> r .: "registration_no"
      <*> r .: "air_conditioned"
      <*> optional (r .: "route_codes")
      <*> optional (r .: "vehicle_category")
      <*> optional (r .: "fleet_phone_no")
      <*> optional (r .: "driver_phone_no")
      <*> optional (r .: "country_code")
      <*> optional (r .: "manufacturer")
      <*> optional (r .: "model")
      <*> optional (r .: "colour")
      <*> optional (r .: "vehicle_class")
      <*> optional (r .: "vehicle_model_year")

type ParsedVehicleRow = (DRegCommon.RegisterRCReq, DbHash, [DRoute.Route], Maybe Text, Maybe Text, Maybe Text)

readVehiclesCsv :: DTCConfig.TransporterConfig -> DMOC.MerchantOperatingCity -> FilePath -> Flow [ParsedVehicleRow]
readVehiclesCsv transporterConfig merchantOpCity file = do
  rows <- Csv.readCsv @VehicleDetailsCSVRow @ParsedVehicleRow file (parseVehicleInfo merchantOpCity)
  let maxVehiclesCsvRows = fromMaybe 100 (transporterConfig.limitsConfig >>= (.maxVehiclesCsvRows))
  when (length rows > maxVehiclesCsvRows) $ throwError $ MaxVehiclesLimitExceeded maxVehiclesCsvRows
  pure rows

parseVehicleInfo :: DMOC.MerchantOperatingCity -> Int -> VehicleDetailsCSVRow -> Flow ParsedVehicleRow
parseVehicleInfo moc idx row = do
  let airConditioned :: (Maybe Bool) = Csv.readMaybeCSVField idx row.airConditioned "Air Conditioned"
      mbRouteCodes :: Maybe [Text] = Csv.readMaybeCSVField idx (fromMaybe "" row.routeCodes) "Route Codes"
      mbFleetPhoneNo :: Maybe Text = Csv.cleanMaybeCSVField idx (fromMaybe "" row.fleetPhoneNo) "Fleet Phone Number"
      mbDriverPhoneNo :: Maybe Text = Csv.cleanMaybeCSVField idx (fromMaybe "" row.driverPhoneNo) "Driver Phone Number"
      mbCountryCode :: Maybe Text = Csv.cleanMaybeCSVField idx (fromMaybe "" row.countryCode) "Country Code"
      vehicleCategory :: Maybe DVC.VehicleCategory = Csv.readMaybeCSVField idx (fromMaybe "" row.vehicleCategory) "Vehicle Category"
      mbManufacturer :: Maybe Text = Csv.cleanMaybeCSVField idx (fromMaybe "" row.manufacturer) "Manufacturer"
      mbModel :: Maybe Text = Csv.cleanMaybeCSVField idx (fromMaybe "" row.model) "Model"
      mbColour :: Maybe Text = Csv.cleanMaybeCSVField idx (fromMaybe "" row.colour) "Colour"
      mbVehicleClass :: Maybe Text = Csv.cleanMaybeCSVField idx (fromMaybe "" row.vehicleClass) "Vehicle Class"
      mbVehicleModelYear :: Maybe Int = Csv.readMaybeCSVField idx (fromMaybe "" row.vehicleModelYear) "Vehicle Model Year"
  vehicleRegistrationCertNumber <- Csv.cleanCSVField idx row.registrationNo "Registration No"
  mbVehicleDetails <- case (mbManufacturer, mbModel, mbColour) of
    (Just manufacturer', Just model', Just colour') ->
      pure $
        Just
          DC.DriverVehicleDetails
            { vehicleManufacturer = manufacturer',
              vehicleModel = model',
              vehicleColour = colour',
              vehicleDoors = Nothing,
              vehicleSeatBelts = Nothing,
              vehicleModelYear = mbVehicleModelYear
            }
    (Nothing, Nothing, Nothing) -> pure Nothing
    _ -> throwError $ InvalidRequest $ "Manufacturer, model and colour must either all be filled or all be left empty at row: " <> show idx
  vehicleNumberHash <- getDbHash vehicleRegistrationCertNumber
  routes <-
    case mbRouteCodes of
      Just routeCodes -> mapM (\routeCode -> CQRoute.findByRouteCode routeCode >>= fromMaybeM (RouteNotFound routeCode)) routeCodes
      Nothing -> pure []
  pure
    ( DRegCommon.RegisterRCReq
        { dateOfRegistration = Nothing,
          oxygen = Nothing,
          ventilator = Nothing,
          operatingCity = show moc.city,
          imageId = Id "bulkVehicleUpload",
          imageId2 = Nothing,
          vehicleDetails = mbVehicleDetails,
          vehicleClass = mbVehicleClass,
          vehicleCategory = Just $ fromMaybe DVC.CAR vehicleCategory,
          udinNumber = Nothing,
          engineNumber = Nothing,
          chassisNumber = Nothing,
          previousRcNumber = Nothing,
          ..
        },
      vehicleNumberHash,
      routes,
      mbCountryCode,
      mbFleetPhoneNo,
      mbDriverPhoneNo
    )
