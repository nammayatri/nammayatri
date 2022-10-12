module API.Dashboard.Driver where

import Beckn.External.Encryption (decrypt)
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (LatLong))
import Beckn.Utils.Common
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver as Common
import Data.Coerce
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Text as T
import Domain.Action.UI.DriverOnboarding.Status (ResponseStatus (..))
import qualified Domain.Action.UI.DriverOnboarding.Status as St
import Domain.Types.DriverOnboarding.DriverLicense
import Domain.Types.DriverOnboarding.DriverRCAssociation (DriverRCAssociation)
import qualified Domain.Types.DriverOnboarding.IdfyVerification as IV
import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate
import Domain.Types.Person
import Environment
import Servant hiding (throwError)
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverOnboarding.Status as QDocStatus
import qualified Storage.Queries.Person as QPerson

type API =
  "driver"
    :> ( DriverDocumentsInfoAPI
           :<|> DriverListAPI
           :<|> DriverActivityAPI
           :<|> EnableDriversAPI
           :<|> DisableDriversAPI
           :<|> DriverLocationAPI
       )

type DriverDocumentsInfoAPI =
  "documents"
    :> "info"
    :> Common.DriverDocumentsInfoAPI

type DriverListAPI = "list" :> Common.DriverListAPI

type DriverActivityAPI = "activity" :> Common.DriverActivityAPI

type EnableDriversAPI = "enable" :> Common.EnableDriversAPI

type DisableDriversAPI = "disable" :> Common.DisableDriversAPI

type DriverLocationAPI = "location" :> Common.DriverLocationAPI

handler :: FlowServer API
handler =
  driverDocumentsInfo
    :<|> listDrivers
    :<|> driverActivity
    :<|> enableDrivers
    :<|> disableDrivers
    :<|> driverLocation

---------

-- FIXME: not tested yet because of no onboarding test data
driverDocumentsInfo :: FlowHandler Common.DriverDocumentsInfoRes
driverDocumentsInfo = withFlowHandlerAPI $ do
  now <- getCurrentTime
  onboardingTryLimit <- asks (.driverOnboardingConfigs.onboardingTryLimit)
  drivers <- QDocStatus.fetchDriverDocsInfo Nothing
  pure $ foldl' (func onboardingTryLimit now) Common.emptyInfo drivers
  where
    oneMonth :: NominalDiffTime
    oneMonth = 60 * 60 * 24 * 30
    func :: Int -> UTCTime -> Common.DriverDocumentsInfoRes -> QDocStatus.DriverDocsInfo -> Common.DriverDocumentsInfoRes
    func onboardingTryLimit now acc fd = do
      let mbLic = fd.license
          mbRegCert = fd.assocReg
          mbLicReq = fd.licenseVerificationReq
          mbVehRegReq = fd.regVerificationReq

          dlStatus = getLicenseStatus onboardingTryLimit fd.numLicenseImages mbLic mbLicReq
          rcStatus = getRegCertStatus onboardingTryLimit fd.numVehRegImages mbRegCert mbVehRegReq

          mbDlExpiration = (.licenseExpiry) <$> mbLic
          mbRcExpiration = getRcExpiration . snd <$> mbRegCert
          dlExpiresInMonth = expiresInMonth mbDlExpiration
          rcExpiresInMonth = expiresInMonth mbRcExpiration
          expiresInMonth mbExp = fromMaybe False $
            flip fmap mbExp $
              \exp_ -> let dif = diffUTCTime exp_ now in dif < oneMonth && dif > 0

      acc{Common.registered = acc.registered + 1,
          Common.verified = incrIf fd.driverInfo.verified acc.verified,
          Common.enabled = incrIf fd.driverInfo.enabled acc.enabled,
          Common.validDocuments = incrDocs (dlStatus == VALID) (rcStatus == VALID) acc.validDocuments,
          Common.invalidDocuments = incrDocs (dlStatus == INVALID) (rcStatus == INVALID) acc.invalidDocuments,
          Common.verificationPending = incrDocs (dlStatus == PENDING) (rcStatus == PENDING) acc.verificationPending,
          Common.verificationFailed = incrDocs (dlStatus == FAILED) (rcStatus == FAILED) acc.verificationFailed,
          Common.verificationLimitExceeded = incrDocs (dlStatus == LIMIT_EXCEED) (rcStatus == LIMIT_EXCEED) acc.verificationLimitExceeded,
          Common.docsExpiringInMonth = incrDocs dlExpiresInMonth rcExpiresInMonth acc.docsExpiringInMonth
         }

incrIf :: Num a => Bool -> a -> a
incrIf b = if b then (+ 1) else identity

incrDocs :: Bool -> Bool -> Common.DocumentsByStateInfo -> Common.DocumentsByStateInfo
incrDocs lic vehReg old =
  old{Common.driverLicense = incrIf lic old.driverLicense,
      Common.vehicleRegistrationCertificate = incrIf vehReg old.vehicleRegistrationCertificate
     }

getRcExpiration :: VehicleRegistrationCertificate -> UTCTime
getRcExpiration = (.fitnessExpiry)

getLicenseStatus :: Int -> Int -> Maybe DriverLicense -> Maybe IV.IdfyVerification -> St.ResponseStatus
getLicenseStatus onboardingTryLimit currentTries mbLicense mbLicReq =
  case mbLicense of
    Just driverLicense -> St.mapStatus driverLicense.verificationStatus
    Nothing -> St.verificationStatus onboardingTryLimit currentTries mbLicReq

getRegCertStatus :: Int -> Int -> Maybe (DriverRCAssociation, VehicleRegistrationCertificate) -> Maybe IV.IdfyVerification -> St.ResponseStatus
getRegCertStatus onboardingTryLimit currentTries mbRegCert mbVehRegReq =
  case mbRegCert of
    Just (_assoc, vehicleRC) -> St.mapStatus vehicleRC.verificationStatus
    Nothing -> St.verificationStatus onboardingTryLimit currentTries mbVehRegReq

---------

getDlStatusInfo :: EncFlow m r => DriverLicense -> m (Common.DocumentInfo Common.LicDetails)
getDlStatusInfo lic = do
  let licDetails =
        Just
          Common.LicDetails
            { licExpiry = lic.licenseExpiry,
              vehicleClass = map show lic.classOfVehicles
            }
  documentNumber <- decrypt lic.licenseNumber
  pure
    Common.DocumentInfo
      { documentNumber,
        status = show lic.verificationStatus,
        details = licDetails
      }

getRcStatusInfo :: EncFlow m r => VehicleRegistrationCertificate -> m (Common.DocumentInfo Common.RCDetails)
getRcStatusInfo rc = do
  let rcDetails =
        Just
          Common.RCDetails
            { vehicleClass = fromMaybe "" rc.vehicleClass,
              fitnessExpiry = rc.fitnessExpiry,
              insuranceExpiry = rc.insuranceValidity
            }
  documentNumber <- decrypt rc.certificateNumber
  pure
    Common.DocumentInfo
      { documentNumber,
        status = show rc.verificationStatus,
        details = rcDetails
      }

limitOffset :: Maybe Int -> Maybe Int -> [a] -> [a]
limitOffset mbLimit mbOffset =
  maybe identity take mbLimit . maybe identity drop mbOffset

listDrivers :: Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> FlowHandler Common.DriverListRes
listDrivers mbLimit mbOffset mbVerified mbEnabled mbPendingdoc mbSearchPhone = withFlowHandlerAPI $ do
  driverDocsInfo <- QDocStatus.fetchFullDriverInfoWithDocsFirstnameAsc Nothing
  items <- catMaybes <$> mapM buildDriverListItem driverDocsInfo
  let limitedItems = limitOffset mbLimit mbOffset items
  pure $ Common.DriverListRes (length limitedItems) limitedItems
  where
    filterOnStatus :: Maybe Bool -> Bool -> Bool
    filterOnStatus mbRequestedStatus driverStatus =
      maybe True (== driverStatus) mbRequestedStatus

    filterPhone :: Text -> Bool
    filterPhone driverPhone = maybe True (`T.isInfixOf` driverPhone) mbSearchPhone

    buildDriverListItem :: (EncFlow m r) => QDocStatus.FullDriverWithDocs -> m (Maybe Common.DriverListItem)
    buildDriverListItem f = do
      let pendingDocDriver = isNothing f.license || isNothing f.registration
      if not $
        filterOnStatus mbVerified f.info.verified
          && filterOnStatus mbEnabled f.info.enabled
          && filterOnStatus mbPendingdoc pendingDocDriver
        then pure Nothing
        else do
          let p = f.person; drin = f.info; veh = f.vehicle
          phoneNo <- maybe (pure "") decrypt p.mobileNumber
          if not $ filterPhone phoneNo
            then pure Nothing
            else do
              dlStatusInfo <- traverse getDlStatusInfo f.license
              rcStatusInfo <- traverse getRcStatusInfo (fmap snd f.registration)
              pure $
                Just
                  Common.DriverListItem
                    { driverId = cast p.id,
                      firstName = p.firstName,
                      middleName = p.middleName,
                      lastName = p.lastName,
                      vehicleNo = veh <&> (.registrationNo),
                      phoneNo,
                      enabled = drin.enabled,
                      verified = drin.verified,
                      dlStatus = dlStatusInfo,
                      rcStatus = rcStatusInfo
                    }

---------------------------------------------------------------------
driverActivity :: FlowHandler Common.DriverActivityRes
driverActivity = withFlowHandlerAPI $ do
  foldl' func Common.emptyDriverActivityRes <$> QPerson.findAllDrivers
  where
    func :: Common.DriverActivityRes -> QPerson.FullDriver -> Common.DriverActivityRes
    func acc x =
      if x.info.active
        then acc {Common.activeDriversInApp = acc.activeDriversInApp + 1}
        else acc {Common.inactiveDrivers = acc.inactiveDrivers + 1}

---------------------------------------------------------------------
enableDrivers :: Common.DriverIds -> FlowHandler Common.EnableDriversRes
enableDrivers req = withFlowHandlerAPI $ do
  let enable = True
  updatedDrivers <- QDriverInfo.updateEnabledStateReturningIds (coerce req.driverIds) enable
  let driversNotFound = filter (not . (`elem` coerce @[Id Driver] @[Id Common.Driver] updatedDrivers)) req.driverIds
  let numDriversEnabled = length updatedDrivers
  pure $
    Common.EnableDriversRes
      { numDriversEnabled,
        driversEnabled = coerce updatedDrivers,
        message = mconcat [show numDriversEnabled, " drivers enabled, following drivers not found: ", show $ coerce @_ @[Text] driversNotFound]
      }

---------------------------------------------------------------------
disableDrivers :: Common.DriverIds -> FlowHandler Common.DisableDriversRes
disableDrivers req = withFlowHandlerAPI $ do
  let enable = False
  updatedDrivers <- QDriverInfo.updateEnabledStateReturningIds (coerce req.driverIds) enable
  let driversNotFound = filter (not . (`elem` coerce @[Id Driver] @[Id Common.Driver] updatedDrivers)) req.driverIds
  let numDriversDisabled = length updatedDrivers
  pure $
    Common.DisableDriversRes
      { numDriversDisabled,
        driversDisabled = coerce updatedDrivers,
        message = mconcat [show numDriversDisabled, " drivers disabled, following drivers not found: ", show $ coerce @_ @[Text] driversNotFound]
      }

driverLocation :: Maybe Int -> Maybe Int -> Common.DriverIds -> FlowHandler Common.DriverLocationRes
driverLocation mbLimit mbOffset req = withFlowHandler $ do
  let driverIds = coerce req.driverIds
  allDrivers <- QPerson.findAllDriversByIdsFirstnameAsc driverIds
  let driversNotFound =
        filter (not . (`elem` map ((.id) . (.person)) allDrivers)) driverIds
      limitedDrivers = limitOffset mbLimit mbOffset allDrivers
  resultList <- mapM buildDriverLocationListItem limitedDrivers
  pure $ Common.DriverLocationRes (nonEmpty $ coerce driversNotFound) resultList

buildDriverLocationListItem :: (EncFlow m r) => QPerson.FullDriver -> m Common.DriverLocationItem
buildDriverLocationListItem f = do
  let p = f.person
      v = f.vehicle
  phoneNo <- maybe (pure "") decrypt p.mobileNumber
  pure
    Common.DriverLocationItem
      { driverId = cast p.id,
        firstName = p.firstName,
        middleName = p.middleName,
        lastName = p.lastName,
        vehicleNo = v.registrationNo,
        phoneNo,
        active = f.info.active,
        onRide = f.info.onRide,
        location = LatLong f.location.lat f.location.lon,
        lastLocationTimestamp = f.location.coordinatesCalculatedAt
      }
