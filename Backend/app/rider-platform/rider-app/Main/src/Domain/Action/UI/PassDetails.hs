{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.PassDetails
  ( getGetOrganizations,
    postPassDetailsUpdate,
    getPassDetailsData,
    getPassDetailsVerificationStatus,
    postPassDetailsUploadDocument,
    getPassDetailsDocument,
    fetchPassDocumentFromS3,
    parsePassEnum,
    parseVerificationStatus,
    computeValidTill,
  )
where

import qualified API.Types.UI.PassDetails as PassDetailsAPI
import AWS.S3 as S3
import qualified BecknV2.OnDemand.Enums as Enums
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Time
import Domain.Types.FRFSRouteDetails (gtfsIdtoDomainCode)
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PassDetails as DPassDetails
import qualified Domain.Types.PassOrganization as DPassOrganization
import qualified Domain.Types.PassType as DPassType
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.RiderConfig as DRiderConfig
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Prelude as EHP (withFile)
import EulerHS.Types (base64Encode)
import GHC.IO.Handle (hFileSize)
import GHC.IO.IOMode (IOMode (..))
import qualified IssueManagement.Domain.Types.MediaFile as DMF
import qualified IssueManagement.Storage.Queries.MediaFile as QMediaFile
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, encrypt)
import qualified Kernel.External.Maps.Google.MapsClient.Types as GT
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.External.MultiModal.Interface as KMultiModal
import Kernel.External.MultiModal.Interface.Types (GeneralVehicleType (..), GetTransitRoutesReq (..), MultiModalLeg, SortingType (..))
import qualified Kernel.External.MultiModal.OpenTripPlanner.Types as OTPTypes
import qualified Kernel.Prelude
import Kernel.ServantMultipart
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Id as Id
import qualified Kernel.Types.TimeBound as DTB
import Kernel.Types.TryException (withTryCatch)
import Kernel.Utils.Common (fromMaybeM, generateGUID, getCurrentTime, logInfo)
import qualified Kernel.Utils.Common as Utils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import Storage.Beam.IssueManagement ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.Queries.FRFSRouteFareProduct as QFRFSRouteFareProduct
import qualified Storage.Queries.FRFSRouteStopStageFare as QFRFSRouteStopStageFare
import qualified Storage.Queries.PassDetails as QPassDetails
import qualified Storage.Queries.PassOrganization as QPassOrganization
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.MultiModal as TMultiModal

parsePassEnum :: Kernel.Prelude.Text -> Environment.Flow DPassType.PassEnum
parsePassEnum "StudentPass" = pure DPassType.StudentPass
parsePassEnum "RegularPass" = pure DPassType.RegularPass
parsePassEnum "TouristPass" = pure DPassType.TouristPass
parsePassEnum _ = Utils.throwError $ InvalidRequest "Invalid pass enum"

parseVerificationStatus :: Kernel.Prelude.Text -> Environment.Flow DPassDetails.VerificationStatus
parseVerificationStatus "PENDING" = pure DPassDetails.PENDING
parseVerificationStatus "CLG_VERIFIED" = pure DPassDetails.CLG_VERIFIED
parseVerificationStatus "MTC_VERIFIED" = pure DPassDetails.MTC_VERIFIED
parseVerificationStatus "REJECTED" = pure DPassDetails.REJECTED
parseVerificationStatus "EXPIRED" = pure DPassDetails.EXPIRED
parseVerificationStatus _ = Utils.throwError $ InvalidRequest "Invalid verification status"

getGetOrganizations ::
  ( ( Kernel.Prelude.Maybe (Id.Id DPerson.Person),
      Id.Id DMerchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow [PassDetailsAPI.GetOrganizationResp]
  )
getGetOrganizations (mbPersonId, _merchantId) passEnumText = do
  passEnum <- parsePassEnum passEnumText
  personId <- mbPersonId & fromMaybeM (PersonNotFound "personId")
  person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  organizations <- QPassOrganization.findByMerchantOperatingCityIdAndPassEnum (DPerson.merchantOperatingCityId person) passEnum
  pure $ map mkPassOrganizationResp organizations

mkPassOrganizationResp :: DPassOrganization.PassOrganization -> PassDetailsAPI.GetOrganizationResp
mkPassOrganizationResp DPassOrganization.PassOrganization {..} =
  PassDetailsAPI.GetOrganizationResp {..}

postPassDetailsUpdate ::
  ( ( Kernel.Prelude.Maybe (Id.Id DPerson.Person),
      Id.Id DMerchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    PassDetailsAPI.PassDetailsUpdateReq ->
    Environment.Flow APISuccess.APISuccess
  )
postPassDetailsUpdate (mbPersonId, _) passEnumText req = do
  passEnum <- parsePassEnum passEnumText
  personId <- mbPersonId & fromMaybeM (PersonNotFound "personId")
  person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mbPassDetail <- QPassDetails.findByPersonId person.id passEnum
  case mbPassDetail of
    Nothing -> createPassDetail req person passEnum
    Just passDetail -> updatePassDetail req person passDetail
  pure APISuccess.Success

createPassDetail :: PassDetailsAPI.PassDetailsUpdateReq -> DPerson.Person -> DPassType.PassEnum -> Environment.Flow ()
createPassDetail req person passEnum = do
  passDetailId <- generateGUID
  now <- getCurrentTime
  validTill <- computeValidTill now person.merchantOperatingCityId
  encAadharNo <- mapM encrypt req.aadharNo
  encGuardianMobile <- mapM encrypt req.guardianMobileNumber
  (applicableRouteIds, routePairs, numberOfStages) <- processRouteDetails person.merchantOperatingCityId req.routeDetails
  refNumber <- getNextReferenceNumber

  let passDetail =
        DPassDetails.PassDetails
          { id = passDetailId,
            name = req.name,
            department = req.department,
            year = req.year,
            guardianName = req.guardianName,
            guardianMobileNumber = encGuardianMobile,
            age = req.age,
            gender = req.gender,
            idCardPicture = req.idCardPicture,
            selfImage = req.selfImage,
            address = req.address,
            pincode = req.pincode,
            aadharNo = encAadharNo,
            routePairs = routePairs,
            registerNo = req.registerNo,
            personId = person.id,
            passEnum = passEnum,
            passOrganizationId = req.passOrganizationId,
            verificationStatus = DPassDetails.PENDING,
            validTill = validTill,
            remark = Nothing,
            academicYearStart = req.academicYearStart,
            academicYearEnd = req.academicYearEnd,
            numberOfStages = numberOfStages,
            referenceNumber = Just (fromInteger refNumber),
            applicableRouteIds = applicableRouteIds,
            createdAt = now,
            updatedAt = now,
            merchantId = person.merchantId,
            merchantOperatingCityId = person.merchantOperatingCityId
          }
  QPassDetails.create passDetail

getNextReferenceNumber :: Environment.Flow Integer
getNextReferenceNumber =
  Hedis.safeGet refNumberKey >>= \case
    Just (_ :: Integer) -> Hedis.incr refNumberKey
    Nothing -> do
      lastRef <- QPassDetails.getLastReferenceNumber
      Hedis.set refNumberKey (fromIntegral lastRef :: Integer)
      Hedis.incr refNumberKey
  where
    refNumberKey :: Kernel.Prelude.Text
    refNumberKey = "CachedQueries:PassDetails:NextReferenceNumber"

computeValidTill ::
  Data.Time.UTCTime ->
  Id.Id DMOC.MerchantOperatingCity ->
  Environment.Flow Data.Time.UTCTime
computeValidTill now moid = do
  riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = moid.getId}) >>= fromMaybeM (RiderConfigDoesNotExist moid.getId)
  let durationDays = maybe 365 (.validityDurationDays) riderConfig.studentPassVerifyConfig
  pure $ Data.Time.addUTCTime (fromIntegral durationDays * Data.Time.nominalDay) now

updatePassDetail :: PassDetailsAPI.PassDetailsUpdateReq -> DPerson.Person -> DPassDetails.PassDetails -> Environment.Flow ()
updatePassDetail req person passDetail = do
  encAadharNo <- mapM encrypt req.aadharNo
  encGuardianMobile <- mapM encrypt req.guardianMobileNumber
  (applicableRouteIds, routePairs, numberOfStages) <- processRouteDetails person.merchantOperatingCityId req.routeDetails

  let updatedPassDetails =
        passDetail
          { DPassDetails.name = req.name,
            DPassDetails.passOrganizationId = req.passOrganizationId,
            DPassDetails.idCardPicture = req.idCardPicture <|> passDetail.idCardPicture,
            DPassDetails.selfImage = req.selfImage,
            DPassDetails.address = req.address <|> passDetail.address,
            DPassDetails.pincode = req.pincode <|> passDetail.pincode,
            DPassDetails.age = req.age <|> passDetail.age,
            DPassDetails.gender = req.gender,
            DPassDetails.guardianName = req.guardianName <|> passDetail.guardianName,
            DPassDetails.guardianMobileNumber = encGuardianMobile <|> passDetail.guardianMobileNumber,
            DPassDetails.department = req.department <|> passDetail.department,
            DPassDetails.year = req.year <|> passDetail.year,
            DPassDetails.aadharNo = encAadharNo <|> passDetail.aadharNo,
            DPassDetails.routePairs = routePairs,
            DPassDetails.applicableRouteIds = applicableRouteIds,
            DPassDetails.numberOfStages = numberOfStages,
            DPassDetails.academicYearStart = req.academicYearStart <|> passDetail.academicYearStart,
            DPassDetails.academicYearEnd = req.academicYearEnd <|> passDetail.academicYearEnd,
            DPassDetails.verificationStatus = DPassDetails.PENDING,
            DPassDetails.remark = Nothing
          }
  QPassDetails.updateByPrimaryKey updatedPassDetails

processRouteDetails ::
  Id.Id DMOC.MerchantOperatingCity ->
  [PassDetailsAPI.RouteDetails] ->
  Environment.Flow (Kernel.Prelude.Maybe [Kernel.Prelude.Text], [DPassDetails.RoutePair], Kernel.Prelude.Maybe Int)
processRouteDetails _ [] = pure (Nothing, [], Nothing)
processRouteDetails moid routeDetails = do
  let dedupedRouteDetails = L.nubBy (\a b -> a.srcStopId == b.srcStopId && a.destStopId == b.destStopId) routeDetails
  integratedBPPConfigs <- SIBC.findAllIntegratedBPPConfig moid Enums.BUS DIBC.MULTIMODAL
  case Kernel.Prelude.listToMaybe integratedBPPConfigs of
    Nothing -> pure (Nothing, [], Nothing)
    Just integratedBPPConfig -> do
      riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = moid.getId}) >>= fromMaybeM (RiderConfigDoesNotExist moid.getId)
      (allRouteIds, allRoutePairs, totalStages) <- foldM (processOneRouteDetail riderConfig integratedBPPConfig) ([], [], 0) dedupedRouteDetails
      pure (Just (L.nub allRouteIds), allRoutePairs, Just totalStages)

processOneRouteDetail ::
  DRiderConfig.RiderConfig ->
  DIBC.IntegratedBPPConfig ->
  ([Kernel.Prelude.Text], [DPassDetails.RoutePair], Int) ->
  PassDetailsAPI.RouteDetails ->
  Environment.Flow ([Kernel.Prelude.Text], [DPassDetails.RoutePair], Int)
processOneRouteDetail riderConfig integratedBPPConfig (accRouteIds, accRoutePairs, accStages) rd = do
  let srcStopCode = rd.srcStopId
      destStopCode = rd.destStopId
  srcMappings <- OTPRest.getRouteStopMappingByStopCode srcStopCode integratedBPPConfig
  destMappings <- OTPRest.getRouteStopMappingByStopCode destStopCode integratedBPPConfig
  let destRouteSet = map (.routeCode) destMappings
      commonRoutes = L.nub $ filter (`elem` destRouteSet) (map (.routeCode) srcMappings)

  mbSrcStationLatLong <- tryStationLatLong integratedBPPConfig srcStopCode
  mbDestStationLatLong <- tryStationLatLong integratedBPPConfig destStopCode
  let mbSrcMappingPt = (.stopPoint) <$> Kernel.Prelude.listToMaybe srcMappings
      mbDestMappingPt = (.stopPoint) <$> Kernel.Prelude.listToMaybe destMappings

  (routeIdsForPair, stages, srcLatLong, destLatLong) <- case commonRoutes of
    (routeCode : _) -> do
      mbStages <- tryStagesFromStageFare integratedBPPConfig commonRoutes srcStopCode destStopCode
      (s, srcLL, destLL) <- case mbStages of
        Just stages -> do
          srcLL <- (mbSrcStationLatLong <|> mbSrcMappingPt) & fromMaybeM (InternalError $ "Missing coordinates for stop " <> srcStopCode)
          destLL <- (mbDestStationLatLong <|> mbDestMappingPt) & fromMaybeM (InternalError $ "Missing coordinates for stop " <> destStopCode)
          pure (stages, srcLL, destLL)
        Nothing -> stagesAndCoordsFromTrip integratedBPPConfig routeCode srcStopCode destStopCode mbSrcStationLatLong mbDestStationLatLong
      pure (commonRoutes, s, srcLL, destLL)
    [] -> do
      logInfo $ "PassDetails OTP fallback for srcStop=" <> srcStopCode <> " destStop=" <> destStopCode
      srcLL <- (mbSrcStationLatLong <|> mbSrcMappingPt) & fromMaybeM (InvalidRequest $ "Missing coordinates for stop " <> srcStopCode)
      destLL <- (mbDestStationLatLong <|> mbDestMappingPt) & fromMaybeM (InvalidRequest $ "Missing coordinates for stop " <> destStopCode)
      (otpRouteIds, otpStages) <- stagesViaOTP riderConfig integratedBPPConfig srcStopCode destStopCode srcLL destLL
      pure (otpRouteIds, otpStages, srcLL, destLL)
  let forwardPair =
        DPassDetails.RoutePair
          { srcStopName = rd.srcStopName,
            srcStopId = srcStopCode,
            destStopName = rd.destStopName,
            destStopId = destStopCode,
            srcLatLong = srcLatLong,
            destLatLong = destLatLong
          }
      reversePair =
        DPassDetails.RoutePair
          { srcStopName = rd.destStopName,
            srcStopId = destStopCode,
            destStopName = rd.srcStopName,
            destStopId = srcStopCode,
            srcLatLong = destLatLong,
            destLatLong = srcLatLong
          }
  pure (accRouteIds <> routeIdsForPair, accRoutePairs <> [forwardPair, reversePair], accStages + stages)

tryStagesFromStageFare ::
  DIBC.IntegratedBPPConfig ->
  [Kernel.Prelude.Text] ->
  Kernel.Prelude.Text ->
  Kernel.Prelude.Text ->
  Environment.Flow (Maybe Int)
tryStagesFromStageFare _ [] _ _ = pure Nothing
tryStagesFromStageFare integratedBPPConfig (routeCode : rest) srcStopCode destStopCode = do
  fareProducts <- QFRFSRouteFareProduct.findByRouteCode routeCode integratedBPPConfig.id
  case find (\fp -> fp.timeBounds == DTB.Unbounded) fareProducts of
    Nothing -> tryStagesFromStageFare integratedBPPConfig rest srcStopCode destStopCode
    Just fareProduct -> do
      mbSrc <- QFRFSRouteStopStageFare.findByRouteAndStopCode fareProduct.farePolicyId routeCode srcStopCode
      mbDest <- QFRFSRouteStopStageFare.findByRouteAndStopCode fareProduct.farePolicyId routeCode destStopCode
      case (mbSrc, mbDest) of
        (Just s, Just d) -> pure $ Just $ max 1 (abs (d.stage - s.stage))
        _ -> tryStagesFromStageFare integratedBPPConfig rest srcStopCode destStopCode

stagesAndCoordsFromTrip ::
  DIBC.IntegratedBPPConfig ->
  Kernel.Prelude.Text ->
  Kernel.Prelude.Text ->
  Kernel.Prelude.Text ->
  Maybe LatLong ->
  Maybe LatLong ->
  Environment.Flow (Int, LatLong, LatLong)
stagesAndCoordsFromTrip integratedBPPConfig routeCode srcStopCode destStopCode mbSrcStationLatLong mbDestStationLatLong = do
  trip <- OTPRest.getExampleTrip integratedBPPConfig routeCode >>= fromMaybeM (InvalidRequest "Could not fetch trip details for route")
  srcTripStop <- OTPRest.findTripStopByStopCode trip srcStopCode & fromMaybeM (InvalidRequest ("Source stop not found in trip: " <> srcStopCode))
  destTripStop <- OTPRest.findTripStopByStopCode trip destStopCode & fromMaybeM (InvalidRequest ("Destination stop not found in trip: " <> destStopCode))
  stages <- case (OTPRest.extractStageFromTripStop srcTripStop, OTPRest.extractStageFromTripStop destTripStop) of
    (Just s, Just d) -> pure $ max 1 (abs (d - s))
    _ -> Utils.throwError $ InvalidRequest "Stage Calculation Failed"
  let srcLL = fromMaybe (LatLong {lat = srcTripStop.lat, lon = srcTripStop.lon}) mbSrcStationLatLong
      destLL = fromMaybe (LatLong {lat = destTripStop.lat, lon = destTripStop.lon}) mbDestStationLatLong
  pure (stages, srcLL, destLL)

stagesViaOTP ::
  DRiderConfig.RiderConfig ->
  DIBC.IntegratedBPPConfig ->
  Kernel.Prelude.Text ->
  Kernel.Prelude.Text ->
  LatLong ->
  LatLong ->
  Environment.Flow ([Kernel.Prelude.Text], Int)
stagesViaOTP riderConfig integratedBPPConfig srcStopCode destStopCode srcLL destLL = do
  transitServiceReq <- TMultiModal.getTransitServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId

  departureTime <- nextPeakHourIST
  let transitRoutesReq =
        GetTransitRoutesReq
          { origin = GT.WayPointV2 {location = GT.LocationV2 {latLng = GT.LatLngV2 {latitude = srcLL.lat, longitude = srcLL.lon}}},
            destination = GT.WayPointV2 {location = GT.LocationV2 {latLng = GT.LatLngV2 {latitude = destLL.lat, longitude = destLL.lon}}},
            arrivalTime = Nothing,
            departureTime = Just departureTime,
            mode = Nothing,
            transitPreferences = Nothing,
            transportModes = Just [Just (OTPTypes.TransportMode "BUS")],
            minimumWalkDistance = riderConfig.minimumWalkDistance,
            permissibleModes = [Bus, Walk],
            maxAllowedPublicTransportLegs = riderConfig.maxAllowedPublicTransportLegs,
            sortingType = Fastest,
            walkSpeed = Nothing
          }

  eOtpResponse <- withTryCatch "stagesViaOTP:getTransitRoutes" (KMultiModal.getTransitRoutes Nothing transitServiceReq transitRoutesReq)
  otpResponse <- case eOtpResponse of
    Right (Just r) -> pure r
    _ -> Utils.throwError $ InvalidRequest $ "No routes found between stops " <> srcStopCode <> " and " <> destStopCode
  perItinerary <- forM otpResponse.routes $ \route -> do
    let busLegs = filter (\leg -> leg.mode == Bus) route.legs
    case busLegs of
      [] -> pure Nothing
      _ -> do
        legResults <- mapM (computeLegStages integratedBPPConfig) busLegs
        let computedLegs = catMaybes legResults
            partialStages = sum computedLegs
            cleanStages = if all isJust legResults then Just partialStages else Nothing
            itineraryRouteIds = collectBusLegRouteIds busLegs
        pure $ Just (cleanStages, partialStages, itineraryRouteIds)
  let usable = catMaybes perItinerary
      allRouteIds = L.nub $ concatMap (\(_, _, rids) -> rids) usable
      cleanStagesList = [s | (Just s, _, _) <- usable]
      partialStagesList = [p | (Nothing, p, _) <- usable]
  chosenStages <- case (cleanStagesList, partialStagesList) of
    (s : rest, _) -> pure (Kernel.Prelude.minimum (s : rest))
    ([], p : rest) -> pure (Kernel.Prelude.minimum (p : rest)) -- failing-bucket fallback: under-counts on purpose, see docstring
    ([], []) -> Utils.throwError $ InvalidRequest $ "No routes found between stops " <> srcStopCode <> " and " <> destStopCode
  pure (allRouteIds, max 1 chosenStages)

nextPeakHourIST :: Environment.Flow Data.Time.UTCTime
nextPeakHourIST = do
  now <- getCurrentTime
  let istOffset = 19800 :: Data.Time.NominalDiffTime -- +05:30
      nowIST = Data.Time.addUTCTime istOffset now
      today = Data.Time.utctDay nowIST
      tomorrow = Data.Time.addDays 1 today
      nineHrs = Data.Time.secondsToDiffTime (9 * 3600)
      tomorrowNineIST = Data.Time.UTCTime tomorrow nineHrs
  pure $ Data.Time.addUTCTime (negate istOffset) tomorrowNineIST

computeLegStages :: DIBC.IntegratedBPPConfig -> MultiModalLeg -> Environment.Flow (Maybe Int)
computeLegStages integratedBPPConfig leg = do
  let mbLegRouteCode = (Kernel.Prelude.listToMaybe leg.routeDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode
      mbFromCode = (leg.fromStopDetails >>= (.stopCode)) <|> ((leg.fromStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode)
      mbToCode = (leg.toStopDetails >>= (.stopCode)) <|> ((leg.toStopDetails >>= (.gtfsId)) <&> gtfsIdtoDomainCode)
  case (mbLegRouteCode, mbFromCode, mbToCode) of
    (Just legRouteCode, Just fromCode, Just toCode) -> do
      mbTrip <- OTPRest.getExampleTrip integratedBPPConfig legRouteCode
      pure $ do
        trip <- mbTrip
        srcStop <- OTPRest.findTripStopByStopCode trip fromCode
        destStop <- OTPRest.findTripStopByStopCode trip toCode
        s <- OTPRest.extractStageFromTripStop srcStop
        d <- OTPRest.extractStageFromTripStop destStop
        pure (max 1 (abs (d - s)))
    _ -> pure Nothing

collectBusLegRouteIds :: [MultiModalLeg] -> [Kernel.Prelude.Text]
collectBusLegRouteIds busLegs =
  L.nub $
    [ gtfsIdtoDomainCode gtfsId
      | leg <- busLegs,
        rd <- leg.routeDetails,
        Just gtfsId <- [rd.gtfsId]
    ]

tryStationLatLong :: DIBC.IntegratedBPPConfig -> Kernel.Prelude.Text -> Environment.Flow (Maybe LatLong)
tryStationLatLong integratedBPPConfig stopCode = do
  mbStation <- OTPRest.getStationByGtfsIdAndStopCode stopCode integratedBPPConfig
  pure $ do
    station <- mbStation
    lat <- station.lat
    lon <- station.lon
    pure $ LatLong {lat = lat, lon = lon}

getPassDetailsData ::
  ( ( Kernel.Prelude.Maybe (Id.Id DPerson.Person),
      Id.Id DMerchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow PassDetailsAPI.PassDetailsDataResp
  )
getPassDetailsData (mbPersonId, _) passEnumText = do
  passEnum <- parsePassEnum passEnumText
  personId <- mbPersonId & fromMaybeM (PersonNotFound "personId")
  person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  passDetail <- QPassDetails.findByPersonId person.id passEnum >>= fromMaybeM (PassDetailsNotFound personId.getId)
  org <- QPassOrganization.findById passDetail.passOrganizationId >>= fromMaybeM (PassOrganizationNotFound passDetail.passOrganizationId.getId)
  decGuardianMobile <- mapM decrypt passDetail.guardianMobileNumber
  pure $ mkPassDetailResp decGuardianMobile passDetail org

mkPassDetailResp :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> DPassDetails.PassDetails -> DPassOrganization.PassOrganization -> PassDetailsAPI.PassDetailsDataResp
mkPassDetailResp decGuardianMobile DPassDetails.PassDetails {..} org =
  PassDetailsAPI.PassDetailsDataResp
    { passDetailsId = id,
      passOrganizationId = org.id,
      passOrganizationName = org.name,
      guardianMobileNumber = decGuardianMobile,
      ..
    }

getPassDetailsVerificationStatus ::
  ( ( Kernel.Prelude.Maybe (Id.Id DPerson.Person),
      Id.Id DMerchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow PassDetailsAPI.PassStatusResp
  )
getPassDetailsVerificationStatus (mbPersonId, _) passEnumText = do
  passEnum <- parsePassEnum passEnumText
  personId <- mbPersonId & fromMaybeM (PersonNotFound "personId")
  person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  passDetail <- QPassDetails.findByPersonId person.id passEnum >>= fromMaybeM (PassDetailsNotFound personId.getId)
  pure $ mkPassStatusResp passDetail

mkPassStatusResp :: DPassDetails.PassDetails -> PassDetailsAPI.PassStatusResp
mkPassStatusResp DPassDetails.PassDetails {..} =
  PassDetailsAPI.PassStatusResp {..}

instance FromMultipart Tmp PassDetailsAPI.UploadDocumentReq where
  fromMultipart form = do
    fileData <- lookupFile "file" form
    let fileName = fdFileName fileData
        tmpPath = fdPayload fileData
        encodedPath = T.unpack $ fileName <> "\NUL" <> T.pack tmpPath
    pure $ PassDetailsAPI.UploadDocumentReq {file = encodedPath}

instance ToMultipart Tmp PassDetailsAPI.UploadDocumentReq where
  toMultipart req =
    MultipartData
      []
      [FileData "file" "" "image/jpeg" req.file]

postPassDetailsUploadDocument ::
  ( ( Kernel.Prelude.Maybe (Id.Id DPerson.Person),
      Id.Id DMerchant.Merchant
    ) ->
    PassDetailsAPI.UploadDocumentReq ->
    Environment.Flow PassDetailsAPI.UploadDocumentResp
  )
postPassDetailsUploadDocument (mbPersonId, merchantId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "personId")
  merchantConfig <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  let (originalFileName, actualFilePath) = case T.splitOn "\NUL" (T.pack req.file) of
        [fname, p] -> (fname, T.unpack p)
        _ -> ("", req.file)
  fileExtension <- allowedFileExtension originalFileName
  fileSize <- L.runIO $ EHP.withFile actualFilePath ReadMode hFileSize
  when (fileSize > fromIntegral merchantConfig.mediaFileSizeUpperLimit) $
    Utils.throwError $ FileSizeExceededError (show fileSize)
  rawBytes <- L.runIO $ BS.readFile actualFilePath
  let normalize ext = if ext == ".jpeg" then ".jpg" else ext
  unless (detectFileType rawBytes == Just (normalize fileExtension)) $
    Utils.throwError $ InvalidRequest "Uploaded file content does not match the declared file type"
  documentId <- generateGUID
  let imageData = base64Encode rawBytes
      s3FileType = if fileExtension == ".pdf" then S3.PDF else S3.Image
  s3FilePath <- S3.createFilePath "/pass-details/" ("pass-details-" <> personId.getId <> "-" <> documentId.getId) s3FileType fileExtension
  let fileUrl =
        merchantConfig.mediaFileUrlPattern
          & T.replace "<DOMAIN>" "passDetails"
          & T.replace "<FILE_PATH>" s3FilePath
  S3.put (T.unpack s3FilePath) imageData
  now <- getCurrentTime
  QMediaFile.create $
    DMF.MediaFile
      { id = documentId,
        _type = s3FileType,
        url = fileUrl,
        s3FilePath = Just s3FilePath,
        status = Just DMF.COMPLETED,
        createdAt = now
      }
  pure $ PassDetailsAPI.UploadDocumentResp {documentId}

getPassDetailsDocument ::
  ( ( Kernel.Prelude.Maybe (Id.Id DPerson.Person),
      Id.Id DMerchant.Merchant
    ) ->
    Id.Id DMF.MediaFile ->
    Environment.Flow Kernel.Prelude.Text
  )
getPassDetailsDocument (mbPersonId, _) documentId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "personId")
  mediaFile <- QMediaFile.findById documentId >>= fromMaybeM (InvalidRequest "Document not found")
  s3FilePath <- mediaFile.s3FilePath & fromMaybeM (InvalidRequest "Document has no associated S3 path")
  let ownerPrefix = "/pass-details/pass-details-" <> personId.getId <> "-"
  unless (T.isInfixOf ownerPrefix s3FilePath) $
    Utils.throwError AccessDenied
  fetchPassDocumentFromS3 s3FilePath

fetchPassDocumentFromS3 :: Kernel.Prelude.Text -> Environment.Flow Kernel.Prelude.Text
fetchPassDocumentFromS3 s3FilePath = do
  when (T.isInfixOf ".." s3FilePath) $
    Utils.throwError $ InvalidRequest "filePath must not contain path-traversal sequences"
  unless (T.isInfixOf "/pass-details/" s3FilePath) $
    Utils.throwError $ InvalidRequest "filePath must reside under /pass-details/"
  S3.get (T.unpack s3FilePath)

allowedFileExtension :: Kernel.Prelude.Text -> Environment.Flow Kernel.Prelude.Text
allowedFileExtension name =
  let lowerName = T.toLower name
   in if
          | T.isSuffixOf ".png" lowerName -> pure ".png"
          | T.isSuffixOf ".jpeg" lowerName -> pure ".jpeg"
          | T.isSuffixOf ".jpg" lowerName -> pure ".jpg"
          | T.isSuffixOf ".pdf" lowerName -> pure ".pdf"
          | otherwise -> Utils.throwError $ InvalidRequest "Only jpg, jpeg, png, and pdf files are allowed"

detectFileType :: BS.ByteString -> Maybe Kernel.Prelude.Text
detectFileType bs
  | BS.isPrefixOf (BS.pack [0xFF, 0xD8, 0xFF]) bs = Just ".jpg"
  | BS.isPrefixOf (BS.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]) bs = Just ".png"
  | BS.isPrefixOf (BS.pack [0x25, 0x50, 0x44, 0x46, 0x2D]) bs = Just ".pdf"
  | otherwise = Nothing
