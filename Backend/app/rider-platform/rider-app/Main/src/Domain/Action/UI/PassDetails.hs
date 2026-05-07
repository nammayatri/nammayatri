{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.PassDetails
  ( getGetOrganizations,
    postPassDetailsUpdate,
    getPassDetailsData,
    getPassDetailsVerificationStatus,
    parsePassEnum,
    parseVerificationStatus,
  )
where

import qualified API.Types.UI.PassDetails as PassDetailsAPI
import qualified BecknV2.OnDemand.Enums as Enums
import qualified Data.List as L
import qualified Data.Time
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PassDetails as DPassDetails
import qualified Domain.Types.PassOrganization as DPassOrganization
import qualified Domain.Types.PassType as DPassType
import qualified Domain.Types.Person as DPerson
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (encrypt)
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common (fromMaybeM, generateGUID, getCurrentTime)
import qualified Kernel.Utils.Common as Utils
import qualified Lib.JourneyModule.Utils as JMUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.PassDetails as QPassDetails
import qualified Storage.Queries.PassOrganization as QPassOrganization
import qualified Storage.Queries.Person as QPerson
import Tools.Error

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
mkPassOrganizationResp organization =
  PassDetailsAPI.GetOrganizationResp
    { PassDetailsAPI.id = organization.id,
      PassDetailsAPI.name = organization.name,
      PassDetailsAPI.address = organization.address
    }

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
  let validTill = Data.Time.addUTCTime (730 * Data.Time.nominalDay) now
  encAadharNo <- mapM encrypt req.aadharNo
  (applicableRouteIds, routePairs, numberOfStages) <- processRouteDetails person.merchantOperatingCityId req.routeDetails
  refNumber <- getNextReferenceNumber

  let passDetail =
        DPassDetails.PassDetails
          { id = passDetailId,
            name = req.name,
            studentClass = req.studentClass,
            guardianName = req.guardianName,
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
            graduationDate = req.graduationDate,
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

updatePassDetail :: PassDetailsAPI.PassDetailsUpdateReq -> DPerson.Person -> DPassDetails.PassDetails -> Environment.Flow ()
updatePassDetail req person passDetail = do
  encAadharNo <- mapM encrypt req.aadharNo
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
            DPassDetails.studentClass = req.studentClass <|> passDetail.studentClass,
            DPassDetails.aadharNo = encAadharNo <|> passDetail.aadharNo,
            DPassDetails.routePairs = routePairs,
            DPassDetails.applicableRouteIds = applicableRouteIds,
            DPassDetails.numberOfStages = numberOfStages,
            DPassDetails.graduationDate = req.graduationDate <|> passDetail.graduationDate,
            DPassDetails.verificationStatus = DPassDetails.PENDING,
            DPassDetails.remark = Nothing
          }
  QPassDetails.updateByPrimaryKey updatedPassDetails

-- | Look up route IDs, build RoutePairs (with LatLong), and total stage count from request route details.
-- Dedups input by (srcStopId, destStopId) so a duplicate submission doesn't double-count.
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
      (allRouteIds, allRoutePairs, totalStages) <- foldM (processOneRouteDetail integratedBPPConfig) ([], [], 0) dedupedRouteDetails
      pure (Just (L.nub allRouteIds), allRoutePairs, Just totalStages)

processOneRouteDetail ::
  DIBC.IntegratedBPPConfig ->
  ([Kernel.Prelude.Text], [DPassDetails.RoutePair], Int) ->
  PassDetailsAPI.RouteDetails ->
  Environment.Flow ([Kernel.Prelude.Text], [DPassDetails.RoutePair], Int)
processOneRouteDetail integratedBPPConfig (accRouteIds, accRoutePairs, accStages) rd = do
  let srcStopCode = rd.srcStopId
      destStopCode = rd.destStopId
  -- Primary: cached station lookup (route-independent)
  mbSrcStationLatLong <- tryStationLatLong integratedBPPConfig srcStopCode
  mbDestStationLatLong <- tryStationLatLong integratedBPPConfig destStopCode
  forwardRoutes <- JMUtils.getRouteCodesFromTo srcStopCode destStopCode integratedBPPConfig
  reverseRoutes <- JMUtils.getRouteCodesFromTo destStopCode srcStopCode integratedBPPConfig
  let routeIds = L.nub $ forwardRoutes <> reverseRoutes
  -- Trip lookup is needed for stage calc; reused as fallback for any missing stop coords.
  (srcLatLong, destLatLong, stages) <- case routeIds of
    [] -> Utils.throwError $ InvalidRequest ("No routes found between stops " <> srcStopCode <> " and " <> destStopCode)
    (routeCode : _) -> do
      trip <- OTPRest.getExampleTrip integratedBPPConfig routeCode >>= fromMaybeM (InvalidRequest "Could not fetch trip details for route")
      srcTripStop <- OTPRest.findTripStopByStopCode trip srcStopCode & fromMaybeM (InvalidRequest ("Source stop not found in trip: " <> srcStopCode))
      destTripStop <- OTPRest.findTripStopByStopCode trip destStopCode & fromMaybeM (InvalidRequest ("Destination stop not found in trip: " <> destStopCode))
      stages <- case (OTPRest.extractStageFromTripStop srcTripStop, OTPRest.extractStageFromTripStop destTripStop) of
        (Just s, Just d) -> pure (abs (d - s))
        _ -> Utils.throwError $ InvalidRequest "Stage Calculation Failed"
      let srcLL = fromMaybe (LatLong {lat = srcTripStop.lat, lon = srcTripStop.lon}) mbSrcStationLatLong
          destLL = fromMaybe (LatLong {lat = destTripStop.lat, lon = destTripStop.lon}) mbDestStationLatLong
      pure (srcLL, destLL, stages)
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
  pure (accRouteIds <> routeIds, accRoutePairs <> [forwardPair, reversePair], accStages + stages)

-- | Try fetching a stop's lat/long via cached OTPRest station lookup.
-- Returns Nothing if the stop is unknown or missing coordinates so the caller can fall back.
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
  pure $ mkPassDetailResp passDetail org

mkPassDetailResp :: DPassDetails.PassDetails -> DPassOrganization.PassOrganization -> PassDetailsAPI.PassDetailsDataResp
mkPassDetailResp passDetail org =
  PassDetailsAPI.PassDetailsDataResp
    { PassDetailsAPI.passDetailsId = passDetail.id,
      PassDetailsAPI.name = passDetail.name,
      PassDetailsAPI.age = passDetail.age,
      PassDetailsAPI.gender = passDetail.gender,
      PassDetailsAPI.guardianName = passDetail.guardianName,
      PassDetailsAPI.address = passDetail.address,
      PassDetailsAPI.pincode = passDetail.pincode,
      PassDetailsAPI.studentClass = passDetail.studentClass,
      PassDetailsAPI.idCardPicture = passDetail.idCardPicture,
      PassDetailsAPI.selfImage = passDetail.selfImage,
      PassDetailsAPI.routePairs = passDetail.routePairs,
      PassDetailsAPI.passOrganizationId = org.id,
      PassDetailsAPI.passOrganizationName = org.name,
      PassDetailsAPI.registerNo = passDetail.registerNo,
      PassDetailsAPI.graduationDate = passDetail.graduationDate,
      PassDetailsAPI.verificationStatus = passDetail.verificationStatus,
      PassDetailsAPI.validTill = passDetail.validTill,
      PassDetailsAPI.remark = passDetail.remark
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
mkPassStatusResp passDetail =
  PassDetailsAPI.PassStatusResp
    { PassDetailsAPI.verificationStatus = passDetail.verificationStatus,
      PassDetailsAPI.validTill = passDetail.validTill,
      PassDetailsAPI.remark = passDetail.remark
    }
