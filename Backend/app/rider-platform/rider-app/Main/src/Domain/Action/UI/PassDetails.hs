{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.PassDetails
  ( getGetOrganizations,
    postPassDetailsUpdate,
    getPassDetailsData,
    getPassDetailsVerificationStatus,
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
import qualified Kernel.Prelude
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
    Just passDetail -> updatePassDetail req person passEnum passDetail
  pure APISuccess.Success

createPassDetail :: PassDetailsAPI.PassDetailsUpdateReq -> DPerson.Person -> DPassType.PassEnum -> Environment.Flow ()
createPassDetail req person passEnum = do
  passDetailId <- generateGUID
  now <- getCurrentTime
  let verificationDate = Data.Time.addUTCTime (730 * Data.Time.nominalDay) now

  -- Calculate applicable route IDs, route pairs, and number of stages based on src and dest stop codes
  (applicableRouteIds, routePairs, numberOfStages) <- case (req.srcStopId, req.destStopId) of
    (Just srcStopCode, Just destStopCode) -> do
      routeIds <- calculateApplicableRouteIds person.merchantOperatingCityId srcStopCode destStopCode
      stages <- calculateNumberOfStages person.merchantOperatingCityId routeIds srcStopCode destStopCode
      let routePair =
            DPassDetails.RoutePair
              { srcStopName = fromMaybe srcStopCode req.srcStopName,
                destStopName = fromMaybe destStopCode req.destStopName,
                srcStageName = srcStopCode,
                destStageName = destStopCode
              }
      pure (Just routeIds, [routePair], stages)
    _ -> pure (Nothing, [], Nothing)

  let passDetail =
        DPassDetails.PassDetails
          { id = passDetailId,
            name = req.name,
            studentClass = req.studentClass,
            guardianName = req.guardianName,
            age = req.age,
            idCardPicture = req.idCardPicture,
            address = req.address,
            routePairs = routePairs,
            registerNo = req.registerNo,
            personId = person.id,
            passEnum = passEnum,
            passOrganizationId = req.passOrganizationId,
            verificationStatus = req.verificationStatus,
            verificationDate = Just verificationDate,
            remark = Nothing,
            graduationDate = req.graduationDate,
            numberOfStages = numberOfStages,
            applicableRouteIds = applicableRouteIds,
            createdAt = now,
            updatedAt = now,
            merchantId = Just person.merchantId,
            merchantOperatingCityId = Just person.merchantOperatingCityId
          }
  QPassDetails.create passDetail
  unless (fromMaybe False person.isOrganizationPassHolder) $ do
    QPerson.updateIsOrganizationPassHolder (Just True) person.id

updatePassDetail :: PassDetailsAPI.PassDetailsUpdateReq -> DPerson.Person -> DPassType.PassEnum -> DPassDetails.PassDetails -> Environment.Flow ()
updatePassDetail req person _passEnum passDetail = do
  -- Calculate applicable route IDs, route pairs, and number of stages based on src and dest stop codes
  (applicableRouteIds, routePairs, numberOfStages) <- case (req.srcStopId, req.destStopId) of
    (Just srcStopCode, Just destStopCode) -> do
      routeIds <- calculateApplicableRouteIds person.merchantOperatingCityId srcStopCode destStopCode
      stages <- calculateNumberOfStages person.merchantOperatingCityId routeIds srcStopCode destStopCode
      let routePair =
            DPassDetails.RoutePair
              { srcStopName = fromMaybe srcStopCode req.srcStopName,
                destStopName = fromMaybe destStopCode req.destStopName,
                srcStageName = srcStopCode,
                destStageName = destStopCode
              }
      pure (Just routeIds, [routePair], stages)
    _ -> pure (passDetail.applicableRouteIds, passDetail.routePairs, passDetail.numberOfStages)

  let updatedPassDetails =
        passDetail
          { DPassDetails.name = req.name,
            DPassDetails.passOrganizationId = req.passOrganizationId,
            DPassDetails.idCardPicture = req.idCardPicture <|> passDetail.idCardPicture,
            DPassDetails.address = req.address <|> passDetail.address,
            DPassDetails.age = req.age <|> passDetail.age,
            DPassDetails.guardianName = req.guardianName <|> passDetail.guardianName,
            DPassDetails.studentClass = req.studentClass <|> passDetail.studentClass,
            DPassDetails.routePairs = routePairs,
            DPassDetails.applicableRouteIds = applicableRouteIds,
            DPassDetails.numberOfStages = numberOfStages,
            DPassDetails.graduationDate = req.graduationDate <|> passDetail.graduationDate,
            DPassDetails.verificationStatus = req.verificationStatus
          }
  QPassDetails.updateByPrimaryKey updatedPassDetails

-- | Calculate applicable route IDs by getting routes bidirectionally (forward + reverse)
calculateApplicableRouteIds ::
  Id.Id DMOC.MerchantOperatingCity ->
  Kernel.Prelude.Text ->
  Kernel.Prelude.Text ->
  Environment.Flow [Kernel.Prelude.Text]
calculateApplicableRouteIds merchantOperatingCityId srcStopCode destStopCode = do
  integratedBPPConfigs <- SIBC.findAllIntegratedBPPConfig merchantOperatingCityId Enums.BUS DIBC.MULTIMODAL
  case Kernel.Prelude.listToMaybe integratedBPPConfigs of
    Nothing -> pure []
    Just integratedBPPConfig -> do
      -- Get routes in forward direction (src -> dest)
      forwardRoutes <- JMUtils.getRouteCodesFromTo srcStopCode destStopCode integratedBPPConfig
      -- Get routes in reverse direction (dest -> src)
      reverseRoutes <- JMUtils.getRouteCodesFromTo destStopCode srcStopCode integratedBPPConfig
      -- Combine and deduplicate
      pure $ L.nub $ forwardRoutes <> reverseRoutes

-- | Calculate the number of stages between two stops using fare stage numbers
calculateNumberOfStages ::
  Id.Id DMOC.MerchantOperatingCity ->
  [Kernel.Prelude.Text] ->
  Kernel.Prelude.Text ->
  Kernel.Prelude.Text ->
  Environment.Flow (Maybe Int)
calculateNumberOfStages merchantOperatingCityId routeIds srcStopCode destStopCode = do
  case routeIds of
    [] -> pure Nothing
    (routeCode : _) -> do
      integratedBPPConfigs <- SIBC.findAllIntegratedBPPConfig merchantOperatingCityId Enums.BUS DIBC.MULTIMODAL
      case Kernel.Prelude.listToMaybe integratedBPPConfigs of
        Nothing -> pure Nothing
        Just integratedBPPConfig -> do
          tripDetails <- OTPRest.getExampleTrip integratedBPPConfig routeCode
          case tripDetails of
            Nothing -> pure Nothing
            Just trip -> do
              let startStop = OTPRest.findTripStopByStopCode trip srcStopCode
                  endStop = OTPRest.findTripStopByStopCode trip destStopCode
              case (startStop, endStop) of
                (Just startTripStop, Just endTripStop) -> do
                  let startStage = OTPRest.extractStageFromTripStop startTripStop
                      endStage = OTPRest.extractStageFromTripStop endTripStop
                  case (startStage, endStage) of
                    (Just startStageNum, Just endStageNum) ->
                      pure $ Just $ abs (endStageNum - startStageNum)
                    _ -> pure Nothing
                _ -> pure Nothing

getPassDetailsData ::
  ( ( Kernel.Prelude.Maybe (Id.Id DPerson.Person),
      Id.Id DMerchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow PassDetailsAPI.PassDetailsDataResp
  )
getPassDetailsData (mbPersonId, _merchatId) passEnumText = do
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
      PassDetailsAPI.guardianName = passDetail.guardianName,
      PassDetailsAPI.address = passDetail.address,
      PassDetailsAPI.studentClass = passDetail.studentClass,
      PassDetailsAPI.idCardPicture = passDetail.idCardPicture,
      PassDetailsAPI.routePairs = passDetail.routePairs,
      PassDetailsAPI.passOrganizationId = org.id,
      PassDetailsAPI.passOrganizationName = org.name,
      PassDetailsAPI.registerNo = passDetail.registerNo,
      PassDetailsAPI.graduationDate = passDetail.graduationDate,
      PassDetailsAPI.verificationStatus = passDetail.verificationStatus
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
      PassDetailsAPI.verificationDate = passDetail.verificationDate,
      PassDetailsAPI.remark = passDetail.remark
    }
