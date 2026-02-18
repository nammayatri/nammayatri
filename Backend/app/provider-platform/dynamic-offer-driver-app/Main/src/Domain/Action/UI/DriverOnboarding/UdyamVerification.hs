{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is distributed under the terms of the GNU Affero General Public License.
-}

module Domain.Action.UI.DriverOnboarding.UdyamVerification
  ( DriverUdyamReq (..),
    DriverUdyamRes,
    verifyUdyam,
    verifyUdyamFlow,
    onVerifyUdyam,
  )
where

import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Action.Dashboard.Fleet.RegistrationV2 as DFR
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DVRC
import qualified Domain.Types.DocumentVerificationConfig as ODC
import qualified Domain.Types.DriverUdyam as DUdyam
import qualified Domain.Types.IdfyVerification as DIdfy
import qualified Domain.Types.Image as Image
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Environment
import Kernel.External.Encryption
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import SharedLogic.DriverOnboarding (VerificationReqRecord)
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverUdyam as DUQuery
import qualified Storage.Queries.DriverUdyamExtra as DUQueryExtra
import qualified Storage.Queries.IdfyVerification as IVQuery
import qualified Storage.Queries.Image as IQuery
import qualified Storage.Queries.Person as PersonQuery
import Tools.Error
import qualified Tools.Verification as Verification
import Utils.Common.Cac.KeyNameConstants

data DriverUdyamReq = DriverUdyamReq
  { uamNumber :: Text,
    imageId1 :: Id Image.Image
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

type DriverUdyamRes = APISuccess

makeVerifyUdyamHitsCountKey :: Text -> Text
makeVerifyUdyamHitsCountKey uamNumber = "VerifyUdyam:uamNumberHits:" <> uamNumber <> ":hitsCount"

verifyUdyam ::
  (Id Person.Person, Id DMOC.MerchantOperatingCity) ->
  DriverUdyamReq ->
  Flow Bool
verifyUdyam (personId, merchantOpCityId) req = do
  externalServiceRateLimitOptions <- asks (.externalServiceRateLimitOptions)
  checkSlidingWindowLimitWithOptions (makeVerifyUdyamHitsCountKey req.uamNumber) externalServiceRateLimitOptions

  person <- PersonQuery.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  (blocked, _driverDocument) <- DVRC.getDriverDocumentInfo person
  when blocked $ throwError AccountBlocked
  transporterConfig <- SCTC.findByMerchantOpCityId person.merchantOperatingCityId (Just (DriverId (cast person.id))) >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
  case transporterConfig.allowDuplicateUdyam of
    Just False -> do
      udyamHash <- getDbHash req.uamNumber
      udyamInfoList <- DUQueryExtra.findAllByEncryptedUdyamNumber udyamHash
      when (length udyamInfoList > 1) $ throwError UdyamAlreadyLinked
      udyamPersonDetails <- PersonQuery.getDriversByIdIn (map (.driverId) udyamInfoList)
      let getRoles = map (.role) udyamPersonDetails
      when (person.role `elem` getRoles) $ throwError UdyamAlreadyLinked
    _ -> pure ()
  documentVerificationConfig <-
    CQDVC.findByMerchantOpCityIdAndDocumentType merchantOpCityId ODC.UDYAMCertificate Nothing
      >>= fromMaybeM (DocumentVerificationConfigNotFound merchantOpCityId.getId (show ODC.UDYAMCertificate))
        . listToMaybe
  verifyUdyamFlow person merchantOpCityId documentVerificationConfig req.uamNumber req.imageId1
  res <- case person.role of
    Person.DRIVER -> do
      now <- getCurrentTime
      fork "enabling driver if all the mandatory document is verified" $ do
        merchantOpCity <-
          CQMOC.findById merchantOpCityId
            >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
        driverImages <- IQuery.findAllByPersonId transporterConfig personId
        let driverImagesInfo = IQuery.DriverImagesInfo {driverId = personId, merchantOperatingCity = merchantOpCity, driverImages, transporterConfig, now}
        let onlyMandatoryDocs = Just True
            shouldActivateRc = False
        void $ SStatus.statusHandler' (Just person) driverImagesInfo Nothing Nothing Nothing Nothing (Just True) shouldActivateRc onlyMandatoryDocs
      pure False
    role
      | DCommon.checkFleetOwnerRole role ->
        DFR.enableFleetIfPossible person.id Nothing (DFR.castRoleToFleetType person.role) person.merchantOperatingCityId
    _ -> pure False
  pure res

verifyUdyamFlow :: Person.Person -> Id DMOC.MerchantOperatingCity -> ODC.DocumentVerificationConfig -> Text -> Id Image.Image -> Flow ()
verifyUdyamFlow person merchantOpCityId _documentVerificationConfig uamNumber imageId1 = do
  now <- getCurrentTime
  encryptedUam <- encrypt uamNumber
  let imageExtractionValidation = DIdfy.Skipped
  verifyRes <-
    Verification.verifyUdyamAadhaarAsync person.merchantId merchantOpCityId $
      Verification.VerifyUdyamAadhaarAsyncReq {uamNumber, driverId = person.id.getId}
  case verifyRes.requestor of
    VT.Idfy -> IVQuery.create =<< mkIdfyVerificationEntityUdyam person imageId1 verifyRes.requestId now imageExtractionValidation encryptedUam
    _ -> throwError $ InternalError ("Service provider not configured to return Udyam Aadhaar verification async responses. Provider Name : " <> show verifyRes.requestor)
  pure ()

onVerifyUdyam :: VerificationReqRecord -> VT.UdyamAadhaarVerificationResponse -> VT.VerificationService -> Flow AckResponse
onVerifyUdyam verificationReq output serviceName = do
  case serviceName of
    VT.Idfy -> do
      person <- PersonQuery.findById verificationReq.driverId >>= fromMaybeM (PersonNotFound verificationReq.driverId.getId)
      mDriverUdyam <- DUQuery.findByDriverId person.id
      let verificationStatus = Documents.VALID
      case mDriverUdyam of
        Just driverUdyam -> do
          let updated =
                driverUdyam
                  { DUdyam.verificationStatus = verificationStatus,
                    DUdyam.enterpriseName = output.enterpriseName,
                    DUdyam.enterpriseType = output.enterpriseType
                  }
          DUQuery.updateByPrimaryKey updated
        Nothing -> do
          udyamCardDetails <- buildDriverUdyamCard person verificationReq.documentNumber output.enterpriseName output.enterpriseType
          DUQuery.create udyamCardDetails
      case person.role of
        role
          | DCommon.checkFleetOwnerRole role ->
            void $ DFR.enableFleetIfPossible person.id Nothing (DFR.castRoleToFleetType person.role) person.merchantOperatingCityId
        _ -> pure ()
    _ -> throwError $ InternalError ("Unknown Service provider webhook encountered in onVerifyUdyam. Name of provider : " <> show serviceName)
  pure Ack

buildDriverUdyamCard :: Person.Person -> EncryptedHashedField 'AsEncrypted Text -> Maybe Text -> Maybe Text -> Flow DUdyam.DriverUdyam
buildDriverUdyamCard person encryptedUdyamNumber enterpriseName enterpriseType = do
  now <- getCurrentTime
  uuid <- generateGUID
  return $
    DUdyam.DriverUdyam
      { driverId = person.id,
        id = Id uuid :: Id DUdyam.DriverUdyam,
        udyamNumber = encryptedUdyamNumber,
        verificationStatus = Documents.VALID,
        verifiedBy = Nothing,
        merchantId = Just person.merchantId,
        merchantOperatingCityId = Just person.merchantOperatingCityId,
        enterpriseName = enterpriseName,
        enterpriseType = enterpriseType,
        createdAt = now,
        updatedAt = now
      }

mkIdfyVerificationEntityUdyam :: MonadFlow m => Person.Person -> Id Image.Image -> Text -> UTCTime -> DIdfy.ImageExtractionValidation -> EncryptedHashedField 'AsEncrypted Text -> m DIdfy.IdfyVerification
mkIdfyVerificationEntityUdyam person imageId1 requestId now imageExtractionValidation encryptedUam = do
  entityId <- generateGUID
  return $
    DIdfy.IdfyVerification
      { id = Id entityId,
        driverId = person.id,
        documentImageId1 = imageId1,
        documentImageId2 = Nothing,
        requestId,
        docType = ODC.UDYAMCertificate,
        documentNumber = encryptedUam,
        driverDateOfBirth = Nothing,
        imageExtractionValidation = imageExtractionValidation,
        issueDateOnDoc = Nothing,
        status = "pending",
        idfyResponse = Nothing,
        vehicleCategory = Nothing,
        airConditioned = Nothing,
        oxygen = Nothing,
        ventilator = Nothing,
        retryCount = Just 0,
        nameOnCard = Nothing,
        merchantId = Just person.merchantId,
        merchantOperatingCityId = Just person.merchantOperatingCityId,
        createdAt = now,
        updatedAt = now
      }
