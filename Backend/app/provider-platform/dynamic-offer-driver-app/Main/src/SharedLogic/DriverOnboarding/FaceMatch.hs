module SharedLogic.DriverOnboarding.FaceMatch
  ( compareProfilePhotoAgainstDocs,
    compareDocAgainstProfilePhoto,
  )
where

import AWS.S3 as S3
import qualified Data.Text as T
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.Image as DImage
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Prelude
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import qualified Storage.Queries.AadhaarCard as QAadhaar
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.DriverPanCard as QPan
import qualified Storage.Queries.Image as QImage
import Tools.Error (DriverOnboardingError (..))
import qualified Tools.Verification as Verification

compareProfilePhotoAgainstDocs ::
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id Person.Person ->
  Maybe Double ->
  Text ->
  Flow ()
compareProfilePhotoAgainstDocs merchantId merchantOpCityId personId mbScoreThreshold profilePhotoBase64 = do
  mbDL <- QDL.findByDriverId personId
  mbPan <- QPan.findByDriverId personId
  mbAadhaar <- QAadhaar.findByPrimaryKey personId
  forM_ mbDL $ \dl ->
    when (dl.verificationStatus == Documents.VALID) $
      void $
        withTryCatch "faceMatch:DL" $
          compareFaceAndReject merchantId merchantOpCityId personId mbScoreThreshold profilePhotoBase64 dl.documentImageId1 DVC.DriverLicense
  forM_ mbPan $ \pan ->
    when (pan.verificationStatus == Documents.VALID) $
      void $
        withTryCatch "faceMatch:PAN" $
          compareFaceAndReject merchantId merchantOpCityId personId mbScoreThreshold profilePhotoBase64 pan.documentImageId1 DVC.PanCard
  forM_ mbAadhaar $ \aadhaar ->
    when (aadhaar.verificationStatus == Documents.VALID) $
      forM_ aadhaar.aadhaarFrontImageId $ \frontImgId ->
        void $
          withTryCatch "faceMatch:Aadhaar" $
            compareFaceAndReject merchantId merchantOpCityId personId mbScoreThreshold profilePhotoBase64 frontImgId DVC.AadhaarCard

compareDocAgainstProfilePhoto ::
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id Person.Person ->
  Maybe Double ->
  Id DImage.Image ->
  Flow Bool
compareDocAgainstProfilePhoto merchantId merchantOpCityId personId mbScoreThreshold docImageId = do
  mbProfilePhoto <- QImage.findByPersonIdImageTypeAndValidationStatus personId DVC.ProfilePhoto DImage.APPROVED
  case mbProfilePhoto of
    Nothing -> pure True
    Just profilePhoto -> do
      profilePhotoBase64 <- S3.get $ T.unpack profilePhoto.s3Path
      if T.null profilePhotoBase64
        then pure True
        else do
          mbDocBase64 <- fetchImageBase64 docImageId
          case mbDocBase64 of
            Nothing -> pure True
            Just docBase64 -> do
              result <-
                withTryCatch "faceMatch:compareDocAgainstProfilePhoto" $
                  callFaceCompare merchantId merchantOpCityId personId mbScoreThreshold profilePhotoBase64 docBase64
              case result of
                Right val -> pure val
                Left err -> throwError $ InternalError $ "Face comparison service failed: " <> show err

compareFaceAndReject ::
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id Person.Person ->
  Maybe Double ->
  Text ->
  Id DImage.Image ->
  DVC.DocumentType ->
  Flow ()
compareFaceAndReject merchantId merchantOpCityId personId mbScoreThreshold profilePhotoBase64 docImageId docType = do
  mbDocBase64 <- fetchImageBase64 docImageId
  forM_ mbDocBase64 $ \docBase64 -> do
    isMatch <- callFaceCompare merchantId merchantOpCityId personId mbScoreThreshold profilePhotoBase64 docBase64
    unless isMatch $ rejectDocument personId docType docImageId

callFaceCompare ::
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id Person.Person ->
  Maybe Double ->
  Text ->
  Text ->
  Flow Bool
callFaceCompare merchantId merchantOpCityId personId mbScoreThreshold image1 image2 = do
  resp <-
    Verification.faceCompare
      merchantId
      merchantOpCityId
      Verification.FaceCompareReq
        { selfieImage = image1,
          documentImage = image2,
          driverId = personId.getId
        }
  let isApiMatch = fromMaybe True (resp.faceComparedData >>= (.is_a_match))
      matchScore = resp.faceComparedData >>= (.match_score)
      isScoreAboveThreshold = case (mbScoreThreshold, matchScore) of
        (Just threshold, Just score) -> score >= threshold
        (Just _, Nothing) -> False
        _ -> True
  pure $ isApiMatch || isScoreAboveThreshold

rejectDocument ::
  Id Person.Person ->
  DVC.DocumentType ->
  Id DImage.Image ->
  Flow ()
rejectDocument personId docType imageId = do
  logInfo $ "Face mismatch detected for driver " <> personId.getId <> " on " <> show docType <> ", rejecting document"
  QImage.updateVerificationStatusAndFailureReason Documents.INVALID (ImageNotValid "Face mismatch with profile photo") imageId
  case docType of
    DVC.DriverLicense -> QDL.updateVerificationStatus Documents.INVALID imageId
    DVC.PanCard -> QPan.updateVerificationStatus Documents.INVALID personId
    DVC.AadhaarCard -> QAadhaar.updateVerificationStatus Documents.INVALID personId
    _ -> pure ()
  void $
    withTryCatch
      "faceMatch:refreshDocsVerificationStatuses"
      (void $ SStatus.processStatusEvent Nothing Nothing (SStatus.PersonDocChangedEvent personId))

fetchImageBase64 ::
  Id DImage.Image ->
  Flow (Maybe Text)
fetchImageBase64 imageId = do
  mbImage <- QImage.findById imageId
  case mbImage of
    Nothing -> pure Nothing
    Just img -> do
      content <- S3.get $ T.unpack img.s3Path
      pure $ if T.null content then Nothing else Just content
