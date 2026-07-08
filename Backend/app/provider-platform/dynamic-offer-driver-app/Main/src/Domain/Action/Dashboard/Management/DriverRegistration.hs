{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Management.DriverRegistration
  ( getDriverRegistrationDocumentsList,
    getDriverRegistrationGetDocument,
    postDriverRegistrationDocumentUpload,
    postDriverRegistrationDocumentsCommon,
    postDriverRegistrationRegisterDl,
    postDriverRegistrationRegisterRc,
    postDriverRegistrationRegisterGenerateAadhaarOtp,
    postDriverRegistrationRegisterVerifyAadhaarOtp,
    getDriverRegistrationUnderReviewDrivers,
    getDriverRegistrationDocumentsInfo,
    postDriverRegistrationDocumentsUpdate,
    postDriverRegistrationRegisterAadhaar,
    postDriverRegistrationUnlinkDocument,
    mapDocumentType,
    sendDocumentRejectionNotification,
    getDriverRegistrationVerificationStatus,
    postDriverRegistrationTriggerReminder,
    postDriverRegistrationVerifyBankAccount,
    getDriverRegistrationInfoBankAccount,
    getDriverRegistrationPayoutRegistration,
    getDriverRegistrationPayoutRegistrationWithActor,
    getDriverRegistrationPayoutOrderStatus,
    postDriverRegistrationDeleteBankAccount,
    getDriverRegistrationDocumentsCommonList,
    getDriverRegistrationCommonDocumentsList,
  )
where

import qualified API.Types.ProviderPlatform.Management.Account as Common
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverRegistration as Common
import qualified API.Types.UI.DriverOnboardingV2
import qualified Dashboard.Common
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as DAK
import qualified Data.Aeson.KeyMap as DAKM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.List (nub, (\\))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (utctDay)
import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Action.Dashboard.Fleet.RegistrationV2 as DRegistrationV2
import qualified Domain.Action.Dashboard.Management.Driver as DDriver
import qualified Domain.Action.UI.DriverOnboarding.AadhaarVerification as AV
import qualified Domain.Action.UI.DriverOnboarding.BankAccountVerification as BankAccountVerification
import Domain.Action.UI.DriverOnboarding.DriverLicense
import Domain.Action.UI.DriverOnboarding.Image
import qualified Domain.Action.UI.DriverOnboarding.Status as DStatus
import Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate
import qualified Domain.Action.UI.DriverOnboardingV2 as DOV
import qualified Domain.Action.UI.ReferralPayout as ReferralPayout
import qualified Domain.Types.AadhaarCard as DAadhaar
import qualified Domain.Types.BusinessLicense as DBL
import qualified Domain.Types.CommonDriverOnboardingDocuments as DCommonDoc
import qualified Domain.Types.DocsVerificationStatus as DDVS
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DriverGstin as DGstin
import qualified Domain.Types.DriverIdentityInfo as DII
import qualified Domain.Types.DriverLicense as DDL
import qualified Domain.Types.DriverPanCard as DPan
import qualified Domain.Types.DriverRCAssociation as DRCA
import qualified Domain.Types.DriverUdyam as DUdyam
import qualified Domain.Types.FleetOwnerInformation as DFOI
import qualified Domain.Types.Image as DImage
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantMessage as DMM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.OperationHubRequests as DOHR
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Plan as DPlan
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DVCat
import qualified Domain.Types.VehicleFitnessCertificate as DFC
import qualified Domain.Types.VehicleInsurance as DVI
import qualified Domain.Types.VehicleNOC as DNOC
import qualified Domain.Types.VehiclePUC as DPUC
import qualified Domain.Types.VehiclePermit as DVPermit
import qualified Domain.Types.VehicleRegistrationCertificate as DRC
import Environment
import EulerHS.Prelude hiding (elem, find, foldl', map, whenJust)
import Kernel.Beam.Functions
import Kernel.External.AadhaarVerification.Interface.Types
import Kernel.External.Encryption (decrypt, encrypt, getDbHash, hash)
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.External.Payout.Interface.Types as PayoutTypes
import Kernel.External.Types (ServiceFlow)
import qualified Kernel.External.Types as Lang
import Kernel.External.Verification.Interface.Types
import qualified Kernel.External.Verification.Types as VerificationTypes
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Documents (VerificationStatus (..))
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransaction as QIndirectTaxExtra
import qualified Lib.Finance.Storage.Queries.Invoice as QFinanceInvoice
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import SharedLogic.Analytics as Analytics
import qualified SharedLogic.DriverOnboarding as SDO
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import SharedLogic.Merchant (findMerchantByShortId)
import SharedLogic.Reminder.Helper (createReminder)
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.FleetOwnerDocumentVerificationConfig as CQFODVC
import qualified Storage.CachedQueries.Merchant.MerchantMessage as QMM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import Storage.ConfigPilot.Config.DocumentVerificationConfig (DocumentVerificationConfigDimensions (..))
import Storage.ConfigPilot.Config.FleetOwnerDocumentVerificationConfig (FleetOwnerDocumentVerificationConfigDimensions (..))
import Storage.ConfigPilot.Config.Translation (TranslationDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.BusinessLicense as QBL
import qualified Storage.Queries.BusinessLicenseExtra as QBLExtra
import qualified Storage.Queries.CommonDriverOnboardingDocuments as QCommonDriverOnboardingDocuments
import qualified Storage.Queries.CommonDriverOnboardingDocumentsExtra as QCommonDriverOnboardingDocumentsExtra
import qualified Storage.Queries.DriverGstin as QGstin
import qualified Storage.Queries.DriverIdentityInfo as QDII
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.DriverPanCard as QPan
import qualified Storage.Queries.DriverRCAssociation as QRCAssoc
import qualified Storage.Queries.DriverSSN as QSSN
import qualified Storage.Queries.DriverUdyam as QUdyam
import qualified Storage.Queries.DriverUdyamExtra as QUdyamExtra
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.FleetOwnerInformationExtra as QFOIE
import qualified Storage.Queries.HyperVergeVerificationExtra as HVQuery
import qualified Storage.Queries.IdfyVerificationExtra as IDQuery
import Storage.Queries.Image as QImage
import qualified Storage.Queries.OperationHubRequests as QOHR
import qualified Storage.Queries.Person as QDriver
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Translations as QTranslations
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleFitnessCertificate as QFC
import qualified Storage.Queries.VehicleInsurance as QVI
import qualified Storage.Queries.VehicleNOC as QVNOC
import qualified Storage.Queries.VehiclePUC as QVPUC
import qualified Storage.Queries.VehiclePermit as QVPermit
import qualified Storage.Queries.VehicleRegistrationCertificate as QRC
import qualified Tools.AadhaarVerification as AadhaarVerification
import qualified Tools.ActorInfo as ActorInfo
import Tools.Error
import Tools.Notifications as Notify
import qualified Tools.Payment as TPayment
import qualified Tools.SMS as Sms

-- TDS Certificate validation (kept in sync with UI DriverOnboardingV2 flow)

parseTDSCertificateData :: Text -> Flow API.Types.UI.DriverOnboardingV2.TDSCertificateData
parseTDSCertificateData jsonText =
  case A.eitherDecode (BSL.fromStrict $ TE.encodeUtf8 jsonText) of
    Left err -> throwError $ InvalidRequest $ "Invalid TDS certificate data JSON: " <> show err
    Right data_ -> pure data_

validateTDSCertificate :: API.Types.UI.DriverOnboardingV2.TDSCertificateData -> Flow [API.Types.UI.DriverOnboardingV2.TDSCertificateValidationError]
validateTDSCertificate tdsData = do
  invoiceErrors <- fmap concat $ mapM validateInvoiceEntry tdsData.tdsCertificates

  let sumOfTds = Kernel.Prelude.sum $ map (.tdsAmountAgainstInvoice) tdsData.tdsCertificates
  let overallError =
        if sumOfTds == tdsData.overallTdsAmount
          then []
          else
            [ API.Types.UI.DriverOnboardingV2.TDSCertificateValidationError
                { field = "overallTdsAmount",
                  invoiceId = "N/A",
                  errorMessage = "Sum of invoice TDS amounts " <> show sumOfTds <> " does not match overall TDS amount " <> show tdsData.overallTdsAmount
                }
            ]

  pure $ overallError <> invoiceErrors

validateInvoiceEntry :: API.Types.UI.DriverOnboardingV2.TDSInvoiceEntry -> Flow [API.Types.UI.DriverOnboardingV2.TDSCertificateValidationError]
validateInvoiceEntry entry = do
  let mkError field errorMessage =
        API.Types.UI.DriverOnboardingV2.TDSCertificateValidationError
          { field,
            invoiceId = entry.invoiceId,
            errorMessage
          }

  let roundTo2 :: HighPrecMoney -> HighPrecMoney
      roundTo2 x = fromIntegral (round (x * 100) :: Integer) / 100

  mbInvoice <- QFinanceInvoice.findById (Id entry.invoiceId)
  let invoiceErrors =
        case mbInvoice of
          Nothing -> [mkError "invoiceId" "Invoice ID not found in records"]
          Just invoice ->
            catMaybes
              [ if utctDay invoice.issuedAt /= utctDay entry.invoiceDate
                  then Just (mkError "invoiceDate" "Invoice date does not match records")
                  else Nothing,
                if invoice.invoiceNumber /= entry.invoiceNumber
                  then Just (mkError "invoiceNumber" "Invoice number does not match records")
                  else Nothing,
                if roundTo2 invoice.totalAmount /= roundTo2 entry.invoiceValue
                  then Just (mkError "invoiceValue" "Invoice value does not match records")
                  else Nothing
              ]

  taxTxns <- QIndirectTaxExtra.findByInvoiceNumber (Just entry.invoiceNumber)
  let totalTaxableValue = Kernel.Prelude.sum $ map (.taxableValue) taxTxns
      totalGstAmount = Kernel.Prelude.sum $ map (.totalGstAmount) taxTxns

  let taxErrors =
        catMaybes
          [ if roundTo2 totalTaxableValue /= roundTo2 entry.baseValue
              then Just (mkError "baseValue" "Base value does not match records")
              else Nothing,
            if roundTo2 totalGstAmount /= roundTo2 entry.gst
              then Just (mkError "gst" "GST amount does not match records")
              else Nothing
          ]

  pure (invoiceErrors <> taxErrors)

extractInvoiceIds :: Text -> [Text]
extractInvoiceIds docData =
  case A.eitherDecode (BSL.fromStrict $ TE.encodeUtf8 docData) of
    Right tdsData -> map (.invoiceId) (tdsData :: API.Types.UI.DriverOnboardingV2.TDSCertificateData).tdsCertificates
    Left _ -> []

getDriverRegistrationDocumentsList :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Maybe Text -> Flow Common.DocumentsListResponse
getDriverRegistrationDocumentsList merchantShortId city driverId mbRcId = do
  merchant <- findMerchantByShortId merchantShortId
  odometerImg <- getVehicleImages merchant.id DVC.Odometer
  vehicleFrontImgs <- getVehicleImages merchant.id DVC.VehicleFront
  vehicleBackImgs <- getVehicleImages merchant.id DVC.VehicleBack
  vehicleRightImgs <- getVehicleImages merchant.id DVC.VehicleRight
  vehicleLeftImgs <- getVehicleImages merchant.id DVC.VehicleLeft
  vehicleFrontInteriorImgs <- getVehicleImages merchant.id DVC.VehicleFrontInterior
  vehicleBackInteriorImgs <- getVehicleImages merchant.id DVC.VehicleBackInterior
  pucImages <- getDriverImages merchant.id DVC.VehiclePUC
  permitImages <- getDriverImages merchant.id DVC.VehiclePermit
  dlImgs <- groupByTxnIdInHM <$> runInReplica (findImagesByPersonAndType Nothing Nothing merchant.id (cast driverId) DVC.DriverLicense)
  vInspectionImgs <- getDriverImages merchant.id DVC.VehicleInspectionForm
  vehRegImgs <- getDriverImages merchant.id DVC.VehicleRegistrationCertificate
  uploadProfImgs <- getDriverImages merchant.id DVC.UploadProfile
  vehicleFitnessCertImgs <- getDriverImages merchant.id DVC.VehicleFitnessCertificate
  vehicleInsImgs <- getDriverImages merchant.id DVC.VehicleInsurance
  profilePics <- getDriverImages merchant.id DVC.ProfilePhoto
  gstImgs <- getDriverImages merchant.id DVC.GSTCertificate
  udyamImgs <- getDriverImages merchant.id DVC.UDYAMCertificate
  panImgs <- getDriverImages merchant.id DVC.PanCard
  businessLicenseImgs <- getDriverImages merchant.id DVC.BusinessLicense
  aadhaarImgs <- getDriverImages merchant.id DVC.AadhaarCard
  vehicleNOCImgs <- getDriverImages merchant.id DVC.VehicleNOC
  driverVehicleNOCImgs <- getDriverImages merchant.id DVC.DriverVehicleNOC
  localResidenceProofImgs <- getDriverImages merchant.id DVC.LocalResidenceProof
  policeVerificationCertificateImgs <- getDriverImages merchant.id DVC.PoliceVerificationCertificate
  drivingSchoolCertificateImgs <- getDriverImages merchant.id DVC.DrivingSchoolCertificate
  medicalCertificateImgs <- getDriverImages merchant.id DVC.MedicalCertificate
  commonDocumentsData <- runInReplica (QCommonDriverOnboardingDocuments.findByDriverId (Just (cast driverId)))
  let commonDocuments = map toCommonDocumentItem commonDocumentsData
  allDlImgs <- runInReplica (QDL.findAllByImageId (map (Id) $ mapMaybe listToMaybe dlImgs))
  allRCImgs <- runInReplica (QRC.findAllByImageId (map (Id) vehRegImgs))
  allDLDetails <- mapM convertDLToDLDetails allDlImgs
  allRCDetails <- mapM convertRCToRCDetails allRCImgs
  ssnEntry <- QSSN.findByDriverId (cast driverId)
  ssnUnenc <- case ssnEntry of
    Just ssnRecord -> do
      ssnDecrypt <- decrypt ssnRecord.ssn
      return $ Just ssnDecrypt
    _ -> return Nothing
  pure
    Common.DocumentsListResponse
      { driverLicense = dlImgs,
        vehicleRegistrationCertificate = vehRegImgs,
        vehicleInsurance = vehicleInsImgs,
        uploadProfile = uploadProfImgs,
        ssn = ssnUnenc,
        vehicleFitnessCertificate = vehicleFitnessCertImgs,
        profilePhoto = profilePics,
        driverLicenseDetails = allDLDetails,
        vehicleRegistrationCertificateDetails = allRCDetails,
        vehicleInspectionForm = vInspectionImgs,
        vehiclePermit = permitImages,
        vehiclePUC = pucImages,
        vehicleFront = vehicleFrontImgs,
        vehicleBack = vehicleBackImgs,
        vehicleRight = vehicleRightImgs,
        vehicleLeft = vehicleLeftImgs,
        vehicleFrontInterior = vehicleFrontInteriorImgs,
        vehicleBackInterior = vehicleBackInteriorImgs,
        pan = panImgs,
        businessLicense = businessLicenseImgs,
        aadhaar = aadhaarImgs,
        vehicleNOC = vehicleNOCImgs,
        driverVehicleNOC = driverVehicleNOCImgs,
        odometer = odometerImg,
        gstCertificate = gstImgs,
        localResidenceProof = localResidenceProofImgs,
        policeVerificationCertificate = policeVerificationCertificateImgs,
        drivingSchoolCertificate = drivingSchoolCertificateImgs,
        udyamCertificate = udyamImgs,
        medicalCertificate = medicalCertificateImgs,
        commonDocuments = commonDocuments
      }
  where
    getVehicleImages merchantId imageType = case mbRcId of
      Just rcId -> map (.id.getId) <$> runInReplica (findImagesByRCAndType merchantId (Just rcId) imageType Nothing)
      Nothing -> pure []

    getDriverImages merchantId imageType = map (.id.getId) <$> runInReplica (findImagesByPersonAndType Nothing Nothing merchantId (cast driverId) imageType)

    groupByTxnIdInHM = handleNullTxnIds . foldl' (\acc img -> HM.insertWith (++) (fromMaybe "Nothing" img.workflowTransactionId) [img.id.getId] acc) (HM.empty :: HM.HashMap Text [Text])
    handleNullTxnIds hm = (maybe [] (map (: [])) $ HM.lookup "Nothing" hm) ++ (HM.elems $ HM.delete "Nothing" hm)
    convertDLToDLDetails dl = do
      driverLicenseNumberDec <- decrypt dl.licenseNumber
      pure $
        Common.DLDetails
          { driverName = dl.driverName,
            driverLicenseNumber = driverLicenseNumberDec,
            operatingCity = show city,
            driverDateOfBirth = dl.driverDob,
            classOfVehicles = dl.classOfVehicles,
            imageId1 = dl.documentImageId1.getId,
            imageId2 = getId <$> dl.documentImageId2,
            createdAt = dl.createdAt,
            dateOfIssue = dl.dateOfIssue
          }
    convertRCToRCDetails rc = do
      certificateNumberDec <- decrypt rc.certificateNumber
      pure $
        Common.RCDetails
          { vehicleRegistrationCertNumber = certificateNumberDec,
            imageId = rc.documentImageId.getId,
            operatingCity = show city,
            vehicleCategory = show <$> rc.userPassedVehicleCategory,
            airConditioned = rc.airConditioned,
            oxygen = rc.oxygen,
            ventilator = rc.ventilator,
            vehicleManufacturer = rc.vehicleManufacturer,
            vehicleModel = rc.vehicleModel,
            vehicleColor = rc.vehicleColor,
            vehicleDoors = rc.vehicleDoors,
            vehicleSeatBelts = rc.vehicleSeatBelts,
            createdAt = rc.createdAt,
            dateOfRegistration = rc.dateOfRegistration,
            vehicleModelYear = rc.vehicleModelYear,
            failedRules = rc.failedRules,
            verificationStatus = Just $ DCommon.castVerificationStatus rc.verificationStatus,
            permitExpiry = rc.permitExpiry
          }

    toCommonDocumentItem :: DCommonDoc.CommonDriverOnboardingDocuments -> Common.CommonDocumentItem
    toCommonDocumentItem doc =
      Common.CommonDocumentItem
        { documentId = cast doc.id,
          documentType = SDO.castDocumentType doc.documentType,
          documentData = doc.documentData,
          verificationStatus = DCommon.castVerificationStatus doc.verificationStatus,
          rejectReason = doc.rejectReason,
          documentImageId = getId <$> doc.documentImageId,
          createdAt = doc.createdAt,
          updatedAt = doc.updatedAt
        }

getDriverRegistrationCommonDocumentsList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe [Common.DocumentType] ->
  Maybe [Common.VerificationStatus] ->
  Maybe [Id Common.Driver] ->
  Maybe Text ->
  Maybe Text ->
  Flow Common.CommonDocumentsListRes
getDriverRegistrationCommonDocumentsList merchantShortId opCity mbLimit mbOffset mbFrom mbTo mbDocTypes mbVerStatuses mbDriverIds mbSortByField mbSortOrder = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let mbPersonIds = fmap (map (cast @Common.Driver @DP.Person)) mbDriverIds
      mbDomainDocTypes = fmap (map mapDocumentType) mbDocTypes
      convertStatus = \case
        Common.PENDING -> Documents.PENDING
        Common.VALID -> Documents.VALID
        Common.INVALID -> Documents.INVALID
        Common.MANUAL_VERIFICATION_REQUIRED -> Documents.MANUAL_VERIFICATION_REQUIRED
        Common.UNAUTHORIZED -> Documents.UNAUTHORIZED
      mbDomainVerStatuses = fmap (map convertStatus) mbVerStatuses
  let filters =
        QCommonDriverOnboardingDocumentsExtra.CommonDocumentsFilter
          { merchantId = merchant.id,
            merchantOperatingCityId = merchantOpCityId,
            driverIds = mbPersonIds,
            documentTypes = mbDomainDocTypes,
            verificationStatuses = mbDomainVerStatuses,
            from = mbFrom,
            to = mbTo,
            limit = mbLimit,
            offset = mbOffset,
            sortByField = mbSortByField,
            sortOrder = mbSortOrder
          }
  docs <- runInReplica $ QCommonDriverOnboardingDocumentsExtra.findAllForCommonDocuments filters
  let documents = map toCommonDocumentListItem docs
      summary =
        Common.Summary
          { totalCount = 20,
            count = Kernel.Prelude.length documents
          }
  pure
    Common.CommonDocumentsListRes
      { summary = summary,
        documents = documents
      }
  where
    toCommonDocumentListItem :: DCommonDoc.CommonDriverOnboardingDocuments -> Common.CommonDocumentListItem
    toCommonDocumentListItem doc =
      Common.CommonDocumentListItem
        { documentId = cast doc.id,
          driverId = (.getId) <$> doc.driverId,
          documentType = SDO.castDocumentType doc.documentType,
          documentData = doc.documentData,
          verificationStatus = DCommon.castVerificationStatus doc.verificationStatus,
          rejectReason = doc.rejectReason,
          documentImageId = (.getId) <$> doc.documentImageId,
          createdAt = doc.createdAt,
          updatedAt = doc.updatedAt
        }

-- Backwards-compatible name expected by generated API.Action module
getDriverRegistrationDocumentsCommonList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe [Common.DocumentType] ->
  Maybe [Common.VerificationStatus] ->
  Maybe [Id Common.Driver] ->
  Maybe Text ->
  Maybe Text ->
  Flow Common.CommonDocumentsListRes
getDriverRegistrationDocumentsCommonList =
  getDriverRegistrationCommonDocumentsList

getDriverRegistrationGetDocument :: ShortId DM.Merchant -> Context.City -> Id Common.Image -> Flow Common.GetDocumentResponse
getDriverRegistrationGetDocument merchantShortId _ imageId = do
  merchant <- findMerchantByShortId merchantShortId
  img <- getImage merchant.id (cast imageId)
  image <- QImage.findById (cast imageId) >>= fromMaybeM (InternalError "Image not found by image id")
  pure Common.GetDocumentResponse {imageBase64 = img, status = castVerificationStatus <$> image.verificationStatus, createdAt = image.createdAt}
  where
    castVerificationStatus :: VerificationStatus -> Common.VerificationStatus
    castVerificationStatus = \case
      PENDING -> Common.PENDING
      VALID -> Common.VALID
      INVALID -> Common.INVALID
      MANUAL_VERIFICATION_REQUIRED -> Common.MANUAL_VERIFICATION_REQUIRED
      UNAUTHORIZED -> Common.UNAUTHORIZED
      PULL_REQUIRED -> Common.PENDING

mapDocumentType :: Common.DocumentType -> DVC.DocumentType
mapDocumentType Common.DriverLicense = DVC.DriverLicense
mapDocumentType Common.BankAccount = DVC.BankingDetails
mapDocumentType Common.VehicleRegistrationCertificate = DVC.VehicleRegistrationCertificate
mapDocumentType Common.VehiclePUCImage = DVC.VehiclePUC
mapDocumentType Common.VehiclePermitImage = DVC.VehiclePermit
mapDocumentType Common.VehicleInsuranceImage = DVC.VehicleInsurance
mapDocumentType Common.VehicleFitnessCertificateImage = DVC.VehicleFitnessCertificate
mapDocumentType Common.VehicleInspectionImage = DVC.VehicleInspectionForm
mapDocumentType Common.DriverInspectionFormImage = DVC.DriverInspectionForm
mapDocumentType Common.TrainingFormImage = DVC.TrainingForm
mapDocumentType Common.ProfilePhotoImage = DVC.ProfilePhoto
mapDocumentType Common.PanCard = DVC.PanCard
mapDocumentType Common.Permissions = DVC.Permissions
mapDocumentType Common.SubscriptionPlan = DVC.SubscriptionPlan
mapDocumentType Common.ProfileDetails = DVC.ProfileDetails
mapDocumentType Common.AadhaarCard = DVC.AadhaarCard
mapDocumentType Common.SocialSecurityNumber = DVC.SocialSecurityNumber
mapDocumentType Common.GSTCertificate = DVC.GSTCertificate
mapDocumentType Common.BackgroundVerification = DVC.BackgroundVerification
mapDocumentType Common.UploadProfileImage = DVC.UploadProfile
mapDocumentType Common.VehicleNOC = DVC.VehicleNOC
mapDocumentType Common.DriverVehicleNOC = DVC.DriverVehicleNOC
mapDocumentType Common.BusinessLicense = DVC.BusinessLicense
mapDocumentType Common.VehicleFront = DVC.VehicleFront
mapDocumentType Common.VehicleBack = DVC.VehicleBack
mapDocumentType Common.VehicleFrontInterior = DVC.VehicleFrontInterior
mapDocumentType Common.VehicleBackInterior = DVC.VehicleBackInterior
mapDocumentType Common.VehicleLeft = DVC.VehicleLeft
mapDocumentType Common.VehicleRight = DVC.VehicleRight
mapDocumentType Common.Odometer = DVC.Odometer
mapDocumentType Common.InspectionHub = DVC.InspectionHub
mapDocumentType Common.DriverInspectionHub = DVC.DriverInspectionHub
-- Netherlands Document Types
mapDocumentType Common.KIWADriverCard = DVC.KIWADriverCard
mapDocumentType Common.KIWATaxiPermit = DVC.KIWATaxiPermit
mapDocumentType Common.KvKChamberOfCommerceRegistration = DVC.KvKChamberOfCommerceRegistration
mapDocumentType Common.TAXDetails = DVC.TAXDetails
mapDocumentType Common.BankingDetails = DVC.BankingDetails
mapDocumentType Common.VehicleDetails = DVC.VehicleDetails
mapDocumentType Common.SchipolAirportAgreement = DVC.SchipolAirportAgreement
mapDocumentType Common.SchipolSmartcardProof = DVC.SchipolSmartcardProof
mapDocumentType Common.TXQualityMark = DVC.TXQualityMark
-- Finland Document Types
mapDocumentType Common.TaxiDriverPermit = DVC.TaxiDriverPermit
mapDocumentType Common.TaxiTransportLicense = DVC.TaxiTransportLicense
mapDocumentType Common.FinnishIDResidencePermit = DVC.FinnishIDResidencePermit
mapDocumentType Common.BusinessRegistrationExtract = DVC.BusinessRegistrationExtract
mapDocumentType Common.PersonalId = DVC.PersonalId
mapDocumentType Common.LocalResidenceProof = DVC.LocalResidenceProof
mapDocumentType Common.PoliceVerificationCertificate = DVC.PoliceVerificationCertificate
mapDocumentType Common.DrivingSchoolCertificate = DVC.DrivingSchoolCertificate
-- India Certificate Document Types
mapDocumentType Common.LDCCertificate = DVC.LDCCertificate
mapDocumentType Common.TDSCertificate = DVC.TDSCertificate
mapDocumentType Common.TANCertificate = DVC.TANCertificate
mapDocumentType Common.UDYAMCertificate = DVC.UDYAMCertificate
mapDocumentType Common.PanAadhaarLink = DVC.PanAadhaarLinkage
mapDocumentType Common.VoterIdCard = DVC.VoterIdCard
mapDocumentType Common.OperatorPartnerCode = DVC.OperatorPartnerCode
mapDocumentType Common.MedicalCertificate = DVC.MedicalCertificate
mapDocumentType Common.Rating = DVC.Rating
mapDocumentType Common.BotApproval = DVC.BotApproval
mapDocumentType Common.NomineeDetails = DVC.NomineeDetails
mapDocumentType Common.FleetRegistration = DVC.FleetRegistration

postDriverRegistrationDocumentUpload :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UploadDocumentReq -> Flow Common.UploadDocumentResp
postDriverRegistrationDocumentUpload merchantShortId opCity driverId_ req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let docType = mapDocumentType req.imageType
  docConfigs <- getConfig (DocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, documentType = Just docType, vehicleCategory = Nothing}) (Just (CQDVC.findByMerchantOpCityIdAndDocumentType merchantOpCityId docType Nothing))
  -- Nothing or empty list = no role restriction (all allowed); non-empty = only those roles can upload (enforced in validateImageHandler)
  let mbRolesAllowed = listToMaybe docConfigs >>= (.rolesAllowedToUploadDocument)
  let isRoleRestricted = case mbRolesAllowed of Nothing -> False; Just roles -> not (Kernel.Prelude.null roles)
  mbUploaderRole <-
    if isRoleRestricted
      then do
        -- When requestorId is present, get their role for validation; when absent, skip and use Nothing
        case req.requestorId of
          Nothing -> return Nothing
          Just requestorId -> do
            mbRequestor <- QPerson.findById (Id requestorId)
            return (DP.role <$> mbRequestor)
      else do
        -- When requestorId is present and requestor exists at BPP, validate association; when absent or not at BPP (e.g. Admin), allow. Single query for both.
        whenJust req.requestorId $ \requestorId -> do
          entities <- QPerson.findAllByPersonIdsAndMerchantOpsCityId [Id requestorId, cast driverId_] merchantOpCityId
          entity <- find (\e -> e.id == cast driverId_) entities & fromMaybeM (PersonDoesNotExist driverId_.getId)
          whenJust (find (\e -> e.id == Id requestorId) entities) $ \requestor -> do
            isValid <- DDriver.isAssociationBetweenTwoPerson requestor entity
            unless isValid $ throwError (InvalidRequest "Driver is not associated with the entity")
        return Nothing
  res <-
    validateImage
      True
      mbUploaderRole
      (Just docConfigs)
      (cast driverId_, cast merchant.id, merchantOpCityId)
      ImageValidateRequest
        { image = req.imageBase64,
          imageType = mapDocumentType req.imageType,
          rcNumber = req.rcNumber,
          validationStatus = Nothing,
          workflowTransactionId = Nothing,
          vehicleCategory = Nothing,
          sdkFailureReason = Nothing,
          fileExtension = req.fileExtension
        }
  let shouldRefreshVehicleDocsStatus = Kernel.Prelude.any ((== Just DVC.Vehicle) . (.documentCategory)) docConfigs
  let uploadedImageId = cast res.imageId :: Id DImage.Image
  when shouldRefreshVehicleDocsStatus $ do
    mbRcId <- resolveRcIdFromDocument docType uploadedImageId
    whenJust mbRcId $ \rcId ->
      runStatusEventSafely
        "refreshVehicleDocsVerificationStatusForRC:postDriverRegistrationDocumentUpload"
        Nothing
        Nothing
        (SStatus.VehicleDocChangedEvent rcId)
  pure $ Common.UploadDocumentResp {imageId = cast res.imageId}

postDriverRegistrationDocumentsCommon ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Common.CommonDocumentCreateReq ->
  Flow Common.CommonDocumentCreateRes
postDriverRegistrationDocumentsCommon merchantShortId opCity driverId Common.CommonDocumentCreateReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let driverPersonId = cast @Common.Driver @DP.Person driverId
  void $ QPerson.findById driverPersonId >>= fromMaybeM (PersonNotFound driverPersonId.getId)
  whenJust imageId $ \imgId -> do
    void $ QImage.findById (cast imgId) >>= fromMaybeM (InvalidRequest "Image not found")

  -- Validate TDSCertificate document data (same as UI /driver/register/commonDocument)
  when (documentType == Common.TDSCertificate) $ do
    tdsData <- parseTDSCertificateData documentData
    validationErrors <- validateTDSCertificate tdsData
    unless (Kernel.Prelude.null validationErrors) $ do
      let errorJson = TE.decodeUtf8 $ BSL.toStrict $ A.encode validationErrors
      throwError $ InvalidRequest $ "TDS Certificate validation failed: " <> errorJson

    -- Check for duplicate invoiceIds within the new list itself
    let newInvoiceIds = map (.invoiceId) tdsData.tdsCertificates
        uniqueNewInvoiceIds = nub newInvoiceIds
        intraDuplicateIds = newInvoiceIds \\ uniqueNewInvoiceIds
    unless (Kernel.Prelude.null intraDuplicateIds) $
      throwError $ InvalidRequest $ "Duplicate TDS invoiceIds in the same request: " <> T.intercalate ", " (nub intraDuplicateIds)

  let createDocumentEntry = do
        documentId <- generateGUID
        now <- getCurrentTime
        let documentEntry =
              DCommonDoc.CommonDriverOnboardingDocuments
                { id = documentId,
                  documentImageId = cast <$> imageId,
                  documentType = mapDocumentType documentType,
                  driverId = Just driverPersonId,
                  documentData = documentData,
                  rejectReason = Nothing,
                  verificationStatus = Documents.MANUAL_VERIFICATION_REQUIRED,
                  merchantOperatingCityId = merchantOpCityId,
                  merchantId = merchant.id,
                  createdAt = now,
                  updatedAt = now
                }
        QCommonDriverOnboardingDocuments.create documentEntry
        pure $ Common.CommonDocumentCreateRes {result = "Success", documentId = cast documentId}

  res <-
    if documentType == Common.TDSCertificate
      then do
        -- Redis lock to prevent TOCTOU race on duplicate invoice check + create
        let lockKey = "tds-dedup-lock:" <> driverPersonId.getId
        Redis.withLockRedisAndReturnValue lockKey 10 $ do
          -- Check for duplicate invoiceIds across existing TDS documents for this driver
          tdsData' <- parseTDSCertificateData documentData
          let newInvoiceIds' = map (.invoiceId) tdsData'.tdsCertificates
          existingDocs <- QCommonDriverOnboardingDocuments.findByDriverIdAndDocumentType (Just driverPersonId) DVC.TDSCertificate
          let activeDocs = filter (\d -> d.verificationStatus /= Documents.INVALID) existingDocs
          let existingInvoiceIds = Kernel.Prelude.concatMap (extractInvoiceIds . (.documentData)) activeDocs
              duplicateIds = filter (`elem` existingInvoiceIds) newInvoiceIds'
          unless (Kernel.Prelude.null duplicateIds) $
            throwError $ InvalidRequest $ "Duplicate TDS invoiceIds already submitted: " <> T.intercalate ", " duplicateIds
          createDocumentEntry
      else createDocumentEntry
  person <- QPerson.findById driverPersonId
  runStatusEventSafely
    "refreshDocsStatus:postDriverRegistrationDocumentsCommon"
    person
    Nothing
    (SStatus.PersonDocChangedEvent driverPersonId)
  pure res

postDriverRegistrationUnlinkDocument :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.DocumentType -> Maybe Text -> Flow Common.UnlinkDocumentResp
postDriverRegistrationUnlinkDocument merchantShortId opCity personId documentType mbRequestorId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  person <- case mbRequestorId of
    Just requestorId -> do
      -- Single query for both person and requestor; if requestor not present at BPP (e.g. Admin), allow
      entities <- QPerson.findAllByPersonIdsAndMerchantOpsCityId [Id requestorId, cast personId] merchantOpCityId
      entity <- find (\e -> e.id == cast personId) entities & fromMaybeM (PersonDoesNotExist personId.getId)
      whenJust (find (\e -> e.id == Id requestorId) entities) $ \requestor -> do
        isValid <- DDriver.isAssociationBetweenTwoPerson requestor entity
        unless isValid $ throwError (InvalidRequest "Driver is not associated with the entity")
      pure entity
    Nothing -> runInReplica $ QPerson.findById (cast personId) >>= fromMaybeM (PersonDoesNotExist personId.getId)
  res <- unlinkPersonDocument merchantOpCityId person
  runStatusEventSafely
    "onDriverRegistrationUnlinkDocument:postDriverRegistrationUnlinkDocument"
    (Just person)
    Nothing
    (SStatus.DocumentUnlinkedEvent person.id)
  return
    Common.UnlinkDocumentResp
      { mandatoryDocumentRemoved = res
      }
  where
    unlinkPersonDocument :: Id DMOC.MerchantOperatingCity -> DP.Person -> Flow Bool
    unlinkPersonDocument merchantOpCityId person = do
      case person.role of
        role | DCommon.checkFleetOwnerRole role -> do
          case documentType of
            Common.PanCard -> QFOI.updatePanImage Nothing Nothing person.id
            Common.GSTCertificate -> QFOI.updateGstImage Nothing Nothing person.id
            Common.AadhaarCard -> QFOI.updateAadhaarImage Nothing Nothing Nothing person.id
            _ -> throwError (InvalidRequest "Invalid document type")
        DP.DRIVER -> do
          case documentType of
            Common.PanCard -> QDriverInfo.updatePanNumber Nothing person.id
            Common.AadhaarCard -> QDriverInfo.updateAadhaarNumber Nothing person.id
            _ -> throwError (InvalidRequest "Invalid document type")
        _ -> pure ()
      case documentType of
        Common.PanCard -> QPan.deleteByDriverId person.id
        Common.GSTCertificate -> QGstin.deleteByDriverId person.id
        Common.AadhaarCard -> QAadhaarCard.deleteByPersonId person.id
        Common.DriverLicense -> QDL.deleteByDriverId person.id
        _ -> throwError (InvalidRequest "Invalid document type")
      checkAndUpdateEnabledStatus merchantOpCityId documentType person

    checkAndUpdateEnabledStatus :: Id DMOC.MerchantOperatingCity -> Common.DocumentType -> DP.Person -> Flow Bool
    checkAndUpdateEnabledStatus merchantOpCityId docType person = do
      transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
      let enableBotFlow = transporterConfig.enableBotFlow == Just True
      -- isMandatory → verified, isMandatoryForEnabling → enabled (deliberately split).
      case person.role of
        role | DCommon.checkFleetOwnerRole role -> do
          mbCfg <- getOneConfig (FleetOwnerDocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, documentType = Just (mapDocumentType docType), role = Just person.role}) (Just (CQFODVC.findAllByMerchantOpCityId merchantOpCityId (Just [])))
          let blocksVerified = maybe False (.isMandatory) mbCfg
              blocksEnabled = maybe False (\c -> fromMaybe c.isMandatory c.isMandatoryForEnabling) mbCfg
          if enableBotFlow
            then do
              -- BOT: downgrade enabled/verified independently (single query; leaves non-blocked field untouched).
              when (blocksEnabled || blocksVerified) $ QFOI.updateFleetOwnerDowngradeStatus blocksEnabled blocksVerified person.id
              pure (blocksEnabled || blocksVerified)
            else do
              -- Legacy: isMandatory drives only enabled.
              when blocksVerified $ QFOI.updateFleetOwnerEnabledStatus False person.id
              pure blocksVerified
        DP.DRIVER -> do
          mbCfg <- getOneConfig (DocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, documentType = Just (mapDocumentType docType), vehicleCategory = Just DVCat.CAR}) (Just (maybeToList <$> CQDVC.findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOpCityId (mapDocumentType docType) DVCat.CAR Nothing))
          let blocksVerified = maybe False (.isMandatory) mbCfg
              blocksEnabled = maybe False (\c -> fromMaybe c.isMandatory c.isMandatoryForEnabling) mbCfg
          if enableBotFlow
            then do
              applyDriverDocInvalidation transporterConfig person.id blocksEnabled blocksVerified
              pure (blocksEnabled || blocksVerified)
            else do
              -- Legacy: mandatory drives only enabled.
              when blocksEnabled $ Analytics.updateEnabledVerifiedStateWithAnalytics Nothing transporterConfig person.id False Nothing
              pure False
        _ -> pure False

-- | BOT doc invalidation: downgrade enabled/verified separately, and revoke approved.
--   enabled cases go through analytics (which also revokes approved under BOT); the verified-only case
--   writes verified+approved in one query (leaving enabled untouched).
applyDriverDocInvalidation :: DTC.TransporterConfig -> Id DP.Person -> Bool -> Bool -> Flow ()
applyDriverDocInvalidation transporterConfig personId blocksEnabled blocksVerified =
  case (blocksEnabled, blocksVerified) of
    (True, True) -> Analytics.updateEnabledVerifiedStateWithAnalytics Nothing transporterConfig personId False (Just False)
    (True, False) -> Analytics.updateEnabledVerifiedStateWithAnalytics Nothing transporterConfig personId False Nothing
    (False, True) -> QDriverInfo.updateVerifiedAndApprovedState (cast personId) False (Just False)
    (False, False) -> pure ()

postDriverRegistrationRegisterDl :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.RegisterDLReq -> Flow APISuccess
postDriverRegistrationRegisterDl merchantShortId opCity driverId_ Common.RegisterDLReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let verifyBy = case accessType of
        Just accessTypeValue -> case accessTypeValue of
          Common.DASHBOARD_ADMIN -> DPan.DASHBOARD_ADMIN
          Common.DASHBOARD_USER -> DPan.DASHBOARD_USER
          _ -> DPan.DASHBOARD
        Nothing -> DPan.DASHBOARD
  verifyDL
    verifyBy
    (Just merchant)
    (cast driverId_, cast merchant.id, merchantOpCityId)
    DriverDLReq
      { imageId1 = cast imageId1,
        imageId2 = fmap cast imageId2,
        vehicleCategory = vehicleCategory,
        nameOnCardFromSdk = Nothing,
        requestId = Nothing,
        sdkTransactionId = Nothing,
        nameOnCard = Nothing,
        isDLImageValidated = Nothing,
        ..
      }

castVehicleDetails :: Common.DriverVehicleDetails -> DriverVehicleDetails
castVehicleDetails Common.DriverVehicleDetails {..} =
  DriverVehicleDetails
    { vehicleManufacturer = vehicleManufacturer,
      vehicleModel = vehicleModel,
      vehicleColour = vehicleColour,
      vehicleDoors = vehicleDoors,
      vehicleSeatBelts = vehicleSeatBelts,
      vehicleModelYear = vehicleModelYear
    }

postDriverRegistrationRegisterRc :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.RegisterRCReq -> Flow APISuccess
postDriverRegistrationRegisterRc merchantShortId opCity driverId_ req@Common.RegisterRCReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  isFleetOwner <- QFOI.findByPrimaryKey (cast driverId_)
  let (vehicleDetailsToPass, vehicleCategoryToPass, vehicleClassToPass) =
        if transporterConfig.allowDashboardToPassVehicleDetails == Just True
          then (castVehicleDetails <$> req.vehicleDetails, req.vehicleCategory, req.vehicleClass)
          else (Nothing, Nothing, Nothing)
  verifyRC
    True
    (Just merchant)
    (cast driverId_, cast merchant.id, merchantOpCityId)
    ( DriverRCReq
        { imageId = cast imageId,
          imageId2 = cast <$> imageId2,
          udinNumber = udinNumber,
          vehicleCategory = vehicleCategoryToPass,
          vehicleClass = vehicleClassToPass,
          vehicleDetails = vehicleDetailsToPass,
          isRCImageValidated = Nothing,
          engineNumber = req.engineNumber,
          chassisNumber = req.chassisNumber,
          ..
        }
    )
    False
    (bool Nothing (Just (cast driverId_)) (isJust isFleetOwner))

postDriverRegistrationRegisterAadhaar :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.AadhaarCardReq -> Flow APISuccess
postDriverRegistrationRegisterAadhaar merchantShortId opCity driverId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  DOV.postDriverRegisterAadhaarCard (Just (cast driverId), merchant.id, merchantOpCityId) (castAadharReq req)
  where
    castAadharReq Common.AadhaarCardReq {..} = API.Types.UI.DriverOnboardingV2.AadhaarCardReq {aadhaarBackImageId = cast <$> aadhaarBackImageId, aadhaarFrontImageId = cast <$> aadhaarFrontImageId, validationStatus = convertValidationStatus validationStatus, ..}
    convertValidationStatus status = case status of
      Common.APPROVED -> API.Types.UI.DriverOnboardingV2.APPROVED
      Common.DECLINED -> API.Types.UI.DriverOnboardingV2.DECLINED
      Common.AUTO_APPROVED -> API.Types.UI.DriverOnboardingV2.AUTO_APPROVED
      Common.AUTO_DECLINED -> API.Types.UI.DriverOnboardingV2.AUTO_DECLINED
      Common.NEEDS_REVIEW -> API.Types.UI.DriverOnboardingV2.NEEDS_REVIEW

--make a separate function casting the driverVehiclereq

postDriverRegistrationRegisterGenerateAadhaarOtp :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.GenerateAadhaarOtpReq -> Flow Common.GenerateAadhaarOtpRes
postDriverRegistrationRegisterGenerateAadhaarOtp merchantShortId opCity driverId_ req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  res <-
    AV.generateAadhaarOtp
      True
      (Just merchant)
      (cast driverId_)
      merchantOpCityId
      AadhaarVerification.AadhaarOtpReq
        { aadhaarNumber = req.aadhaarNumber,
          consent = req.consent
        }
  pure (convertVerifyOtp res)

postDriverRegistrationRegisterVerifyAadhaarOtp :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.VerifyAadhaarOtpReq -> Flow Common.VerifyAadhaarOtpRes
postDriverRegistrationRegisterVerifyAadhaarOtp merchantShortId opCity driverId_ req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  res <-
    AV.verifyAadhaarOtp
      (Just merchant)
      (cast driverId_)
      merchantOpCityId
      AV.VerifyAadhaarOtpReq
        { otp = req.otp,
          shareCode = req.shareCode
        }
  pure (convertSubmitOtp res)

getDriverRegistrationUnderReviewDrivers :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Flow Common.UnderReviewDriversListResponse
getDriverRegistrationUnderReviewDrivers _merchantShortId _opCity _limit _offset = throwError (InternalError "Not Implemented")

getDriverRegistrationDocumentsInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow Common.StatusRes
getDriverRegistrationDocumentsInfo merchantShortId opCity driverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  statusRes <- DStatus.statusHandler (Id driverId.getId, merchant.id, merchantOpCity.id) Nothing Nothing Nothing Nothing Nothing Nothing
  pure $ castToManagementStatusRes statusRes

castToManagementStatusRes :: DStatus.StatusRes -> Common.StatusRes
castToManagementStatusRes res =
  Common.StatusRes
    { driverDocuments = castDocStatusItem <$> res.driverDocuments,
      driverLicenseDetails = fmap (castMgmtDLDetails <$>) res.driverLicenseDetails,
      vehicleDocuments = castVehicleDocItem <$> res.vehicleDocuments,
      vehicleRegistrationCertificateDetails = fmap (castMgmtRCDetails <$>) res.vehicleRegistrationCertificateDetails,
      enabled = res.enabled,
      manualVerificationRequired = res.manualVerificationRequired
    }

castDocStatusItem :: SStatus.DocumentStatusItem -> Common.DocumentStatusItem
castDocStatusItem item =
  Common.DocumentStatusItem
    { documentType = SDO.castDocumentType item.documentType,
      verificationStatus = castMgmtResponseStatus item.verificationStatus,
      verificationMessage = item.verificationMessage,
      s3Path = item.s3Path,
      expiryDate = item.documentExpiry
    }

castMgmtDLDetails :: SStatus.DLDetails -> Common.DLDetails
castMgmtDLDetails dl =
  Common.DLDetails
    { driverName = dl.driverName,
      driverLicenseNumber = dl.driverLicenseNumber,
      operatingCity = dl.operatingCity,
      driverDateOfBirth = dl.driverDateOfBirth,
      classOfVehicles = dl.classOfVehicles,
      imageId1 = dl.imageId1,
      imageId2 = dl.imageId2,
      dateOfIssue = dl.dateOfIssue,
      createdAt = dl.createdAt
    }

castMgmtRCDetails :: SStatus.RCDetails -> Common.RCDetails
castMgmtRCDetails rc =
  Common.RCDetails
    { vehicleRegistrationCertNumber = rc.vehicleRegistrationCertNumber,
      imageId = rc.imageId,
      operatingCity = rc.operatingCity,
      dateOfRegistration = rc.dateOfRegistration,
      vehicleCategory = rc.vehicleCategory,
      airConditioned = rc.airConditioned,
      vehicleManufacturer = rc.vehicleManufacturer,
      vehicleModel = rc.vehicleModel,
      vehicleColor = rc.vehicleColor,
      vehicleDoors = rc.vehicleDoors,
      vehicleSeatBelts = rc.vehicleSeatBelts,
      vehicleModelYear = rc.vehicleModelYear,
      oxygen = rc.oxygen,
      ventilator = rc.ventilator,
      createdAt = rc.createdAt,
      failedRules = rc.failedRules,
      verificationStatus = DCommon.castVerificationStatus <$> rc.verificationStatus,
      permitExpiry = rc.permitExpiry
    }

castVehicleDocItem :: SStatus.VehicleDocumentItem -> Common.VehicleDocumentItem
castVehicleDocItem vd =
  Common.VehicleDocumentItem
    { registrationNo = vd.registrationNo,
      userSelectedVehicleCategory = vd.userSelectedVehicleCategory,
      verifiedVehicleCategory = vd.verifiedVehicleCategory,
      isVerified = vd.isVerified,
      isActive = vd.isActive,
      isApproved = vd.isApproved,
      vehicleModel = vd.vehicleModel,
      documents = castDocStatusItem <$> vd.documents,
      dateOfUpload = vd.dateOfUpload,
      s3Path = vd.s3Path,
      expiryDate = vd.documentExpiry,
      docsVerificationStatus = castMgmtDocsVerificationStatus <$> vd.docsVerificationStatus
    }

castMgmtDocsVerificationStatus :: DDVS.DocsVerificationStatus -> Dashboard.Common.DocsVerificationStatus
castMgmtDocsVerificationStatus = \case
  DDVS.ADMIN_PENDING -> Dashboard.Common.ADMIN_PENDING
  DDVS.ADMIN_APPROVED -> Dashboard.Common.ADMIN_APPROVED
  DDVS.ADMIN_REJECTED -> Dashboard.Common.ADMIN_REJECTED

castMgmtResponseStatus :: SStatus.ResponseStatus -> Common.VerificationStatus
castMgmtResponseStatus = \case
  SStatus.NO_DOC_AVAILABLE -> Common.PENDING
  SStatus.PENDING -> Common.PENDING
  SStatus.VALID -> Common.VALID
  SStatus.FAILED -> Common.INVALID
  SStatus.INVALID -> Common.INVALID
  SStatus.LIMIT_EXCEED -> Common.INVALID
  SStatus.MANUAL_VERIFICATION_REQUIRED -> Common.MANUAL_VERIFICATION_REQUIRED
  SStatus.UNAUTHORIZED -> Common.UNAUTHORIZED
  SStatus.PULL_REQUIRED -> Common.PENDING
  SStatus.CONSENT_DENIED -> Common.INVALID

approveAndUpdateRC :: Common.RCApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdateRC req merchantId merchantOpCityId = do
  let imageId = Id req.documentImageId.getId
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  mbRc <- QRC.findByImageId imageId
  -- Fallback for re-upload-after-reject: the VRC row's documentImageId still
  -- points at the prior (rejected) image, so findByImageId misses it. Look up
  -- by certificate-number hash to recover the existing row and re-point it.
  mbRcResolved <- case mbRc of
    Just _ -> pure mbRc
    Nothing -> case req.vehicleNumberPlate of
      Just plate -> do
        enc <- encrypt plate
        QRC.findByCertificateNumberHash (enc & hash)
      Nothing -> pure Nothing
  case mbRcResolved of
    Just rc -> do
      certificateNumber <- mapM encrypt req.vehicleNumberPlate
      -- Check for duplicate vehicle number plate if being changed
      whenJust certificateNumber $ \encNum -> do
        mbExistingRC <- QRC.findByCertificateNumberHash (encNum & hash)
        whenJust mbExistingRC $ \existingRC ->
          when (existingRC.id /= rc.id) $
            throwError (InvalidRequest "RC with this vehicle number plate already exists")
      let udpatedRC =
            rc
              { DRC.documentImageId = imageId,
                DRC.vehicleVariant = req.vehicleVariant <|> rc.vehicleVariant,
                DRC.verificationStatus = VALID,
                DRC.rejectReason = Nothing,
                DRC.failedRules = [],
                DRC.certificateNumber = fromMaybe rc.certificateNumber certificateNumber,
                DRC.vehicleManufacturer = req.vehicleManufacturer <|> rc.vehicleManufacturer,
                DRC.vehicleModel = req.vehicleModel <|> rc.vehicleModel,
                DRC.vehicleModelYear = req.vehicleModelYear <|> rc.vehicleModelYear,
                DRC.vehicleColor = req.vehicleColor <|> rc.vehicleColor,
                DRC.vehicleDoors = req.vehicleDoors <|> rc.vehicleDoors,
                DRC.vehicleSeatBelts = req.vehicleSeatBelts <|> rc.vehicleSeatBelts,
                DRC.fitnessExpiry = if isJust req.fitnessExpiry then fromJust req.fitnessExpiry else rc.fitnessExpiry,
                DRC.permitExpiry = req.permitExpiry <|> rc.permitExpiry,
                DRC.docsVerificationStatus =
                  if transporterConfig.enableManualDocumentStatusCheck == Just True
                    then Just DDVS.ADMIN_APPROVED
                    else rc.docsVerificationStatus
              }
      QRC.updateByPrimaryKey udpatedRC
      QImage.updateVerificationStatusByIdAndType VALID imageId DVC.VehicleRegistrationCertificate
      rcImage <- findApproveImage imageId
      createReminder
        DVC.VehicleRegistrationCertificate
        rcImage.personId
        merchantId
        merchantOpCityId
        (Just $ udpatedRC.id.getId)
        (Just udpatedRC.fitnessExpiry)
        Nothing
    Nothing -> do
      case transporterConfig.createDocumentRequired of
        Just True -> do
          vehicleNumberPlate <- req.vehicleNumberPlate & fromMaybeM (InvalidRequest "vehicleNumberPlate is required for creating RC document")
          fitnessExpiry <- req.fitnessExpiry & fromMaybeM (InvalidRequest "fitnessExpiry is required for creating RC document")
          rcImage <- findApproveImage imageId
          encryptedRC <- encrypt vehicleNumberPlate
          -- Check if RC already exists for this number plate
          mbExistingRC <- QRC.findByCertificateNumberHash (encryptedRC & hash)
          whenJust mbExistingRC $ \_ ->
            throwError (InvalidRequest "RC with this vehicle number plate already exists")
          now <- getCurrentTime
          rcId <- generateGUID
          let newRC =
                DRC.VehicleRegistrationCertificate
                  { DRC.id = rcId,
                    DRC.documentImageId = imageId,
                    DRC.certificateNumber = encryptedRC,
                    DRC.fitnessExpiry = fitnessExpiry,
                    DRC.permitExpiry = req.permitExpiry,
                    DRC.pucExpiry = Nothing,
                    DRC.vehicleClass = Nothing,
                    DRC.vehicleVariant = req.vehicleVariant,
                    DRC.vehicleManufacturer = req.vehicleManufacturer,
                    DRC.vehicleCapacity = Nothing,
                    DRC.vehicleModel = req.vehicleModel,
                    DRC.vehicleColor = req.vehicleColor,
                    DRC.vehicleDoors = req.vehicleDoors,
                    DRC.vehicleSeatBelts = req.vehicleSeatBelts,
                    DRC.manufacturerModel = Nothing,
                    DRC.vehicleEnergyType = Nothing,
                    DRC.reviewedAt = Nothing,
                    DRC.reviewRequired = Nothing,
                    DRC.insuranceValidity = Nothing,
                    DRC.mYManufacturing = Nothing,
                    DRC.verificationStatus = VALID,
                    DRC.fleetOwnerId = Nothing,
                    DRC.userPassedVehicleCategory = Nothing,
                    DRC.airConditioned = Nothing,
                    DRC.oxygen = Nothing,
                    DRC.ventilator = Nothing,
                    DRC.luggageCapacity = Nothing,
                    DRC.vehicleRating = Nothing,
                    DRC.vehicleRatingRemark = Nothing,
                    DRC.failedRules = [],
                    DRC.dateOfRegistration = Nothing,
                    DRC.vehicleModelYear = req.vehicleModelYear,
                    DRC.rejectReason = Nothing,
                    DRC.unencryptedCertificateNumber = Just vehicleNumberPlate,
                    DRC.approved = Nothing,
                    DRC.vehicleImageId = Nothing,
                    DRC.merchantId = Just merchantId,
                    DRC.merchantOperatingCityId = Just merchantOpCityId,
                    DRC.createdAt = now,
                    DRC.updatedAt = now,
                    DRC.verified = Nothing,
                    DRC.docsVerificationStatus =
                      if transporterConfig.enableManualDocumentStatusCheck == Just True
                        then Just DDVS.ADMIN_APPROVED
                        else Nothing,
                    DRC.pendingChallan = Nothing
                  }
          QRC.create newRC
          -- Create driver RC association so the RC is linked to the driver
          assocId <- generateGUID
          let driverRCAssoc =
                DRCA.DriverRCAssociation
                  { DRCA.id = assocId,
                    DRCA.driverId = rcImage.personId,
                    DRCA.rcId = rcId,
                    DRCA.associatedOn = now,
                    DRCA.associatedTill = convertTextToUTC (Just "2099-12-12"),
                    DRCA.errorMessage = Nothing,
                    DRCA.consent = True,
                    DRCA.consentTimestamp = now,
                    DRCA.isRcActive = True,
                    DRCA.merchantId = Just merchantId,
                    DRCA.merchantOperatingCityId = Just merchantOpCityId,
                    DRCA.createdAt = now,
                    DRCA.updatedAt = now
                  }
          QRCAssoc.create driverRCAssoc
          QImage.updateVerificationStatusByIdAndType VALID imageId DVC.VehicleRegistrationCertificate
          createReminder
            DVC.VehicleRegistrationCertificate
            rcImage.personId
            merchantId
            merchantOpCityId
            (Just $ rcId.getId)
            (Just fitnessExpiry)
            Nothing
        _ -> throwError (InternalError "RC not found by image id")

approveAndUpdateInsurance :: Common.VInsuranceApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdateInsurance req@Common.VInsuranceApproveDetails {..} mId mOpCityId = do
  let imageId = Id req.documentImageId.getId
  QImage.updateVerificationStatusAndExpiry (Just VALID) req.policyExpiry DVC.VehicleInsurance imageId
  vinsurance <- QVI.findByImageId imageId
  now <- getCurrentTime
  uuid <- generateGUID
  case vinsurance of
    Just insurance -> do
      policyNo <- mapM encrypt req.policyNumber
      let updatedInsurance =
            insurance
              { DVI.insuredName = req.insuredName <|> insurance.insuredName,
                DVI.issueDate = req.issueDate <|> insurance.issueDate,
                DVI.limitsOfLiability = req.limitsOfLiability <|> insurance.limitsOfLiability,
                DVI.policyNumber = fromMaybe insurance.policyNumber policyNo,
                DVI.policyProvider = fromMaybe insurance.policyProvider req.policyProvider,
                DVI.verificationStatus = VALID,
                DVI.policyExpiry = fromMaybe insurance.policyExpiry req.policyExpiry
              }
      QVI.updateByPrimaryKey updatedInsurance
      -- Create reminders for Insurance when it's updated
      createReminder
        DVC.VehicleInsurance
        updatedInsurance.driverId
        mId
        mOpCityId
        (Just $ updatedInsurance.rcId.getId)
        (Just updatedInsurance.policyExpiry)
        Nothing
    Nothing -> do
      case (req.policyNumber, req.policyExpiry, req.policyProvider, req.rcNumber) of
        (Just policyNum, Just policyExp, Just provider, Just rcNo) -> do
          insuranceImage <- findApproveImage imageId
          rcNoEnc <- encrypt rcNo
          rc <- QRC.findByCertificateNumberHash (rcNoEnc & hash) >>= fromMaybeM (InternalError "RC not found by RC number")
          policyNo <- encrypt policyNum
          let insurance =
                DVI.VehicleInsurance
                  { documentImageId = imageId,
                    driverId = insuranceImage.personId,
                    id = uuid,
                    policyNumber = policyNo,
                    rcId = rc.id,
                    verificationStatus = VALID,
                    createdAt = now,
                    updatedAt = now,
                    merchantId = Just mId,
                    merchantOperatingCityId = Just mOpCityId,
                    policyExpiry = policyExp,
                    policyProvider = provider,
                    rejectReason = Nothing,
                    ..
                  }
          QVI.create insurance
          -- Create reminders for Insurance when it's created
          createReminder
            DVC.VehicleInsurance
            insurance.driverId
            mId
            mOpCityId
            (Just $ insurance.rcId.getId)
            (Just insurance.policyExpiry)
            Nothing
        _ -> do
          transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = mOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId mOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound mOpCityId.getId)
          case transporterConfig.createDocumentRequired of
            Just True -> throwError (InternalError "Provide all the details for creating insurance document: policyNumber, policyExpiry, policyProvider, rcNumber")
            _ -> pure ()

approveAndUpdatePUC :: Common.VPUCApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdatePUC req@Common.VPUCApproveDetails {..} mId mOpCityId = do
  let imageId = Id req.documentImageId.getId
  QImage.updateVerificationStatusAndExpiry (Just VALID) (Just req.pucExpiry) DVC.VehiclePUC imageId
  vpuc <- QVPUC.findByImageId imageId
  now <- getCurrentTime
  uuid <- generateGUID
  rcNoEnc <- encrypt rcNumber
  rc <- QRC.findByCertificateNumberHash (rcNoEnc & hash) >>= fromMaybeM (InternalError "RC not found by RC number")
  pucNoEnc <- encrypt req.pucNumber
  case vpuc of
    Just puc -> do
      let updatedpuc =
            puc{DPUC.pucNumber = Just pucNoEnc,
                DPUC.pucExpiry = req.pucExpiry,
                DPUC.rcId = rc.id,
                DPUC.testDate = req.testDate <|> puc.testDate,
                DPUC.verificationStatus = VALID
               }
      QVPUC.updateByPrimaryKey updatedpuc
      -- Create reminders for PUC when it's updated
      createReminder
        DVC.VehiclePUC
        updatedpuc.driverId
        mId
        mOpCityId
        (Just $ updatedpuc.rcId.getId)
        (Just updatedpuc.pucExpiry)
        Nothing
    Nothing -> do
      pucImage <- findApproveImage imageId
      let puc =
            DPUC.VehiclePUC
              { documentImageId = imageId,
                driverId = pucImage.personId,
                id = uuid,
                pucExpiry = req.pucExpiry,
                pucNumber = Just pucNoEnc,
                rcId = rc.id,
                testDate = req.testDate,
                verificationStatus = VALID,
                merchantId = Just mId,
                merchantOperatingCityId = Just mOpCityId,
                createdAt = now,
                updatedAt = now
              }
      QVPUC.create puc
      -- Create reminders for PUC when it's created
      createReminder
        DVC.VehiclePUC
        puc.driverId
        mId
        mOpCityId
        (Just $ puc.rcId.getId)
        (Just puc.pucExpiry)
        Nothing

approveAndUpdatePermit :: Common.VPermitApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdatePermit req@Common.VPermitApproveDetails {..} mId mOpCityId = do
  let imageId = Id req.documentImageId.getId
  QImage.updateVerificationStatusAndExpiry (Just VALID) (Just req.permitExpiry) DVC.VehiclePermit imageId
  vPremit <- QVPermit.findByImageId imageId
  now <- getCurrentTime
  uuid <- generateGUID
  rcNoEnc <- encrypt rcNumber
  rc <- QRC.findByCertificateNumberHash (rcNoEnc & hash) >>= fromMaybeM (InternalError "RC not found by RC number")
  permitNumEnc <- encrypt req.permitNumber
  case vPremit of
    Just permit -> do
      let updatedpermit =
            permit
              { DVPermit.issueDate = req.issueDate <|> permit.issueDate,
                DVPermit.nameOfPermitHolder = req.nameOfPermitHolder <|> permit.nameOfPermitHolder,
                DVPermit.permitExpiry = req.permitExpiry,
                DVPermit.permitNumber = permitNumEnc,
                DVPermit.purposeOfJourney = req.purposeOfJourney <|> permit.purposeOfJourney,
                DVPermit.rcId = rc.id,
                DVPermit.regionCovered = req.regionCovered,
                DVPermit.verificationStatus = VALID
              }
      QVPermit.updateByPrimaryKey updatedpermit
      -- Create reminders for Permit when it's updated
      createReminder
        DVC.VehiclePermit
        updatedpermit.driverId
        mId
        mOpCityId
        (Just $ updatedpermit.rcId.getId)
        (Just updatedpermit.permitExpiry)
        Nothing
    Nothing -> do
      permitImage <- findApproveImage imageId
      let permit =
            DVPermit.VehiclePermit
              { documentImageId = imageId,
                driverId = permitImage.personId,
                id = uuid,
                issueDate = req.issueDate,
                nameOfPermitHolder = req.nameOfPermitHolder,
                permitExpiry = req.permitExpiry,
                permitNumber = permitNumEnc,
                purposeOfJourney = req.purposeOfJourney,
                rcId = rc.id,
                regionCovered = req.regionCovered,
                verificationStatus = VALID,
                merchantId = Just mId,
                merchantOperatingCityId = Just mOpCityId,
                createdAt = now,
                updatedAt = now
              }
      QVPermit.create permit
      -- Create reminders for Permit when it's created
      createReminder
        DVC.VehiclePermit
        permit.driverId
        mId
        mOpCityId
        (Just $ permit.rcId.getId)
        (Just permit.permitExpiry)
        Nothing

approveAndUpdateFitnessCertificate :: Common.FitnessApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdateFitnessCertificate req@Common.FitnessApproveDetails {..} mId mOpCityId = do
  let imageId = Id req.documentImageId.getId
  QImage.updateVerificationStatusByIdAndType VALID imageId DVC.VehicleFitnessCertificate
  mbFitnessCert <- QFC.findByImageId imageId
  applicationNo <- encrypt req.applicationNumber
  now <- getCurrentTime
  uuid <- generateGUID
  case mbFitnessCert of
    Just certificate -> do
      let updatedFitnessCert =
            certificate
              { DFC.applicationNumber = applicationNo,
                DFC.categoryOfVehicle = req.categoryOfVehicle <|> certificate.categoryOfVehicle,
                DFC.fitnessExpiry = req.fitnessExpiry,
                DFC.inspectingAuthority = req.inspectingAuthority <|> certificate.inspectingAuthority,
                DFC.inspectingOn = req.inspectingOn <|> certificate.inspectingOn,
                DFC.nextInspectionDate = req.nextInspectionDate <|> certificate.nextInspectionDate,
                DFC.receiptDate = req.receiptDate <|> certificate.receiptDate,
                DFC.verificationStatus = VALID
              }
      QFC.updateByPrimaryKey updatedFitnessCert
      -- Create reminders for Fitness Certificate when it's updated
      createReminder
        DVC.VehicleFitnessCertificate
        updatedFitnessCert.driverId
        mId
        mOpCityId
        (Just $ updatedFitnessCert.rcId.getId)
        (Just updatedFitnessCert.fitnessExpiry)
        Nothing
    Nothing -> do
      certificateImage <- findApproveImage imageId
      rcNoEnc <- encrypt rcNumber
      rc <- QRC.findByCertificateNumberHash (rcNoEnc & hash) >>= fromMaybeM (InternalError "RC not found by rc number")
      let fitnessCert =
            DFC.VehicleFitnessCertificate
              { applicationNumber = applicationNo,
                documentImageId = imageId,
                driverId = certificateImage.personId,
                id = uuid,
                rcId = rc.id,
                verificationStatus = VALID,
                createdAt = now,
                updatedAt = now,
                merchantId = Just mId,
                merchantOperatingCityId = Just mOpCityId,
                ..
              }
      QFC.create fitnessCert
      -- Create reminders for Fitness Certificate when it's created
      createReminder
        DVC.VehicleFitnessCertificate
        fitnessCert.driverId
        mId
        mOpCityId
        (Just $ fitnessCert.rcId.getId)
        (Just fitnessCert.fitnessExpiry)
        Nothing

-- | Wrapper over the per-document domain rows so approve flows can share one validation path.
data ApproveDocumentData
  = DLApproveData DDL.DriverLicense
  | PanApproveData DPan.DriverPanCard
  | AadhaarApproveData DAadhaar.AadhaarCard
  | GstApproveData DGstin.DriverGstin
  | BusinessLicenseApproveData DBL.BusinessLicense
  | UdyamApproveData DUdyam.DriverUdyam

approveDocDriverId :: ApproveDocumentData -> Id DP.Person
approveDocDriverId = \case
  DLApproveData dl -> dl.driverId
  PanApproveData pan -> pan.driverId
  AadhaarApproveData aadhaar -> aadhaar.driverId
  GstApproveData gst -> gst.driverId
  BusinessLicenseApproveData bl -> bl.driverId
  UdyamApproveData udyam -> udyam.driverId

approveDocStatus :: ApproveDocumentData -> Documents.VerificationStatus
approveDocStatus = \case
  DLApproveData dl -> dl.verificationStatus
  PanApproveData pan -> pan.verificationStatus
  AadhaarApproveData aadhaar -> aadhaar.verificationStatus
  GstApproveData gst -> gst.verificationStatus
  BusinessLicenseApproveData bl -> bl.verificationStatus
  UdyamApproveData udyam -> udyam.verificationStatus

-- | Common validation for document approve flows (DL, PAN, Aadhaar, GST, BusinessLicense, UDYAM).
--
-- 'mbExistDocData' is the row the caller resolved via imageId (or its own doc-number fallback).
-- When it is Nothing we additionally look the row up by the requested document number, so a
-- number already linked elsewhere is still validated in the create path.
--
-- Checks:
--   1. Number check: the stored number must match the requested one. Mismatch means one entity
--      uploaded the document with number D1 but the admin approved with number D2.
--   2. Document check: a VALID row for this number must not belong to a different driver.
--      case:- Exist -> (D1-N1-VALID) and Request (D1-N1) -> allow (admin may want to update image and details)
--   3. Driver check: the requesting driver must not already hold a VALID document of this type
--      on a different row. For handle this case:- Exist -> (D2-N2-VALID, D1-N1-INVALID) and Request -> (D2-N1)
-- | Directly deletes ALL of the requesting driver's INVALID rows for the document type
-- (a driver may have multiple) without a prior get, so the approve flow can upsert the
-- approved row cleanly. Call right before the upsert/create in approve functions.
deleteInvalidDocumentOfDriver :: DVC.DocumentType -> Id DP.Person -> Flow ()
deleteInvalidDocumentOfDriver documentType reqDriverId = case documentType of
  DVC.DriverLicense -> QDL.deleteByDriverIdAndStatus reqDriverId Documents.INVALID
  DVC.PanCard -> QPan.deleteByDriverIdAndStatus reqDriverId Documents.INVALID
  DVC.AadhaarCard -> QAadhaarCard.deleteByDriverIdAndStatus reqDriverId Documents.INVALID
  DVC.GSTCertificate -> QGstin.deleteByDriverIdAndStatus reqDriverId Documents.INVALID
  DVC.BusinessLicense -> QBLExtra.deleteByDriverIdAndStatus reqDriverId Documents.INVALID
  DVC.UDYAMCertificate -> QUdyamExtra.deleteByDriverIdAndStatus reqDriverId Documents.INVALID
  _ -> pure ()

validateDocumentApprovalChecks :: DVC.DocumentType -> Maybe Text -> Id DP.Person -> Maybe ApproveDocumentData -> Flow ()
validateDocumentApprovalChecks documentType mbReqDocNum reqDriverId mbExistDocData = do
  mbDocData <- case mbExistDocData of
    Just docData -> pure (Just docData)
    Nothing -> maybe (pure Nothing) findDocumentByNumber mbReqDocNum
  whenJust mbDocData $ \docData -> do
    when
      (approveDocStatus docData == Documents.VALID && approveDocDriverId docData /= reqDriverId) -- Need to discuss that if req document already present in pending or any other status with other driver then in the case of admin, we will overirde the driverId or not.
      throwDocumentAlreadyLinked
  mbDriverDoc <- findValidDocumentByDriverId
  whenJust mbDriverDoc $ \driverDoc ->
    case mbDocData of
      Just docData ->
        when (approveDocDriverId driverDoc /= approveDocDriverId docData) $
          throwError DriverAlreadyLinked
      Nothing -> throwError DriverAlreadyLinked
  where
    findDocumentByNumber docNum = case documentType of
      DVC.DriverLicense -> fmap DLApproveData <$> QDL.findByDLNumber docNum
      DVC.PanCard -> do
        panHash <- getDbHash docNum
        panList <- QPan.findAllByEncryptedPanNumber panHash
        pure $ PanApproveData <$> (find (\pan -> pan.driverId == reqDriverId) panList <|> listToMaybe panList)
      DVC.AadhaarCard -> do
        aadhaarHash <- getDbHash docNum
        aadhaarList <- QAadhaarCard.findAllByEncryptedAadhaarNumber (Just aadhaarHash)
        pure $ AadhaarApproveData <$> (find (\aadhaar -> aadhaar.driverId == reqDriverId) aadhaarList <|> listToMaybe aadhaarList)
      DVC.GSTCertificate -> fmap GstApproveData <$> QGstin.findUnInvalidByGstNumber docNum
      DVC.BusinessLicense -> do
        licenseHash <- getDbHash docNum
        fmap BusinessLicenseApproveData . listToMaybe <$> QBLExtra.findAllByLicenseNumberHash licenseHash
      DVC.UDYAMCertificate -> do
        udyamHash <- getDbHash docNum
        fmap UdyamApproveData . listToMaybe <$> QUdyam.findAllByEncryptedUdyamNumber udyamHash
      _ -> throwError $ InvalidRequest $ "Approve validation not supported for document type " <> show documentType
    findValidDocumentByDriverId = case documentType of
      DVC.DriverLicense -> fmap DLApproveData <$> QDL.findByDriverIdAndVerificationStatus reqDriverId Documents.VALID
      DVC.PanCard -> fmap PanApproveData <$> QPan.findValidByDriverId reqDriverId
      DVC.AadhaarCard -> do
        mbAadhaar <- QAadhaarCard.findByPrimaryKey reqDriverId
        pure $ case mbAadhaar of
          Just aadhaar | aadhaar.verificationStatus == Documents.VALID -> Just (AadhaarApproveData aadhaar)
          _ -> Nothing
      DVC.GSTCertificate -> do
        mbGst <- QGstin.findByDriverId reqDriverId
        pure $ case mbGst of
          Just gst | gst.verificationStatus == Documents.VALID -> Just (GstApproveData gst)
          _ -> Nothing
      DVC.BusinessLicense -> fmap BusinessLicenseApproveData <$> QBLExtra.findByPersonIdAndVerificationStatus reqDriverId Documents.VALID
      DVC.UDYAMCertificate -> fmap UdyamApproveData <$> QUdyam.findByDriverIdAndVerificationStatus reqDriverId Documents.VALID
      _ -> pure Nothing
    throwDocumentAlreadyLinked :: Flow ()
    throwDocumentAlreadyLinked = case documentType of
      DVC.DriverLicense -> throwError DLAlreadyLinked
      DVC.PanCard -> throwError PanAlreadyLinked
      DVC.AadhaarCard -> throwError AadhaarAlreadyLinked
      DVC.GSTCertificate -> throwError GstAlreadyLinked
      DVC.UDYAMCertificate -> throwError UdyamAlreadyLinked
      docType -> throwError $ DocumentAlreadyLinkedToAnotherDriver (show docType)

findApproveImage :: Id DImage.Image -> Flow DImage.Image
findApproveImage imageId = QImage.findById imageId >>= fromMaybeM (InternalError "Image not found by image id")

whenCreateDocumentRequired :: Id DMOC.MerchantOperatingCity -> Flow () -> Flow () -> Flow ()
whenCreateDocumentRequired mOpCityId onSkip createAction = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = mOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId mOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound mOpCityId.getId)
  if transporterConfig.createDocumentRequired == Just True then createAction else onSkip

approveAndUpdateDL :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Common.DLApproveDetails -> Flow ()
approveAndUpdateDL merchantId merchantOpCityId req = do
  let imageId = Id req.documentImageId.getId
  dlImage <- findApproveImage imageId
  let driverId = dlImage.personId
  mbDl <- QDL.findByImageId imageId
  -- Fallback for re-upload-after-reject: the DL row's documentImageId1 still
  -- points at the prior (rejected) image, so findByImageId misses it. Look up
  -- by DL number to recover the existing row and re-point it at the new image.
  mbDlResolved <- case mbDl of
    Just _ -> pure mbDl
    Nothing -> case req.driverLicenseNumber of
      Just dlNum -> QDL.findByDLNumber dlNum
      Nothing -> pure Nothing
  -- Common approve-time checks: number mismatch, document linked to another driver, driver already linked
  validateDocumentApprovalChecks DVC.DriverLicense req.driverLicenseNumber driverId (DLApproveData <$> mbDlResolved)
  case mbDlResolved of
    Just dl -> do
      licenseNumber <- mapM encrypt req.driverLicenseNumber
      let updatedDL =
            dl
              { DDL.documentImageId1 = imageId,
                DDL.licenseNumber = fromMaybe dl.licenseNumber licenseNumber,
                DDL.driverDob = req.driverDateOfBirth <|> dl.driverDob,
                DDL.licenseExpiry = fromMaybe dl.licenseExpiry req.dateOfExpiry,
                DDL.verificationStatus = VALID,
                DDL.rejectReason = Nothing,
                DDL.driverId = driverId
              }
      -- Clean up stale INVALID rows, then upsert (the driver's own row may be among the deleted)
      deleteInvalidDocumentOfDriver DVC.DriverLicense driverId
      QDL.upsert updatedDL
      QImage.updateVerificationStatusByIdAndType VALID imageId DVC.DriverLicense
      whenJust dl.documentImageId2 $ \img2 ->
        QImage.updateVerificationStatusByIdAndType VALID img2 DVC.DriverLicense
      -- Create reminders for DL when it's updated
      createReminder
        DVC.DriverLicense
        updatedDL.driverId
        merchantId
        merchantOpCityId
        (Just $ updatedDL.id.getId)
        (Just updatedDL.licenseExpiry)
        Nothing
    Nothing -> whenCreateDocumentRequired merchantOpCityId (throwError (InternalError "DL not found by image id")) $ do
      dlNumber <- req.driverLicenseNumber & fromMaybeM (InvalidRequest "driverLicenseNumber is required for creating DL document")
      dlExpiry <- req.dateOfExpiry & fromMaybeM (InvalidRequest "dateOfExpiry is required for creating DL document")
      encryptedDLNumber <- encrypt dlNumber
      now <- getCurrentTime
      dlId <- generateGUID
      let newDL =
            DDL.DriverLicense
              { DDL.id = dlId,
                DDL.driverId = driverId,
                DDL.documentImageId1 = imageId,
                DDL.documentImageId2 = Nothing,
                DDL.licenseNumber = encryptedDLNumber,
                DDL.licenseExpiry = dlExpiry,
                DDL.driverDob = req.driverDateOfBirth,
                DDL.driverName = Nothing,
                DDL.classOfVehicles = [],
                DDL.verificationStatus = VALID,
                DDL.failedRules = [],
                DDL.dateOfIssue = Nothing,
                DDL.rejectReason = Nothing,
                DDL.vehicleCategory = Nothing,
                DDL.consent = True,
                DDL.consentTimestamp = now,
                DDL.merchantId = Just merchantId,
                DDL.createdAt = now,
                DDL.updatedAt = now
              }
      deleteInvalidDocumentOfDriver DVC.DriverLicense driverId
      QDL.create newDL
      QImage.updateVerificationStatusByIdAndType VALID imageId DVC.DriverLicense
      createReminder
        DVC.DriverLicense
        driverId
        merchantId
        merchantOpCityId
        (Just $ dlId.getId)
        (Just dlExpiry)
        Nothing

approveAndUpdateNOC :: Common.NOCApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdateNOC req@Common.NOCApproveDetails {..} mId mOpCityId = do
  let imageId = Id req.documentImageId.getId
  QImage.updateVerificationStatusAndExpiry (Just VALID) (Just req.nocExpiry) DVC.VehicleNOC imageId
  vnoc <- QVNOC.findByImageId imageId
  now <- getCurrentTime
  uuid <- generateGUID
  rcNoEnc <- encrypt rcNumber
  rc <- QRC.findByCertificateNumberHash (rcNoEnc & hash) >>= fromMaybeM (InternalError "RC not found by RC number")
  nocNoEnc <- encrypt req.nocNumber
  case vnoc of
    Just noc -> do
      let updatednoc =
            noc{DNOC.nocNumber = nocNoEnc,
                DNOC.nocExpiry = req.nocExpiry,
                DNOC.rcId = rc.id,
                DNOC.verificationStatus = VALID
               }
      QVNOC.updateByPrimaryKey updatednoc
    Nothing -> do
      nocImage <- findApproveImage imageId
      let noc =
            DNOC.VehicleNOC
              { documentImageId = imageId,
                driverId = nocImage.personId,
                id = uuid,
                nocExpiry = req.nocExpiry,
                nocNumber = nocNoEnc,
                rcId = rc.id,
                verificationStatus = VALID,
                merchantId = Just mId,
                merchantOperatingCityId = Just mOpCityId,
                createdAt = now,
                updatedAt = now
              }
      QVNOC.create noc

approveAndUpdateBusinessLicense :: Common.BusinessLicenseApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdateBusinessLicense req mId mOpCityId = do
  let imageId = Id req.documentImageId.getId
  blImage <- findApproveImage imageId
  let driverId = blImage.personId
  person <- validatePersonForDocumentApproval driverId mId
  licenseHash <- getDbHash req.businessLicenseNumber
  mbBlByImage <- QBL.findByImageId imageId
  -- Fallback for re-upload-after-reject: the BL row's documentImageId still points at the
  -- prior (rejected) image. Look up by license number hash to recover the existing row.
  mbBl <- case mbBlByImage of
    Just _ -> pure mbBlByImage
    Nothing -> listToMaybe <$> QBLExtra.findAllByLicenseNumberHash licenseHash
  -- Common approve-time checks: number mismatch, document linked to another driver, driver already linked
  validateDocumentApprovalChecks DVC.BusinessLicense (Just req.businessLicenseNumber) driverId (BusinessLicenseApproveData <$> mbBl)
  case mbBl of
    Just bl -> do
      businessLicenseNumberEnc <- encrypt req.businessLicenseNumber
      QImage.updateVerificationStatusAndExpiry (Just VALID) (Just req.licenseExpiry) DVC.BusinessLicense imageId
      let updatedBl =
            bl
              { DBL.documentImageId = imageId,
                DBL.licenseNumber = businessLicenseNumberEnc,
                DBL.licenseExpiry = req.licenseExpiry,
                DBL.verificationStatus = VALID,
                DBL.driverId = driverId
              }
      -- Clean up stale INVALID rows, then upsert (the driver's own row may be among the deleted)
      deleteInvalidDocumentOfDriver DVC.BusinessLicense driverId
      QBLExtra.upsert updatedBl
      createReminder
        DVC.BusinessLicense
        updatedBl.driverId
        mId
        mOpCityId
        (Just $ updatedBl.id.getId)
        (Just updatedBl.licenseExpiry)
        Nothing
      updateFleetOwnerInfoOnDocApproval person $ \personId ->
        QFOIE.updateBusinessLicenseImageAndNumber (Just imageId.getId) (Just businessLicenseNumberEnc) personId
    Nothing -> whenCreateDocumentRequired mOpCityId (throwError (InternalError "Business License not found by image id")) $ do
      businessLicenseNumberEnc <- encrypt req.businessLicenseNumber
      now <- getCurrentTime
      uuid <- generateGUID
      QImage.updateVerificationStatusAndExpiry (Just VALID) (Just req.licenseExpiry) DVC.BusinessLicense imageId
      let bl =
            DBL.BusinessLicense
              { documentImageId = imageId,
                driverId = driverId,
                id = uuid,
                licenseExpiry = req.licenseExpiry,
                licenseNumber = businessLicenseNumberEnc,
                verificationStatus = VALID,
                merchantId = Just mId,
                merchantOperatingCityId = Just mOpCityId,
                createdAt = now,
                updatedAt = now
              }
      deleteInvalidDocumentOfDriver DVC.BusinessLicense driverId
      QBL.create bl
      createReminder
        DVC.BusinessLicense
        bl.driverId
        mId
        mOpCityId
        (Just $ bl.id.getId)
        (Just bl.licenseExpiry)
        Nothing
      updateFleetOwnerInfoOnDocApproval person $ \personId ->
        QFOIE.updateBusinessLicenseImageAndNumber (Just imageId.getId) (Just businessLicenseNumberEnc) personId

approveAndUpdatePan :: Common.PanApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdatePan req mId mOpCityId = do
  let imageId = Id req.documentImageId.getId
  panImage <- findApproveImage imageId
  let driverId = panImage.personId
  person <- validatePersonForDocumentApproval driverId mId
  panHash <- getDbHash req.panNumber
  panInfoList <- QPan.findAllByEncryptedPanNumber panHash
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = mOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId mOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound mOpCityId.getId)
  case transporterConfig.allowDuplicatePan of
    Just False -> do
      let otherDriverIds = filter (/= driverId) (map (.driverId) panInfoList)
      unless (Kernel.Prelude.null otherDriverIds) $ throwError PanAlreadyLinked
    _ -> pure ()
  -- Fallback for re-upload: if no PAN row points to this image, recover the driver's existing PAN row
  mbPanByImage <- QPan.findByImageId imageId
  mbPan <- case mbPanByImage of
    Just _ -> pure mbPanByImage
    Nothing -> pure $ find (\p -> p.driverId == driverId) panInfoList
  -- Common approve-time checks: number mismatch, document linked to another driver, driver already linked
  validateDocumentApprovalChecks DVC.PanCard (Just req.panNumber) driverId (PanApproveData <$> mbPan)
  now <- getCurrentTime
  uuid <- generateGUID
  panNoEnc <- encrypt req.panNumber
  QImage.updateVerificationStatusByIdAndType VALID (Id imageId.getId) DVC.PanCard
  case mbPan of
    Just pan -> do
      let updatedPan =
            pan{DPan.panCardNumber = panNoEnc,
                DPan.verificationStatus = VALID,
                DPan.docType = castReqTypeToDomain <$> req.docType,
                DPan.driverNameOnGovtDB = req.driverNameOnGovtDB,
                DPan.driverDob = req.driverDob,
                DPan.documentImageId1 = imageId,
                DPan.driverId = driverId
               }
      -- Clean up stale INVALID rows, then upsert (the driver's own row may be among the deleted)
      deleteInvalidDocumentOfDriver DVC.PanCard driverId
      QPan.upsert updatedPan
    Nothing -> do
      let pan =
            DPan.DriverPanCard
              { panCardNumber = panNoEnc,
                documentImageId1 = imageId,
                driverId = driverId,
                id = uuid,
                verificationStatus = VALID,
                rejectReason = Nothing,
                merchantId = Just mId,
                merchantOperatingCityId = Just mOpCityId,
                createdAt = now,
                updatedAt = now,
                consent = True,
                consentTimestamp = now,
                docType = castReqTypeToDomain <$> req.docType,
                documentImageId2 = Nothing,
                driverDob = req.driverDob,
                driverName = Just person.firstName,
                driverNameOnGovtDB = req.driverNameOnGovtDB,
                failedRules = [],
                verifiedBy = Just DPan.DASHBOARD,
                panAadhaarLinkage = Nothing
              }
      deleteInvalidDocumentOfDriver DVC.PanCard driverId
      QPan.create pan
  updateFleetOwnerInfoOnDocApproval person $ \personId ->
    QFOIE.updatePanImage (Just panNoEnc) (Just imageId.getId) personId

approveAndUpdateAadhaar :: Common.AadhaarApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdateAadhaar req mId mOpCityId = do
  let imageId = Id req.documentImageId.getId
      mbImageId2 = Id . (.getId) <$> req.documentImageId2
  aadhaarImage <- findApproveImage imageId
  let driverId = aadhaarImage.personId
  person <- validatePersonForDocumentApproval driverId mId
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = mOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId mOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound mOpCityId.getId)
  aadhaarHash <- getDbHash req.aadhaarNumber
  encryptedAadhaar <- encrypt req.aadhaarNumber
  aadhaarInfoList <- QAadhaarCard.findAllByEncryptedAadhaarNumber (Just aadhaarHash)
  case transporterConfig.allowDuplicateAadhaar of
    Just False -> do
      let otherDriverIds = filter (/= driverId) (map (.driverId) aadhaarInfoList)
      unless (Kernel.Prelude.null otherDriverIds) $ throwError AadhaarAlreadyLinked
    _ -> pure ()
  -- Fallback for re-upload: if no aadhaar row points to this image, recover the driver's existing row by aadhaar number.
  -- Aadhaar is driver-keyed (one record per driver), so also fall back to the primary key lookup to make sure an
  -- existing row with a different number resolves to an update instead of a duplicate create.
  mbAadhaarByImage <- QAadhaarCard.findByFrontImageId (Just imageId)
  mbAadhaar <- case mbAadhaarByImage of
    Just _ -> pure mbAadhaarByImage
    Nothing -> pure $ find (\a -> a.driverId == driverId) aadhaarInfoList
  -- Common approve-time checks: number mismatch, document linked to another driver, driver already linked
  validateDocumentApprovalChecks DVC.AadhaarCard (Just req.aadhaarNumber) driverId (AadhaarApproveData <$> mbAadhaar)
  QImage.updateVerificationStatusByIdAndType VALID imageId DVC.AadhaarCard
  whenJust mbImageId2 $ \backImageId ->
    QImage.updateVerificationStatusByIdAndType VALID backImageId DVC.AadhaarCard
  now <- getCurrentTime
  let maskedNumber = T.replicate (T.length req.aadhaarNumber - 4) "X" <> T.takeEnd 4 req.aadhaarNumber
  case mbAadhaar of
    Just aadhaar -> do
      let updatedAadhaar =
            aadhaar
              { DAadhaar.aadhaarNumber = Just encryptedAadhaar,
                DAadhaar.maskedAadhaarNumber = Just maskedNumber,
                DAadhaar.nameOnCard = req.nameOnCard <|> aadhaar.nameOnCard,
                DAadhaar.dateOfBirth = req.dateOfBirth <|> aadhaar.dateOfBirth,
                DAadhaar.address = req.address <|> aadhaar.address,
                DAadhaar.aadhaarFrontImageId = Just imageId,
                DAadhaar.aadhaarBackImageId = mbImageId2 <|> aadhaar.aadhaarBackImageId,
                DAadhaar.verificationStatus = VALID,
                DAadhaar.updatedAt = now,
                DAadhaar.driverId = driverId
              }
      -- Clean up stale INVALID rows, then upsert (the driver's own row may be among the deleted)
      deleteInvalidDocumentOfDriver DVC.AadhaarCard driverId
      QAadhaarCard.upsertAadhaarRecord updatedAadhaar
    Nothing -> do
      let aadhaarCard =
            DAadhaar.AadhaarCard
              { DAadhaar.driverId = driverId,
                DAadhaar.aadhaarNumber = Just encryptedAadhaar,
                DAadhaar.maskedAadhaarNumber = Just maskedNumber,
                DAadhaar.nameOnCard = req.nameOnCard,
                DAadhaar.dateOfBirth = req.dateOfBirth,
                DAadhaar.address = req.address,
                DAadhaar.verificationStatus = VALID,
                DAadhaar.rejectReason = Nothing,
                DAadhaar.consent = True,
                DAadhaar.consentTimestamp = now,
                DAadhaar.aadhaarFrontImageId = Just imageId,
                DAadhaar.aadhaarBackImageId = mbImageId2,
                DAadhaar.driverGender = Nothing,
                DAadhaar.driverImage = Nothing,
                DAadhaar.driverImagePath = Nothing,
                DAadhaar.merchantId = mId,
                DAadhaar.merchantOperatingCityId = mOpCityId,
                DAadhaar.createdAt = now,
                DAadhaar.updatedAt = now
              }
      deleteInvalidDocumentOfDriver DVC.AadhaarCard driverId
      QAadhaarCard.create aadhaarCard
  updateFleetOwnerInfoOnDocApproval person $ \personId ->
    QFOIE.updateAadhaarImage (Just encryptedAadhaar) (Just imageId.getId) ((.getId) <$> mbImageId2) personId

approveAndUpdateCommonDocument :: Common.CommonDocumentApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdateCommonDocument req _mId _mOpCityId = do
  let documentId = Id req.documentId.getId
  -- Get the existing document
  mbDocument <- QCommonDriverOnboardingDocuments.findById documentId
  document <- mbDocument & fromMaybeM (DocumentNotFound documentId.getId)

  -- Update document with new data if provided, otherwise just update verification status
  let updatedDocument = case req.updatedDocumentData of
        Just newData -> document {DCommonDoc.documentData = newData, DCommonDoc.verificationStatus = VALID}
        Nothing -> document {DCommonDoc.verificationStatus = VALID}

  QCommonDriverOnboardingDocuments.updateByPrimaryKey updatedDocument
  -- Keep image and common-doc statuses in sync for common document approvals.
  whenJust document.documentImageId $ \documentImageId ->
    QImage.updateVerificationStatusOnlyById VALID (Id documentImageId.getId)

  -- When approving BusinessLicense or TAXDetails (used for BID/VAT in international flow),
  -- also update the fleet_owner_information table.
  let finalDocData = updatedDocument.documentData
      mbIdentifier = extractIdentifierFromDocData finalDocData
  whenJust document.driverId $ \driverId -> do
    let personId = cast driverId
    case document.documentType of
      DVC.BusinessLicense ->
        whenJust mbIdentifier $ \identifierNumber -> do
          encNumber <- encrypt identifierNumber
          QFOIE.updateBusinessLicenseNumberById (Just encNumber) personId
      DVC.TAXDetails ->
        QFOIE.updateVatNumberById mbIdentifier personId
      _ -> pure ()
  where
    extractIdentifierFromDocData :: Text -> Maybe Text
    extractIdentifierFromDocData docData = do
      let parsed = A.decode (BSL.fromStrict (TE.encodeUtf8 docData)) :: Maybe A.Object
      parsed >>= DAKM.lookup (DAK.fromText "identifierNumber") >>= \case
        A.String s -> if T.null s then Nothing else Just s
        _ -> Nothing

rejectAndUpdateCommonDocument :: Common.CommonDocumentRejectDetails -> Id DMOC.MerchantOperatingCity -> Flow ()
rejectAndUpdateCommonDocument req _mOpCityId = do
  let documentId = Id req.documentId.getId
  -- Get the existing document
  mbDocument <- QCommonDriverOnboardingDocuments.findById documentId
  document <- mbDocument & fromMaybeM (DocumentNotFound documentId.getId)

  -- Update document with rejection status and reason
  let updatedDocument = document {DCommonDoc.verificationStatus = INVALID, DCommonDoc.rejectReason = Just req.reason}

  QCommonDriverOnboardingDocuments.updateByPrimaryKey updatedDocument
  whenJust document.documentImageId $ \imageId ->
    QImage.updateVerificationStatusAndFailureReason INVALID (ImageNotValid req.reason) imageId

approveAndUpdateUdyamDocument :: Common.UDYAMApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdateUdyamDocument req merchantId merchantOpCityId = do
  let imageId = Id req.documentImageId.getId
  udyamImage <- findApproveImage imageId
  let driverId = udyamImage.personId
  mbUdyamByImage <- QUdyam.findByImageId (Just imageId)
  -- Fallback for re-upload-after-reject: the UDYAM row's documentImageId still points at the
  -- prior (rejected) image. Look up by udyam number hash to recover the existing row.
  mbUdyamResolved <- case mbUdyamByImage of
    Just _ -> pure mbUdyamByImage
    Nothing -> case req.udyamNumber of
      Just uNum -> do
        udyamHash <- getDbHash uNum
        listToMaybe <$> QUdyam.findAllByEncryptedUdyamNumber udyamHash
      Nothing -> pure Nothing
  -- Common approve-time checks: number mismatch, document linked to another driver, driver already linked
  validateDocumentApprovalChecks DVC.UDYAMCertificate req.udyamNumber driverId (UdyamApproveData <$> mbUdyamResolved)
  case mbUdyamResolved of
    Just udyam -> do
      storedUamNumber <- decrypt udyam.udyamNumber
      let uamNumber = fromMaybe storedUamNumber req.udyamNumber
      encryptedUam <- encrypt uamNumber
      let updatedUdyam =
            udyam
              { DUdyam.documentImageId = Just imageId,
                DUdyam.udyamNumber = encryptedUam,
                DUdyam.verificationStatus = VALID,
                DUdyam.rejectReason = Nothing,
                DUdyam.driverId = driverId
              }
      -- Clean up stale INVALID rows, then upsert (the driver's own row may be among the deleted)
      deleteInvalidDocumentOfDriver DVC.UDYAMCertificate driverId
      QUdyamExtra.upsert updatedUdyam
      QImage.updateVerificationStatusByIdAndType VALID imageId DVC.UDYAMCertificate
      whenJust req.tdsRate $ \rate ->
        QFOI.updateTdsRate (Just rate) driverId
    Nothing -> whenCreateDocumentRequired merchantOpCityId (pure ()) $ do
      uamNumber <- req.udyamNumber & fromMaybeM (InvalidRequest "udyamNumber is required for creating UDYAM document")
      encryptedUam <- encrypt uamNumber
      now <- getCurrentTime
      udyamId <- generateGUID
      let newUdyam =
            DUdyam.DriverUdyam
              { id = udyamId,
                driverId = driverId,
                documentImageId = Just imageId,
                udyamNumber = encryptedUam,
                verificationStatus = VALID,
                verifiedBy = Nothing,
                merchantId = Just merchantId,
                merchantOperatingCityId = Just merchantOpCityId,
                enterpriseName = Nothing,
                enterpriseType = Nothing,
                rejectReason = Nothing,
                createdAt = now,
                updatedAt = now
              }
      deleteInvalidDocumentOfDriver DVC.UDYAMCertificate driverId
      QUdyam.create newUdyam
      QImage.updateVerificationStatusByIdAndType VALID imageId DVC.UDYAMCertificate
      whenJust req.tdsRate $ \rate ->
        QFOI.updateTdsRate (Just rate) driverId

approveAndUpdateLdcDocument :: Common.LDCApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdateLdcDocument req _mId _mOpCityId = do
  let documentId = Id req.documentId.getId
  document <- QCommonDriverOnboardingDocuments.findById documentId >>= fromMaybeM (DocumentNotFound documentId.getId)
  let updatedDocument = document {DCommonDoc.verificationStatus = VALID, DCommonDoc.rejectReason = Nothing}
  QCommonDriverOnboardingDocuments.updateByPrimaryKey updatedDocument
  whenJust req.tdsRate $ \rate ->
    whenJust document.driverId $ \driverId ->
      QFOI.updateTdsRate (Just rate) driverId

approveAndUpdateTanDocument :: Common.TANApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdateTanDocument req _mId _mOpCityId = do
  let documentId = Id req.documentId.getId
  document <- QCommonDriverOnboardingDocuments.findById documentId >>= fromMaybeM (DocumentNotFound documentId.getId)
  let updatedDocument = document {DCommonDoc.verificationStatus = VALID, DCommonDoc.rejectReason = Nothing}
  QCommonDriverOnboardingDocuments.updateByPrimaryKey updatedDocument
  whenJust req.tdsRate $ \rate ->
    whenJust document.driverId $ \driverId ->
      QFOI.updateTdsRate (Just rate) driverId

castReqTypeToDomain :: Common.PanType -> DPan.PanType
castReqTypeToDomain = \case
  Common.INDIVIDUAL -> DPan.INDIVIDUAL
  Common.BUSINESS -> DPan.BUSINESS

handleMandatoryDocRejection :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id DP.Person -> DVC.DocumentType -> Id DImage.Image -> Flow ()
handleMandatoryDocRejection _merchantId merchantOperatingCityId driverId docType imageId = do
  docConfigs <- getConfig (DocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, documentType = Just docType, vehicleCategory = Nothing}) (Just (CQDVC.findByMerchantOpCityIdAndDocumentType merchantOperatingCityId docType Nothing))
  -- isMandatory → verified, isMandatoryForEnabling → enabled (split).
  let blocksVerified = Kernel.Prelude.any (.isMandatory) docConfigs
      blocksEnabled = Kernel.Prelude.any (\cfg -> fromMaybe cfg.isMandatory cfg.isMandatoryForEnabling) docConfigs
  when (blocksEnabled || blocksVerified) $ do
    transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOperatingCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCityId.getId)
    let isVehicleDoc = docType `elem` SDO.defaultVehicleDocumentTypes
        separateEnablement = transporterConfig.separateDriverVehicleEnablement == Just True
        -- BOT splits enabled/verified; legacy disables both together.
        disableDriver =
          if transporterConfig.enableBotFlow == Just True
            then applyDriverDocInvalidation transporterConfig driverId blocksEnabled blocksVerified
            else QDriverInfo.updateEnabledVerifiedState (cast driverId) False (Just False) Nothing
    if isVehicleDoc
      then do
        -- Resolve the specific RC tied to the rejected document
        mbRcId <- resolveRcIdFromDocument docType imageId
        whenJust mbRcId $ \rcId -> do
          mbAssoc <- QRCAssoc.findActiveAssociationByRC rcId True
          whenJust mbAssoc $ \assoc -> when (assoc.driverId == driverId) $ do
            QRCAssoc.deactivateRCForDriver False driverId rcId
            QVehicle.deleteByDriverid driverId
            -- If not separate enablement, also disable the driver
            unless separateEnablement disableDriver
      else -- Driver doc rejected: disable the driver
        disableDriver

resolveRcIdFromDocument :: DVC.DocumentType -> Id DImage.Image -> Flow (Maybe (Id DRC.VehicleRegistrationCertificate))
resolveRcIdFromDocument docType imageId = case docType of
  DVC.VehicleRegistrationCertificate -> do
    mbRc <- QRC.findByImageId imageId
    pure $ (.id) <$> mbRc
  DVC.VehiclePermit -> do
    mbPermit <- QVPermit.findByImageId imageId
    pure $ (.rcId) <$> mbPermit
  DVC.VehicleFitnessCertificate -> do
    mbFc <- QFC.findByImageId imageId
    pure $ (.rcId) <$> mbFc
  DVC.VehicleInsurance -> do
    mbInsurance <- QVI.findByImageId imageId
    pure $ (.rcId) <$> mbInsurance
  DVC.VehiclePUC -> do
    mbPuc <- QVPUC.findByImageId imageId
    pure $ (.rcId) <$> mbPuc
  DVC.VehicleNOC -> do
    mbNoc <- QVNOC.findByImageId imageId
    pure $ (.rcId) <$> mbNoc
  _ -> do
    mbImage <- QImage.findById imageId
    pure $ Id <$> (mbImage >>= (.rcId))

runStatusEventSafely ::
  Text ->
  Maybe DP.Person ->
  Maybe DTC.TransporterConfig ->
  SStatus.StatusEvent ->
  Flow ()
runStatusEventSafely logTag mbPerson mbTransporterConfig event =
  void $
    withTryCatch
      logTag
      (void $ SStatus.processStatusEvent mbPerson mbTransporterConfig event)

getImageIdFromApproveDetails :: Common.ApproveDetails -> Maybe (Id Common.Image)
getImageIdFromApproveDetails = \case
  Common.DL req -> Just req.documentImageId
  Common.RC req -> Just req.documentImageId
  Common.VehicleInsurance req -> Just req.documentImageId
  Common.VehiclePUC req -> Just req.documentImageId
  Common.VehiclePermit req -> Just req.documentImageId
  Common.VehicleFitnessCertificate req -> Just req.documentImageId
  Common.VehicleInspectionForm req -> Just req.documentImageId
  Common.Pan req -> Just req.documentImageId
  Common.NOC req -> Just req.documentImageId
  Common.BusinessLicenseImg req -> Just req.documentImageId
  Common.Aadhaar req -> Just req.documentImageId
  Common.UploadProfile imgId -> Just imgId
  Common.ProfilePhoto imgId -> Just imgId
  Common.VehicleFrontImg imgId -> Just imgId
  Common.VehicleBackImg imgId -> Just imgId
  Common.VehicleRightImg imgId -> Just imgId
  Common.VehicleLeftImg imgId -> Just imgId
  Common.VehicleFrontInteriorImg imgId -> Just imgId
  Common.VehicleBackInteriorImg imgId -> Just imgId
  Common.OdometerImg imgId -> Just imgId
  Common.LocalResidenceProofApprove req -> Just req.documentImageId
  Common.PoliceVerificationCertificateImg imgId -> Just imgId
  Common.DriverVehicleNOCImg imgId -> Just imgId
  Common.SSNApprove _ -> Nothing
  Common.CommonDocument _ -> Nothing
  Common.UDYAMApprove req -> Just req.documentImageId
  Common.LDCApprove _ -> Nothing
  Common.GSTApprove req -> Just req.documentImageId
  Common.TANApprove _ -> Nothing

validatePersonForDocumentApproval :: Id DP.Person -> Id DM.Merchant -> Flow DP.Person
validatePersonForDocumentApproval personId merchantId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  blocked <- case person.role of
    role | DCommon.checkFleetOwnerRole role -> do
      foi <- QFOI.findByPrimaryKey personId >>= fromMaybeM (PersonNotFound personId.getId)
      pure foi.blocked
    _ -> do
      driverInfo <- QDriverInfo.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      pure driverInfo.blocked
  when blocked $ throwError AccountBlocked
  unless (merchantId == person.merchantId) $ throwError (PersonNotFound personId.getId)
  pure person

updateFleetOwnerInfoOnDocApproval :: DP.Person -> (Id DP.Person -> Flow ()) -> Flow ()
updateFleetOwnerInfoOnDocApproval person updateAction =
  when (DCommon.checkFleetOwnerRole person.role) $ updateAction person.id

approveAndUpdateLocalResidenceProof :: Common.LocalResidenceProofApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdateLocalResidenceProof req merchantId merchantOperatingCityId = do
  let imageId = Id req.documentImageId.getId
  image <- findApproveImage imageId
  QImage.updateVerificationStatusByIdAndType VALID imageId DVC.LocalResidenceProof
  now <- getCurrentTime
  person <- QPerson.findById image.personId >>= fromMaybeM (PersonNotFound image.personId.getId)
  if DCommon.checkFleetOwnerRole person.role
    then do
      fleetInfo <- QFOI.findByPrimaryKey image.personId >>= fromMaybeM (FleetOwnerNotFound image.personId.getId)
      QFOI.updateLocalAddressDetails
        (req.address <|> fleetInfo.address)
        (req.state <|> fleetInfo.addressState)
        ((DDriver.castFromCommon <$> req.proofDocumentType) <|> fleetInfo.addressDocumentType)
        image.personId
    else do
      mbInfo <- QDII.findByPrimaryKey image.personId
      case mbInfo of
        Just info ->
          QDII.updateByPrimaryKey info {DII.addressState = req.state <|> info.addressState, DII.addressDocumentType = (DDriver.castFromCommon <$> req.proofDocumentType) <|> info.addressDocumentType, DII.address = req.address <|> info.address, DII.updatedAt = now}
        Nothing ->
          QDII.create
            DII.DriverIdentityInfo
              { DII.driverId = image.personId,
                DII.merchantId = merchantId,
                DII.merchantOperatingCityId = merchantOperatingCityId,
                DII.addressState = req.state,
                DII.addressDocumentType = DDriver.castFromCommon <$> req.proofDocumentType,
                DII.address = req.address,
                DII.courtRecord = Nothing,
                DII.nomineeDob = Nothing,
                DII.nomineeName = Nothing,
                DII.nomineeRelationship = Nothing,
                DII.createdAt = now,
                DII.updatedAt = now
              }

handleApproveRequest :: Common.ApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
handleApproveRequest approveReq merchantId merchantOperatingCityId =
  doApproveWithRevert (getImageIdFromApproveDetails approveReq) $
    case approveReq of
      Common.DL dlReq -> approveAndUpdateDL merchantId merchantOperatingCityId dlReq
      Common.RC rcApproveReq -> approveAndUpdateRC rcApproveReq merchantId merchantOperatingCityId
      Common.VehicleInsurance vInsuranceReq -> approveAndUpdateInsurance vInsuranceReq merchantId merchantOperatingCityId
      Common.VehiclePUC pucReq -> approveAndUpdatePUC pucReq merchantId merchantOperatingCityId
      Common.VehiclePermit permitReq -> approveAndUpdatePermit permitReq merchantId merchantOperatingCityId
      Common.VehicleFitnessCertificate fitnessReq -> approveAndUpdateFitnessCertificate fitnessReq merchantId merchantOperatingCityId
      Common.UploadProfile imageId -> QImage.updateVerificationStatusByIdAndType VALID (Id imageId.getId) DVC.UploadProfile
      Common.ProfilePhoto imageId -> QImage.updateVerificationStatusByIdAndType VALID (Id imageId.getId) DVC.ProfilePhoto
      Common.VehicleInspectionForm req -> do
        QImage.updateVerificationStatusByIdAndType VALID (Id req.documentImageId.getId) DVC.VehicleInspectionForm
        QImage.updateDocumentExpiry req.dateOfExpiry (Id req.documentImageId.getId)
      Common.SSNApprove ssnNum -> do
        ssnEnc <- encrypt ssnNum
        QSSN.updateVerificationStatusAndReasonBySSN VALID Nothing (ssnEnc & hash)
      Common.NOC req -> approveAndUpdateNOC req merchantId merchantOperatingCityId
      Common.BusinessLicenseImg req -> approveAndUpdateBusinessLicense req merchantId merchantOperatingCityId
      Common.Pan req -> approveAndUpdatePan req merchantId merchantOperatingCityId
      Common.CommonDocument req -> approveAndUpdateCommonDocument req merchantId merchantOperatingCityId
      Common.Aadhaar req -> approveAndUpdateAadhaar req merchantId merchantOperatingCityId
      Common.VehicleFrontImg imageId -> QImage.updateVerificationStatusByIdAndType VALID (Id imageId.getId) DVC.VehicleFront
      Common.VehicleBackImg imageId -> QImage.updateVerificationStatusByIdAndType VALID (Id imageId.getId) DVC.VehicleBack
      Common.VehicleRightImg imageId -> QImage.updateVerificationStatusByIdAndType VALID (Id imageId.getId) DVC.VehicleRight
      Common.VehicleLeftImg imageId -> QImage.updateVerificationStatusByIdAndType VALID (Id imageId.getId) DVC.VehicleLeft
      Common.VehicleFrontInteriorImg imageId -> QImage.updateVerificationStatusByIdAndType VALID (Id imageId.getId) DVC.VehicleFrontInterior
      Common.VehicleBackInteriorImg imageId -> QImage.updateVerificationStatusByIdAndType VALID (Id imageId.getId) DVC.VehicleBackInterior
      Common.OdometerImg imageId -> QImage.updateVerificationStatusByIdAndType VALID (Id imageId.getId) DVC.Odometer
      Common.LocalResidenceProofApprove req -> approveAndUpdateLocalResidenceProof req merchantId merchantOperatingCityId
      Common.PoliceVerificationCertificateImg imageId -> QImage.updateVerificationStatusByIdAndType VALID (Id imageId.getId) DVC.PoliceVerificationCertificate
      Common.DriverVehicleNOCImg imageId -> QImage.updateVerificationStatusByIdAndType VALID (Id imageId.getId) DVC.DriverVehicleNOC
      Common.UDYAMApprove req -> approveAndUpdateUdyamDocument req merchantId merchantOperatingCityId
      Common.LDCApprove req -> approveAndUpdateLdcDocument req merchantId merchantOperatingCityId
      Common.GSTApprove gstReq -> approveGST gstReq merchantId merchantOperatingCityId
      Common.TANApprove req -> approveAndUpdateTanDocument req merchantId merchantOperatingCityId

-- | On any exception from the approval action, delete the uploaded image and re-throw
-- so the caller receives an error response. withTryCatch ensures the error is logged.
doApproveWithRevert :: Maybe (Id Common.Image) -> Flow () -> Flow ()
doApproveWithRevert mbImageId action = do
  result <- withTryCatch "handleApproveRequest" action
  case result of
    Right () -> pure ()
    Left err -> do
      whenJust mbImageId $ \imgId -> QImage.deleteById (Id imgId.getId)
      throwM err

handleRejectRequest :: Common.RejectDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
handleRejectRequest rejectReq merchantId merchantOperatingCityId = do
  case rejectReq of
    Common.SSNReject ssnRejectReq -> rejectSSNAndSendNotification ssnRejectReq merchantOperatingCityId
    Common.ImageDocuments imageRejectReq -> do
      let imageId = Id imageRejectReq.documentImageId.getId
          reason = imageRejectReq.reason
          rejectImage imgId = QImage.updateVerificationStatusAndFailureReason INVALID (ImageNotValid reason) imgId
          imageOnlyRejectTypes = [DVC.ProfilePhoto, DVC.UploadProfile, DVC.VehicleInspectionForm, DVC.VehicleFront, DVC.VehicleBack, DVC.VehicleRight, DVC.VehicleLeft, DVC.VehicleFrontInterior, DVC.VehicleBackInterior, DVC.Odometer, DVC.PoliceVerificationCertificate, DVC.DriverVehicleNOC] -- TODO Jitu: Fetch through config (onlyImageVerificationStatusLookupRequired)
      image <- findApproveImage imageId
      case image.imageType of
        DVC.VehicleFitnessCertificate -> do
          rejectImage imageId
          QFC.updateVerificationStatus INVALID imageId
        DVC.VehicleInsurance -> do
          rejectImage imageId
          vInsurance <- QVI.findByImageId imageId
          whenJust vInsurance $ \_ ->
            QVI.updateVerificationStatusAndRejectReason INVALID reason imageId
        DVC.DriverLicense -> do
          mbDl <- QDL.findByImageId imageId
          mbResolvedDl <- case mbDl of
            Just _ -> pure mbDl
            Nothing -> QDL.findByDriverId image.personId
          case mbResolvedDl of
            Nothing -> logWarning $ "DL not found for image " <> imageId.getId
            Just dl -> do
              QDL.updateDocImageAndStatusById dl.id imageId INVALID reason
              rejectImage dl.documentImageId1
              whenJust dl.documentImageId2 rejectImage
          rejectImage imageId
        DVC.VehicleRegistrationCertificate -> do
          mbRc <- QRC.findByImageId imageId
          mbResolvedRc <- case mbRc of
            Just _ -> pure mbRc
            Nothing -> case image.rcId of
              Just rcIdRaw -> QRC.findById (Id rcIdRaw)
              Nothing -> pure Nothing
          whenJust mbResolvedRc $ \rc ->
            QRC.updateDocImageAndStatusById rc.id imageId INVALID reason
          rejectImage imageId
        DVC.VehiclePermit -> do
          rejectImage imageId
          QVPermit.updateVerificationStatusByImageId INVALID imageId
        DVC.VehiclePUC -> do
          rejectImage imageId
          QVPUC.updateVerificationStatusByImageId INVALID imageId
        DVC.VehicleNOC -> do
          rejectImage imageId
          QVNOC.updateVerificationStatusByImageId INVALID imageId
        DVC.BusinessLicense -> do
          rejectImage imageId
          QBL.updateVerificationStatusByImageId INVALID imageId
        DVC.PanCard -> do
          rejectImage imageId
          QPan.updateVerificationStatusAndRejectReason INVALID (Just reason) imageId
        DVC.AadhaarCard -> do
          rejectImage imageId
          QAadhaarCard.updateVerificationStatusAndRejectReason INVALID (Just reason) image.personId
        DVC.GSTCertificate -> do
          rejectImage imageId
          QGstin.updateVerificationStatusAndRejectReason INVALID (Just reason) imageId
        DVC.LocalResidenceProof -> do
          rejectImage imageId
          person <- QPerson.findById image.personId >>= fromMaybeM (PersonNotFound image.personId.getId)
          if DCommon.checkFleetOwnerRole person.role
            then QFOI.updateLocalAddressDetails Nothing Nothing Nothing image.personId
            else QDII.updateLocalAddressDetails Nothing Nothing Nothing image.personId
        DVC.UDYAMCertificate -> do
          rejectImage imageId
          mbUdyam <- QUdyam.findByImageId (Just imageId)
          whenJust mbUdyam $ \udyam ->
            QUdyam.updateVerificationStatusAndRejectReason INVALID (Just reason) udyam.id
        docType
          | docType `elem` imageOnlyRejectTypes -> rejectImage imageId
        _ -> throwError (InternalError "Unknown Config in reject update document")
      handleMandatoryDocRejection merchantId merchantOperatingCityId image.personId image.imageType imageId
      mbDriver <- QDriver.findById image.personId
      case mbDriver of
        Nothing -> logWarning $ "Driver not found for rejection notification, skipping: " <> image.personId.getId
        Just driver -> do
          let docType = show image.imageType
          void $
            withTryCatch "ImageDocuments:sendRejectionNotification" $
              sendDocumentRejectionNotification merchantOperatingCityId docType reason driver
    Common.CommonDocumentReject commonRejectReq -> do
      let documentId = Id commonRejectReq.documentId.getId
      document <- QCommonDriverOnboardingDocuments.findById documentId >>= fromMaybeM (DocumentNotFound documentId.getId)
      rejectAndUpdateCommonDocument commonRejectReq merchantOperatingCityId
      -- LDC reject also resets the fleet owner's TDS: explicit rate from the request, else the configured default (mirrors approve)
      when (document.documentType == DVC.LDCCertificate) $ do
        transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOperatingCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCityId.getId)
        let mbTdsRate = commonRejectReq.tdsRate <|> ((.rate) <$> transporterConfig.taxConfig.defaultTdsRate)
        whenJust document.driverId $ \driverId ->
          QFOI.updateTdsRate mbTdsRate driverId
      whenJust document.driverId $ \driverId -> do
        mbDriver <- QDriver.findById driverId
        case mbDriver of
          Nothing -> logWarning $ "Driver not found for rejection notification, skipping: " <> driverId.getId
          Just driver -> do
            let merchantOpCityId = document.merchantOperatingCityId
                docType = show document.documentType
                reason = commonRejectReq.reason
            void $
              withTryCatch "CommonDocumentReject:sendRejectionNotification" $
                sendDocumentRejectionNotification merchantOpCityId docType reason driver
    Common.InspectionHubReject inspectionHubRejectReq -> do
      let requestId = Id inspectionHubRejectReq.requestId :: Id DOHR.OperationHubRequests
      request <- QOHR.findByPrimaryKey requestId >>= fromMaybeM (InternalError "Inspection hub request not found")
      unless (request.requestStatus == DOHR.APPROVED) $
        throwError (InvalidRequest "Inspection hub request is not in APPROVED state")
      now <- getCurrentTime
      QOHR.updateByPrimaryKey request {DOHR.requestStatus = DOHR.REJECTED, DOHR.remarks = Just inspectionHubRejectReq.reason, DOHR.updatedAt = now}
  where
    rejectSSNAndSendNotification req _merchantOpCityId = do
      ssnEnc <- encrypt req.ssn
      QSSN.updateVerificationStatusAndReasonBySSN INVALID (Just req.reason) (ssnEnc & hash)
      ssnEntry <- QSSN.findBySSN (ssnEnc & hash) >>= fromMaybeM (InternalError "SSN not found by ssn no")
      driver <- QDriver.findById ssnEntry.driverId >>= fromMaybeM (PersonNotFound ssnEntry.driverId.getId)
      let docType = "SSN"
          reason = req.reason
      sendDocumentRejectionNotification _merchantOpCityId docType reason driver

notificationType :: FCM.FCMNotificationType
notificationType = FCM.DOCUMENT_INVALID

replacePlaceholders :: Text -> Text -> Text -> Text
replacePlaceholders translatedDocType reason template = T.replace "{#docType#}" "" $ T.replace "{#documentType#}" translatedDocType $ T.replace "{#reason#}" reason template

translateDocumentType :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Lang.Language -> Text -> m Text
translateDocumentType language docType = do
  let translationKey = "DOC_TYPE_" <> docType
  mbTranslation <- getConfig (TranslationDimensions {merchantOperatingCityId = Nothing, messageKey = translationKey, language = Just language}) (Just (QTranslations.findByErrorAndLanguage translationKey language))
  case mbTranslation of
    Just trans -> pure trans.message
    Nothing
      | language /= Lang.ENGLISH -> do
        mbEnglishTranslation <- getConfig (TranslationDimensions {merchantOperatingCityId = Nothing, messageKey = translationKey, language = Just Lang.ENGLISH}) (Just (QTranslations.findByErrorAndLanguage translationKey Lang.ENGLISH))
        pure $ maybe docType (.message) mbEnglishTranslation
      | otherwise -> pure docType

fetchPushNotificationTemplates :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Lang.Language -> Maybe Lang.Language -> m (Text, Text)
fetchPushNotificationTemplates merchantOpCityId language mbLanguage = do
  mbPN <- CPN.findMatchingMerchantPN merchantOpCityId "DOCUMENT_INVALID" Nothing Nothing mbLanguage Nothing
  case mbPN of
    Just pn -> pure (pn.title, pn.body)
    Nothing -> do
      mbMerchantMessage <- QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId DMM.DOCUMENT_INVALID Nothing Nothing
      case mbMerchantMessage of
        Just mm -> pure ("", mm.message)
        Nothing -> fetchFallbackTemplates merchantOpCityId language

fetchSmsTemplates :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Lang.Language -> m (Text, Text)
fetchSmsTemplates merchantOpCityId language = do
  mbMerchantMessage <- QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId DMM.DOCUMENT_INVALID Nothing Nothing
  case mbMerchantMessage of
    Just mm -> pure ("", mm.message)
    Nothing -> fetchFallbackTemplates merchantOpCityId language

fetchFallbackTemplates :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Lang.Language -> m (Text, Text)
fetchFallbackTemplates merchantOpCityId language = do
  mbTitleTranslation <- getConfig (TranslationDimensions {merchantOperatingCityId = Just merchantOpCityId.getId, messageKey = "DOCUMENT_INVALID_TITLE", language = Just language}) (Just (QTranslations.findByErrorAndLanguage "DOCUMENT_INVALID_TITLE" language))
  mbBodyTranslation <- getConfig (TranslationDimensions {merchantOperatingCityId = Just merchantOpCityId.getId, messageKey = "DOCUMENT_INVALID_BODY", language = Just language}) (Just (QTranslations.findByErrorAndLanguage "DOCUMENT_INVALID_BODY" language))
  case (mbTitleTranslation, mbBodyTranslation) of
    (Just titleTrans, Just bodyTrans) -> pure (titleTrans.message, bodyTrans.message)
    _ -> pure ("", "")

constructTitle :: Text -> Text -> Text
constructTitle translatedDocType template
  | T.null template = "Attention: Your " <> translatedDocType <> " is invalid."
  | otherwise = template

constructBody :: Text -> Text -> Text -> Text
constructBody translatedDocType reason template
  | T.null template = translatedDocType <> " rejected - " <> reason <> ". Please reupload the correct document."
  | otherwise = template

buildPushMessages :: Text -> Text -> (Text, Text) -> (Text, Text)
buildPushMessages translatedDocType reason (titleTemplate, bodyTemplate) =
  let title = constructTitle translatedDocType $ replacePlaceholders translatedDocType reason titleTemplate
      body = constructBody translatedDocType reason $ replacePlaceholders translatedDocType reason bodyTemplate
   in (title, body)

buildSmsMessage :: Text -> Text -> (Text, Text) -> Text
buildSmsMessage translatedDocType reason (titleTemplate, bodyTemplate) =
  let smsTemplate = if T.null bodyTemplate then titleTemplate else bodyTemplate
   in constructBody translatedDocType reason $ replacePlaceholders translatedDocType reason smsTemplate

sendRejectDocumentSmsWithBody :: (CacheFlow m r, EsqDBFlow m r, ServiceFlow m r, HasFlowEnv m r '["smsCfg" ::: SmsConfig]) => Id DMOC.MerchantOperatingCity -> Text -> DP.Person -> m ()
sendRejectDocumentSmsWithBody merchantOpCityId smsBody driver = do
  mbMobileNumber <- mapM decrypt driver.mobileNumber
  case mbMobileNumber of
    Nothing -> logWarning $ "Cannot send rejection SMS: mobile number missing for driver " <> driver.id.getId
    Just mobileNumber -> do
      logDebug $ "Sending SMS - Driver: " <> driver.id.getId <> ", Language: " <> show driver.language <> ", Message: " <> smsBody
      smsCfg <- asks (.smsCfg)
      let countryCode = fromMaybe "+91" driver.mobileCountryCode
          phoneNumber = countryCode <> mobileNumber
          sender = smsCfg.sender
          templateId = "" -- no dedicated template; send plain text
      Sms.sendSMS driver.merchantId merchantOpCityId (Sms.SendSMSReq smsBody phoneNumber sender templateId Nothing) >>= Sms.checkSmsResult

isDashboardSmsEnabled :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m Bool
isDashboardSmsEnabled merchantOpCityId =
  getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing))
    >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
    <&> (.enableDashboardSms)

sendDocumentRejectionNotification :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, ServiceFlow m r, HasFlowEnv m r '["smsCfg" ::: SmsConfig], Redis.HedisLTSFlowEnv r) => Id DMOC.MerchantOperatingCity -> Text -> Text -> DP.Person -> m ()
sendDocumentRejectionNotification merchantOpCityId docType reason driver = do
  let language = fromMaybe Lang.ENGLISH driver.language
  translatedDocType <- translateDocumentType language docType
  pushTemplates <- fetchPushNotificationTemplates merchantOpCityId language driver.language
  smsTemplates <- fetchSmsTemplates merchantOpCityId language
  let (title, body) = buildPushMessages translatedDocType reason pushTemplates
      smsBody = buildSmsMessage translatedDocType reason smsTemplates
  Notify.notifyDriver merchantOpCityId notificationType title body driver driver.deviceToken
  smsEnabled <- isDashboardSmsEnabled merchantOpCityId
  when smsEnabled $ do
    sendRejectDocumentSmsWithBody merchantOpCityId smsBody driver

postDriverRegistrationDocumentsUpdate :: ShortId DM.Merchant -> Context.City -> Common.UpdateDocumentRequest -> Flow Common.UpdateDocumentResp
postDriverRegistrationDocumentsUpdate _merchantShortId _opCity _req = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  let updateAndFetchEnabled mbPersonId = case mbPersonId of
        Just personId -> do
          result <-
            withTryCatch "updateAndFetchEnabled:PersonDocChangedEvent" $
              fromMaybe False <$> SStatus.processStatusEvent Nothing Nothing (SStatus.PersonDocChangedEvent personId)
          case result of
            Right isEnabled -> pure isEnabled
            Left e -> do
              logError $ "updateAndFetchEnabled:PersonDocChangedEvent failed for personId=" <> personId.getId <> ", error=" <> show e <> " — falling back to current enabled flag without refreshing docs_verification_status"
              person <- QDriver.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
              if DCommon.checkFleetOwnerRole person.role
                then do
                  fleetOwnerInfo <- QFOI.findByPrimaryKey personId >>= fromMaybeM (PersonNotFound personId.getId)
                  pure fleetOwnerInfo.enabled
                else do
                  driverInfo <- QDriverInfo.findById personId >>= fromMaybeM DriverInfoNotFound
                  pure driverInfo.enabled
        Nothing -> pure False
      refreshVehicleDocsStatus mbRcId =
        whenJust mbRcId $ \rcId ->
          runStatusEventSafely
            "refreshVehicleDocsVerificationStatusForRC:postDriverRegistrationDocumentsUpdate"
            Nothing
            Nothing
            (SStatus.VehicleDocChangedEvent rcId)
      resolvePersonIdViaRc mbRcId = do
        case mbRcId of
          Nothing -> pure Nothing
          Just rcId -> do
            -- Try driver-RC association first
            mbDriverAssoc <- QRCAssoc.findActiveAssociationByRC rcId True
            case mbDriverAssoc of
              Just assoc -> pure $ Just assoc.driverId
              Nothing -> do
                -- Fallback: Try fleet-RC association for fleet-uploaded vehicles
                rc <- QRC.findById rcId
                case rc of
                  Just rcRecord -> do
                    -- Get person who uploaded the RC image
                    mbImage <- QImage.findById rcRecord.documentImageId
                    case mbImage of
                      Just image -> pure $ Just image.personId
                      Nothing -> pure Nothing
                  Nothing -> pure Nothing
  let mkUpdateResp mbPersonId enabled =
        Common.UpdateDocumentResp
          { result = "Success",
            personId = cast @DP.Person @Common.Driver <$> mbPersonId,
            enabled = enabled
          }
      processPostUpdate mbPersonIdRaw mbRcId = do
        mbPersonId <- maybe (resolvePersonIdViaRc mbRcId) (pure . Just) mbPersonIdRaw
        refreshVehicleDocsStatus mbRcId
        enabled <- updateAndFetchEnabled mbPersonId
        pure $ mkUpdateResp mbPersonId enabled
  case _req of
    Common.Approve approveReq -> do
      handleApproveRequest approveReq merchant.id merchantOpCityId
      whenJust (getImageIdFromApproveDetails approveReq) $ \imgId -> do
        mbImage <- QImage.findById (Id imgId.getId)
        whenJust mbImage $ \image -> do
          person <- QPerson.findById image.personId >>= fromMaybeM (PersonNotFound image.personId.getId)
          when (DCommon.checkFleetOwnerRole person.role) $
            void $ DRegistrationV2.enableFleetIfPossible image.personId Nothing (DRegistrationV2.castRoleToFleetType person.role) merchantOpCityId Nothing
      mbPersonIdRaw <- getApproveTargetPersonId approveReq
      mbRcId <- getApproveTargetRcId approveReq
      processPostUpdate mbPersonIdRaw mbRcId
    Common.Reject rejectReq -> do
      mbPersonIdRaw <- getRejectTargetPersonId rejectReq
      whenJust mbPersonIdRaw $ \personId -> do
        mbDriverInfo <- QDriverInfo.findById personId
        whenJust mbDriverInfo $ \driverInfo ->
          when driverInfo.onRide $
            throwError (InvalidRequest "Cannot reject document for a driver who is currently on a ride")
      handleRejectRequest rejectReq merchant.id merchantOpCityId
      mbRcId <- getRejectTargetRcId rejectReq
      processPostUpdate mbPersonIdRaw mbRcId
  where
    isVehicleDocType :: DVC.DocumentType -> Bool
    isVehicleDocType docType = docType `elem` SDO.defaultVehicleDocumentTypes

    getApproveTargetRcId :: Common.ApproveDetails -> Flow (Maybe (Id DRC.VehicleRegistrationCertificate))
    getApproveTargetRcId = \case
      Common.RC req -> resolveRcIdFromDocument DVC.VehicleRegistrationCertificate (Id req.documentImageId.getId)
      Common.VehicleInsurance req -> resolveRcIdFromDocument DVC.VehicleInsurance (Id req.documentImageId.getId)
      Common.VehiclePUC req -> resolveRcIdFromDocument DVC.VehiclePUC (Id req.documentImageId.getId)
      Common.VehiclePermit req -> resolveRcIdFromDocument DVC.VehiclePermit (Id req.documentImageId.getId)
      Common.VehicleFitnessCertificate req -> resolveRcIdFromDocument DVC.VehicleFitnessCertificate (Id req.documentImageId.getId)
      Common.NOC req -> resolveRcIdFromDocument DVC.VehicleNOC (Id req.documentImageId.getId)
      Common.CommonDocument req -> do
        mbDoc <- QCommonDriverOnboardingDocuments.findById (Id req.documentId.getId)
        case mbDoc of
          Just doc | isVehicleDocType doc.documentType ->
            case doc.documentImageId of
              Just imageId -> resolveRcIdFromDocument doc.documentType imageId
              Nothing -> pure Nothing
          _ -> pure Nothing
      req
        | Just imgId <- getImageIdFromApproveDetails req -> do
          mbImage <- QImage.findById (Id imgId.getId)
          case mbImage of
            Just image | isVehicleDocType image.imageType -> resolveRcIdFromDocument image.imageType (Id imgId.getId)
            _ -> pure Nothing
      _ -> pure Nothing

    getRejectTargetRcId :: Common.RejectDetails -> Flow (Maybe (Id DRC.VehicleRegistrationCertificate))
    getRejectTargetRcId = \case
      Common.ImageDocuments imgReq -> do
        let imageId = Id imgReq.documentImageId.getId
        mbImage <- QImage.findById imageId
        case mbImage of
          Just image | isVehicleDocType image.imageType -> resolveRcIdFromDocument image.imageType imageId
          _ -> pure Nothing
      Common.CommonDocumentReject req -> do
        mbDoc <- QCommonDriverOnboardingDocuments.findById (Id req.documentId.getId)
        case mbDoc of
          Just doc | isVehicleDocType doc.documentType ->
            case doc.documentImageId of
              Just imageId -> resolveRcIdFromDocument doc.documentType imageId
              Nothing -> pure Nothing
          _ -> pure Nothing
      _ -> pure Nothing

    getApproveTargetPersonId :: Common.ApproveDetails -> Flow (Maybe (Id DP.Person))
    getApproveTargetPersonId = \case
      Common.CommonDocument req -> do
        mbDoc <- QCommonDriverOnboardingDocuments.findById (Id req.documentId.getId)
        case mbDoc of
          Just doc -> case doc.driverId of
            Just personId -> pure (Just personId)
            Nothing -> do
              case doc.documentImageId of
                Just imageId -> do
                  mbImage <- QImage.findById imageId
                  pure $ (.personId) <$> mbImage
                Nothing -> pure Nothing
          Nothing -> pure Nothing
      Common.SSNApprove ssnNum -> do
        ssnEnc <- encrypt ssnNum
        mbSsn <- QSSN.findBySSN (ssnEnc & hash)
        pure $ (.driverId) <$> mbSsn
      Common.UDYAMApprove req -> do
        mbImage <- QImage.findById (Id req.documentImageId.getId)
        pure $ (.personId) <$> mbImage
      Common.LDCApprove req -> do
        mbDoc <- QCommonDriverOnboardingDocuments.findById (Id req.documentId.getId)
        pure $ join ((.driverId) <$> mbDoc)
      Common.TANApprove req -> do
        mbDoc <- QCommonDriverOnboardingDocuments.findById (Id req.documentId.getId)
        pure $ join ((.driverId) <$> mbDoc)
      req
        | Just imgId <- getImageIdFromApproveDetails req -> do
          mbImage <- QImage.findById (Id imgId.getId)
          pure $ (.personId) <$> mbImage
      _ -> pure Nothing

    getRejectTargetPersonId :: Common.RejectDetails -> Flow (Maybe (Id DP.Person))
    getRejectTargetPersonId = \case
      Common.CommonDocumentReject req -> do
        let documentId = Id req.documentId.getId
        mbDocument <- QCommonDriverOnboardingDocuments.findById documentId
        case mbDocument of
          Just doc -> case doc.driverId of
            Just personId -> pure (Just personId)
            Nothing -> do
              case doc.documentImageId of
                Just imageId -> do
                  mbImage <- QImage.findById imageId
                  pure $ (.personId) <$> mbImage
                Nothing -> pure Nothing
          Nothing -> pure Nothing
      Common.ImageDocuments imgReq -> do
        let imageId = Id imgReq.documentImageId.getId
        mbImage <- QImage.findById imageId
        pure $ (.personId) <$> mbImage
      Common.SSNReject req -> do
        ssnEnc <- encrypt req.ssn
        mbSsnEntry <- QSSN.findBySSN (ssnEnc & hash)
        pure $ (.driverId) <$> mbSsnEntry
      Common.InspectionHubReject req -> do
        let requestId = Id req.requestId :: Id DOHR.OperationHubRequests
        mbRequest <- QOHR.findByPrimaryKey requestId
        pure $ (.driverId) =<< mbRequest

approveGST :: Common.GSTApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveGST req merchantId merchantOperatingCityId = do
  let fleetOwnerId = cast req.fleetOwnerId :: Id DP.Person
      imageId = Id req.documentImageId.getId
  gstImage <- findApproveImage imageId
  when (gstImage.personId /= fleetOwnerId) $
    throwError (InvalidRequest "Image does not belong to the provided fleet owner")
  person <- validatePersonForDocumentApproval fleetOwnerId merchantId
  mbGstByImage <- QGstin.findByImageId imageId
  -- Fallback for re-upload: GST row's documentImageId1 still points to the prior (rejected) image.
  -- Recover the fleet owner's existing GST row via driverId lookup.
  mbGstin <- case mbGstByImage of
    Just _ -> pure mbGstByImage
    Nothing -> QGstin.findByDriverId fleetOwnerId
  -- Common approve-time checks: number mismatch, document linked to another driver, driver already linked
  validateDocumentApprovalChecks DVC.GSTCertificate (Just req.gstNumber) fleetOwnerId (GstApproveData <$> mbGstin)
  QImage.updateVerificationStatusByIdAndType VALID imageId DVC.GSTCertificate
  gstEnc <- encrypt req.gstNumber
  now <- getCurrentTime
  uuid <- generateGUID
  case mbGstin of
    Just gstin -> do
      let updatedGstin =
            gstin
              { DGstin.gstin = gstEnc,
                DGstin.verificationStatus = VALID,
                DGstin.verifiedBy = Just DPan.DASHBOARD,
                DGstin.updatedAt = now,
                DGstin.driverId = fleetOwnerId,
                DGstin.documentImageId1 = imageId
              }
      -- Clean up stale INVALID rows, then upsert (the fleet owner's own row may be among the deleted)
      deleteInvalidDocumentOfDriver DVC.GSTCertificate fleetOwnerId
      QGstin.upsertGstinRecord updatedGstin
    Nothing -> do
      let gstin =
            DGstin.DriverGstin
              { DGstin.id = uuid,
                DGstin.driverId = fleetOwnerId,
                DGstin.documentImageId1 = imageId,
                DGstin.documentImageId2 = Nothing,
                DGstin.gstin = gstEnc,
                DGstin.verificationStatus = VALID,
                DGstin.rejectReason = Nothing,
                DGstin.verifiedBy = Just DPan.DASHBOARD,
                DGstin.driverName = Nothing,
                DGstin.legalName = Nothing,
                DGstin.tradeName = Nothing,
                DGstin.address = Nothing,
                DGstin.constitutionOfBusiness = Nothing,
                DGstin.dateOfLiability = Nothing,
                DGstin.isProvisional = Nothing,
                DGstin.panNumber = Nothing,
                DGstin.typeOfRegistration = Nothing,
                DGstin.validFrom = Nothing,
                DGstin.validUpto = Nothing,
                DGstin.merchantId = Just merchantId,
                DGstin.merchantOperatingCityId = Just merchantOperatingCityId,
                DGstin.createdAt = now,
                DGstin.updatedAt = now,
                pincode = Nothing,
                stateName = Nothing
              }
      deleteInvalidDocumentOfDriver DVC.GSTCertificate fleetOwnerId
      QGstin.create gstin
  updateFleetOwnerInfoOnDocApproval person $ \personId ->
    QFOIE.updateGstImage (Just gstEnc) (Just imageId.getId) personId

convertVerifyOtp :: AadhaarVerificationResp -> Common.GenerateAadhaarOtpRes
convertVerifyOtp AadhaarVerificationResp {..} = Common.GenerateAadhaarOtpRes {..}

convertSubmitOtp :: AadhaarOtpVerifyRes -> Common.VerifyAadhaarOtpRes
convertSubmitOtp AadhaarOtpVerifyRes {..} = Common.VerifyAadhaarOtpRes {..}

getDriverRegistrationVerificationStatus :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> UTCTime -> UTCTime -> Int -> Int -> Common.DocumentType -> Common.ServiceType -> Flow Common.VerificationStatusListResponse
getDriverRegistrationVerificationStatus _merchantShortId _opCity driverId fromDate toDate limit offset documentType serviceType = do
  let personId = cast @Common.Driver @DP.Person driverId
  _ <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  logDebug $ "serviceType: " <> show serviceType
  logDebug $ "documentType: " <> show documentType
  logDebug $ "personId: " <> show personId
  verificationStatuses <- case serviceType of
    Common.HyperVerge -> do
      entries <- HVQuery.findLatestByDriverIdAndDocType limit offset personId (mapDocumentType documentType) fromDate toDate
      pure $ map (\entry -> convertToVerificationStatusItem entry.status entry.hypervergeResponse entry.retryCount entry.requestId entry.createdAt) entries
    Common.Idfy -> do
      entries <- IDQuery.findLatestByDriverIdAndDocType limit offset personId (mapDocumentType documentType) fromDate toDate
      pure $ map (\entry -> convertToVerificationStatusItem entry.status entry.idfyResponse entry.retryCount entry.requestId entry.createdAt) entries
  logDebug $ "entries IDs: " <> show (map (\entry -> entry.requestId) verificationStatuses)
  pure Common.VerificationStatusListResponse {verificationStatuses = verificationStatuses}
  where
    convertToVerificationStatusItem status verificationMessage retryCount requestId createdAt =
      Common.VerificationStatusItem
        { documentType = documentType,
          status = status,
          verificationMessage = verificationMessage,
          retryCount = fromMaybe 0 retryCount,
          requestId = Just requestId,
          createdAt = createdAt
        }

-- | Map Common.DocumentType to Reminder.DocumentType
-- Only maps document types that support reminder triggers
mapDocumentTypeToReminderType :: Common.DocumentType -> Maybe DVC.DocumentType
mapDocumentTypeToReminderType = \case
  Common.DriverLicense -> Just DVC.DriverLicense
  Common.VehicleRegistrationCertificate -> Just DVC.VehicleRegistrationCertificate
  Common.VehicleInsuranceImage -> Just DVC.VehicleInsurance
  Common.VehiclePermitImage -> Just DVC.VehiclePermit
  Common.VehiclePUCImage -> Just DVC.VehiclePUC
  Common.VehicleFitnessCertificateImage -> Just DVC.VehicleFitnessCertificate
  Common.BusinessLicense -> Just DVC.BusinessLicense
  Common.InspectionHub -> Just DVC.InspectionHub
  Common.DriverInspectionHub -> Just DVC.DriverInspectionHub
  Common.TrainingFormImage -> Just DVC.TrainingForm
  _ -> Nothing -- Document types that don't support reminder triggers

postDriverRegistrationTriggerReminder ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Maybe Text ->
  Common.TriggerReminderReq ->
  Flow APISuccess
postDriverRegistrationTriggerReminder merchantShortId opCity driverId_ mbRequestorId req@Common.TriggerReminderReq {..} = do
  runRequestValidation Common.validateTriggerReminderReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)

  -- Load driver (and requestor when present) in one query when mbRequestorId is Just; validate driver ownership and association
  let driverPersonId = cast @Common.Driver @DP.Person driverId_
  void $
    case mbRequestorId of
      Nothing -> do
        d <- runInReplica $ QPerson.findById driverPersonId >>= fromMaybeM (PersonNotFound driverPersonId.getId)
        unless (d.merchantId == merchant.id && d.merchantOperatingCityId == merchantOpCityId) $
          throwError (InvalidRequest "Driver does not belong to the specified merchant and operating city")
        pure ()
      Just requestorId -> do
        entities <- QPerson.findAllByPersonIdsAndMerchantOpsCityId [Id requestorId, driverPersonId] merchantOpCityId
        d <- find (\e -> e.id == driverPersonId) entities & fromMaybeM (PersonNotFound driverPersonId.getId)
        whenJust (find (\e -> e.id == Id requestorId) entities) $ \requestor -> do
          isValid <- DDriver.isAssociationBetweenTwoPerson requestor d
          unless isValid $ throwError (InvalidRequest "Only fleet owners, operators linked to the driver, or admins can trigger reminders")
        pure ()

  reminderDocumentType <- maybe (throwError $ InvalidRequest $ "Document type " <> show documentType <> " does not support reminder triggers") pure $ mapDocumentTypeToReminderType documentType
  createReminder reminderDocumentType driverPersonId merchant.id merchantOpCityId Nothing dueDate intervals
  pure Success

postDriverRegistrationVerifyBankAccount :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.VerifyBankAccountReq -> Flow Kernel.External.Verification.Interface.Types.VerifyAsyncResp
postDriverRegistrationVerifyBankAccount merchantShortId opCity driverId_ req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person driverId_
  BankAccountVerification.verifyBankAccount (personId, merchant.id, merchantOpCityId) $
    BankAccountVerification.DriverBankAccountVerifyReq
      { bankAccountNo = req.bankAccountNo,
        bankIfscCode = req.bankIfscCode,
        nfVerification = False
      }

getDriverRegistrationInfoBankAccount :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Flow VerificationTypes.BankAccountVerificationResponse
getDriverRegistrationInfoBankAccount merchantShortId opCity driverId requestId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  BankAccountVerification.getInfoBankAccount (personId, merchant.id, merchantOpCityId) requestId

getDriverRegistrationPayoutRegistration :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Maybe Text -> Flow Common.PayoutRegistrationRes
getDriverRegistrationPayoutRegistration merchantShortId opCity driverId mbRequestorId =
  ActorInfo.withDashboardMbPersonIdActorInfo ((Id @DP.Person) <$> mbRequestorId) $ do
    getDriverRegistrationPayoutRegistrationWithActor merchantShortId opCity driverId

getDriverRegistrationPayoutRegistrationWithActor :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow Common.PayoutRegistrationRes
getDriverRegistrationPayoutRegistrationWithActor merchantShortId opCity driverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  res <- ReferralPayout.getPayoutRegistrationWithActor (Just personId, merchant.id, merchantOpCityId)
  pure $
    Common.PayoutRegistrationRes
      { orderId = res.orderId.getId,
        orderResp = res.orderResp
      }

getDriverRegistrationPayoutOrderStatus :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Flow PayoutTypes.PayoutOrderStatusResp
getDriverRegistrationPayoutOrderStatus merchantShortId opCity driverId orderId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  paymentOrder <- QOrder.findById (Id orderId) >>= fromMaybeM (InvalidRequest "UPI registration order not found")
  unless (paymentOrder.personId == cast driverId) $ throwError (InvalidRequest "Order does not belong to this driver")
  (freshStatus, mbCapturedVpa) <-
    if paymentOrder.status == Payment.CHARGED
      then pure (paymentOrder.status, Nothing)
      else do
        let commonPersonId = cast @Common.Driver @DPayment.Person driverId
            commonMerchantOpCityId = cast @DMOC.MerchantOperatingCity @DPayment.MerchantOperatingCity merchantOpCityId
        serviceConfig <-
          CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOpCityId Nothing DPlan.YATRI_SUBSCRIPTION
            >>= fromMaybeM (InternalError "No subscription config found")
        let orderStatusCall = TPayment.orderStatus merchant.id merchantOpCityId serviceConfig.paymentServiceName (Just $ getId driverId)
        paymentStatus <- DPayment.orderStatusService commonMerchantOpCityId commonPersonId (Id orderId) orderStatusCall
        case paymentStatus of
          DPayment.PaymentStatus {status, payerVpa} -> do
            when ((status == Payment.CHARGED || status == Payment.AUTO_REFUNDED) && isJust payerVpa) $ do
              let personId = cast @Common.Driver @DP.Person driverId
              whenJust payerVpa $ \vpa ->
                QFOIE.updatePayoutVpaAndStatus (Just vpa) (Just DFOI.VIA_WEBHOOK) personId
              QOrder.updateVpa (Id orderId) payerVpa
            pure (status, payerVpa)
          _ -> do
            s <- DPayment.getTransactionStatus paymentStatus
            pure (s, Nothing)
  let registrationStatus =
        if freshStatus == Payment.AUTO_REFUNDED && isJust mbCapturedVpa
          then PayoutTypes.SUCCESS
          else mapTxnStatusToPayoutStatus freshStatus
  pure $
    PayoutTypes.CreatePayoutOrderResp
      { orderId = orderId,
        status = registrationStatus,
        amount = paymentOrder.amount,
        orderType = Nothing,
        udf1 = Nothing,
        udf2 = Nothing,
        udf3 = Nothing,
        udf4 = Nothing,
        udf5 = Nothing,
        refunds = Nothing,
        payments = Nothing,
        fulfillments = Nothing,
        customerId = Nothing,
        idAssignedByServiceProvider = Nothing,
        transferStatus = Nothing,
        transferId = Nothing,
        merchantTopUpAmount = Nothing
      }

mapTxnStatusToPayoutStatus :: Payment.TransactionStatus -> PayoutTypes.PayoutOrderStatus
mapTxnStatusToPayoutStatus = \case
  Payment.CHARGED -> PayoutTypes.SUCCESS
  Payment.AUTHENTICATION_FAILED -> PayoutTypes.FAILURE
  Payment.AUTHORIZATION_FAILED -> PayoutTypes.FAILURE
  Payment.JUSPAY_DECLINED -> PayoutTypes.FAILURE
  Payment.CANCELLED -> PayoutTypes.CANCELLED
  Payment.AUTO_REFUNDED -> PayoutTypes.REVERSED
  Payment.CLIENT_AUTH_TOKEN_EXPIRED -> PayoutTypes.INVALID
  _ -> PayoutTypes.INITIATED

postDriverRegistrationDeleteBankAccount :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
postDriverRegistrationDeleteBankAccount merchantShortId opCity driverId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let personId = cast @Common.Driver @DP.Person driverId
  _ <- BankAccountVerification.deleteBankAccount (personId, merchant.id, merchantOpCityId)
  pure Success
