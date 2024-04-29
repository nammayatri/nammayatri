{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Verification
  ( module Reexport,
    callVerifyRC',
    verifyDLAsync,
    verifyRC,
    validateImage,
    extractRCImage,
    extractDLImage,
    validateFaceImage,
    OnVerifyRCCallBack,
  )
where

import qualified Domain.Types.DocumentVerificationConfig as ODC
import qualified Domain.Types.IdfyVerification as Domain
import qualified Domain.Types.Image as Image
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import Domain.Types.Merchant.MerchantServiceUsageConfig
import Domain.Types.Person (Person)
import qualified Domain.Types.Vehicle as Vehicle
import Kernel.External.Encryption
import Kernel.External.Types (ServiceFlow)
import Kernel.External.Verification as Reexport hiding
  ( extractDLImage,
    extractRCImage,
    searchAgent,
    validateFaceImage,
    validateImage,
    verifyDLAsync,
  )
import qualified Kernel.External.Verification as Verification
import Kernel.External.Verification.Interface.InternalScripts
import qualified Kernel.External.Verification.Types as KVT
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.RCFlow (RCFlow)
import Kernel.Utils.Common
import Storage.Beam.GovtDataRC ()
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.Queries.IdfyVerification as IVQuery
import Tools.Error

type OnVerifyRCCallBack m = Maybe (Person -> Maybe Domain.IdfyVerification -> VT.RCVerificationResponse -> [VerificationServiceConfig] -> Maybe (Id Image.Image) -> Maybe Bool -> Maybe UTCTime -> Maybe Bool -> Text -> m AckResponse)

verifyDLAsync ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  VerifyDLAsyncReq ->
  m VerifyDLAsyncResp
verifyDLAsync = runWithServiceConfig Verification.verifyDLAsync Nothing (.verificationService)

verifyRC ::
  RCFlow m r =>
  Bool ->
  OnVerifyRCCallBack m ->
  Person ->
  Maybe Bool ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe (Id Image.Image) ->
  Maybe Bool ->
  Maybe Vehicle.Category ->
  Maybe UTCTime ->
  VerifyRCReq ->
  m (Maybe VerifyRCResp)
verifyRC isDashboard mbOnVerifyRCCallBack person mbImageExtraction merchantId merchantOptCityId mbImageId multipleRC mbVehicleCategory mbDateOfRegistration req = do
  config <- CQMSUC.findByMerchantOpCityId merchantOptCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOptCityId.getId)
  merchantServiceConfigList <- mapM getServiceConfig config.verificationProvidersPriorityList
  (bool (\_ -> Nothing) Just isDashboard) <$> callVerifyRC' mbOnVerifyRCCallBack merchantServiceConfigList person mbImageExtraction mbVehicleCategory merchantOptCityId mbImageId multipleRC mbDateOfRegistration req
  where
    getServiceConfig msuc =
      if msuc == KVT.GovtData
        then return $ GovtDataConfig {}
        else do
          cfg <- CQMSC.findByMerchantIdAndServiceWithCity merchantId (DMSC.VerificationService msuc) merchantOptCityId >>= fromMaybeM (InternalError $ "No verification service provider configured for the merchant, merchantOpCityId:" <> merchantOptCityId.getId)
          extractCfg . (.serviceConfig) $ cfg
      where
        extractCfg (DMSC.VerificationServiceConfig x) = return x
        extractCfg _ = throwError (InternalError "Unknown Service Config extracted") -- This case should never occure as we are finding svc config using svc name.

callVerifyRC' ::
  RCFlow m r =>
  OnVerifyRCCallBack m ->
  [VerificationServiceConfig] ->
  Person ->
  Maybe Bool ->
  Maybe Vehicle.Category ->
  Id DMOC.MerchantOperatingCity ->
  Maybe (Id Image.Image) ->
  Maybe Bool ->
  Maybe UTCTime ->
  VerifyRCReq ->
  m VerifyRCResp
callVerifyRC' _ [] _ _ _ _ _ _ _ _ = throwError $ InternalError "Provider list is Emoty !!!! Maybe we echausted all the providers."
callVerifyRC' mbOnVerifyRCCallBack (preferredProvider : restProviders) person mbImageExtraction mbVehicleCategory merchantOptCityId mbImageId multipleRC mbDateOfRegistration req = do
  result <- try @_ @SomeException $ runWithServiceConfig (Verification.verifyRC') (Just preferredProvider) (.verificationService) person.merchantId merchantOptCityId req
  case result of
    Left _ -> callVerifyRC' mbOnVerifyRCCallBack restProviders person mbImageExtraction mbVehicleCategory merchantOptCityId mbImageId multipleRC mbDateOfRegistration req
    Right res -> case (mbImageId, mbImageExtraction, mbOnVerifyRCCallBack) of
      (Just imageId, Just imageExtraction, Just onVerifyRCCallBack) -> do
        callOnVerifyRC onVerifyRCCallBack res imageId mbDateOfRegistration imageExtraction
        return res
      _ -> return res
  where
    callOnVerifyRC onVerifyRCCallBack verifyRes imageId dateOfRegistration imageExtraction = do
      now <- getCurrentTime
      encryptedRC <- encrypt req.rcNumber
      let imageExtractionValidation = if isNothing dateOfRegistration && imageExtraction then Domain.Success else Domain.Skipped
      case verifyRes of
        Verification.AsyncResp res -> do
          idfyVerificationEntity <- mkIdfyVerificationEntity res.requestId now imageExtractionValidation encryptedRC
          IVQuery.create idfyVerificationEntity
        Verification.SyncResp res ->
          void $ onVerifyRCCallBack person Nothing res restProviders (Just imageId) multipleRC dateOfRegistration (Just imageExtraction) req.rcNumber
      where
        mkIdfyVerificationEntity requestId now imageExtractionValidation encryptedRC = do
          id <- generateGUID
          return $
            Domain.IdfyVerification
              { id,
                driverId = person.id,
                documentImageId1 = imageId,
                documentImageId2 = Nothing,
                requestId,
                docType = ODC.VehicleRegistrationCertificate,
                documentNumber = encryptedRC,
                driverDateOfBirth = Nothing,
                imageExtractionValidation = imageExtractionValidation,
                issueDateOnDoc = dateOfRegistration,
                status = "pending",
                idfyResponse = Nothing,
                multipleRC,
                vehicleCategory = mbVehicleCategory,
                retryCount = Just 0,
                nameOnCard = Nothing,
                merchantId = Just person.merchantId,
                merchantOperatingCityId = Just merchantOptCityId,
                createdAt = now,
                updatedAt = now
              }

validateImage ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  ValidateImageReq ->
  m ValidateImageResp
validateImage = runWithServiceConfig Verification.validateImage Nothing (.verificationService)

validateFaceImage ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  FaceValidationReq ->
  m FaceValidationRes
validateFaceImage = runWithServiceConfig Verification.validateFaceImage Nothing (.faceVerificationService)

extractRCImage ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  ExtractRCImageReq ->
  m ExtractRCImageResp
extractRCImage = runWithServiceConfig Verification.extractRCImage Nothing (.verificationService)

extractDLImage ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  ExtractDLImageReq ->
  m ExtractDLImageResp
extractDLImage = runWithServiceConfig Verification.extractDLImage Nothing (.verificationService)

runWithServiceConfig ::
  ServiceFlow m r =>
  (VerificationServiceConfig -> req -> m resp) ->
  Maybe VerificationServiceConfig ->
  (MerchantServiceUsageConfig -> VerificationService) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  req ->
  m resp
runWithServiceConfig func mbpreferredProvider getCfg merchantId merchantOpCityId req = do
  case mbpreferredProvider of
    Just preferredProvider -> func preferredProvider req
    Nothing -> do
      merchantServiceUsageConfig <-
        CQMSUC.findByMerchantOpCityId merchantOpCityId
          >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
      merchantServiceConfig <-
        CQMSC.findByMerchantIdAndServiceWithCity merchantId (DMSC.VerificationService $ getCfg merchantServiceUsageConfig) merchantOpCityId
          >>= fromMaybeM (InternalError $ "No verification service provider configured for the merchant, merchantOpCityId:" <> merchantOpCityId.getId)
      case merchantServiceConfig.serviceConfig of
        DMSC.VerificationServiceConfig vsc -> func vsc req
        _ -> throwError $ InternalError "Unknown Service Config"
