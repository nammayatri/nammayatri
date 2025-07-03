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
    verifyDLAsync,
    verifyRC,
    validateImage,
    extractRCImage,
    extractDLImage,
    extractPanImage,
    extractGSTImage,
    extractAadhaarImage,
    validateFaceImage,
    verifySdkResp,
    getTask,
    nameCompare,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Domain.Types.MerchantServiceUsageConfig
import Kernel.External.Types (ServiceFlow)
import Kernel.External.Verification as Reexport hiding
  ( extractAadhaarImage,
    extractDLImage,
    extractGSTImage,
    extractPanImage,
    extractRCImage,
    getTask,
    nameCompare,
    searchAgent,
    validateFaceImage,
    validateImage,
    verifyDLAsync,
    verifyRC,
    verifySdkResp,
  )
import qualified Kernel.External.Verification as Verification
import Kernel.External.Verification.Interface.InternalScripts
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.GovtDataRC ()
import qualified Storage.Cac.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import Tools.Error
import Tools.Metrics (CoreMetrics)

verifyDLAsync ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  VerifyDLAsyncReq ->
  m VerifyDLAsyncResp
verifyDLAsync _ merchantOpCityId req = do
  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  fromMaybeM (InternalError $ "Providers not configured in the priority list !!!!!" <> show merchantServiceUsageConfig.verificationProvidersPriorityList) (listToMaybe merchantServiceUsageConfig.verificationProvidersPriorityList) >>= \provider -> callService merchantOpCityId provider Verification.verifyDLAsync req -- TODO: Using first element of priority list as of now would be soon replacing this with a proper fallback implementation.

verifyRC ::
  ( ServiceFlow m r,
    CoreMetrics m
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe [VerificationService] ->
  VerifyRCReq ->
  m RCRespWithRemPriorityList
verifyRC _ merchantOptCityId mbRemPriorityList req = getConfig >>= flip (Verification.verifyRC (getServiceConfig merchantOptCityId)) req . flip fromMaybe mbRemPriorityList
  where
    getConfig = CQMSUC.findByMerchantOpCityId merchantOptCityId Nothing >>= ((.verificationProvidersPriorityList) <$>) . fromMaybeM (MerchantServiceUsageConfigNotFound merchantOptCityId.getId)

getServiceConfig :: ServiceFlow m r => Id DMOC.MerchantOperatingCity -> VerificationService -> m VerificationServiceConfig
getServiceConfig merchantOptCityId cfg = case cfg of
  GovtData -> return $ GovtDataConfig {}
  _ -> do
    merchantServiceConfig <- CQMSC.findByServiceAndCity (DMSC.VerificationService cfg) merchantOptCityId >>= fromMaybeM (MerchantServiceConfigNotFound merchantOptCityId.getId "verification" $ show cfg)
    case merchantServiceConfig.serviceConfig of
      DMSC.VerificationServiceConfig vsc -> return vsc
      _ -> throwError $ InternalError "Unknown Service Config"

validateImage ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  ValidateImageReq ->
  m ValidateImageResp
validateImage = runWithServiceConfig Verification.validateImage (.verificationService)

validateFaceImage ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  FaceValidationReq ->
  m FaceValidationRes
validateFaceImage = runWithServiceConfig Verification.validateFaceImage (.faceVerificationService)

extractRCImage ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  ExtractRCImageReq ->
  m ExtractRCImageResp
extractRCImage = runWithServiceConfig Verification.extractRCImage (.verificationService)

extractPanImage ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  ExtractPanImage ->
  m ExtractedPanImageResp
extractPanImage = runWithServiceConfig Verification.extractPanImage (.verificationService)

extractGSTImage ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  ExtractGSTImage ->
  m ExtractedGSTImageResp
extractGSTImage = runWithServiceConfig Verification.extractGSTImage (.verificationService)

extractAadhaarImage ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  ExtractAadhaarImageReq ->
  m ExtractAadhaarImageRes
extractAadhaarImage = runWithServiceConfig Verification.extractAadhaarImage (.verificationService)

nameCompare ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  NameCompareReq ->
  m NameCompareResp
nameCompare = runWithServiceConfig Verification.nameCompare (.verificationService)

extractDLImage ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  ExtractDLImageReq ->
  m ExtractDLImageResp
extractDLImage = runWithServiceConfig Verification.extractDLImage (.verificationService)

verifySdkResp ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  VerifySdkDataReq ->
  m VerifySdkDataResp
verifySdkResp = runWithServiceConfig Verification.verifySdkResp (.sdkVerificationService)

getTask ::
  ServiceFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  VerificationService ->
  GetTaskReq ->
  (Text -> Maybe Text -> Text -> m ()) ->
  m GetTaskResp
getTask merchantOpCityId config req updateResp = do
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity (DMSC.VerificationService config) merchantOpCityId
      >>= fromMaybeM (InternalError $ "No verification service provider configured for the merchant, merchantOpCityId:" <> merchantOpCityId.getId <> " Service : " <> T.pack (show config))
  case merchantServiceConfig.serviceConfig of
    DMSC.VerificationServiceConfig vsc -> Verification.getTask vsc req updateResp
    _ -> throwError $ InternalError "Unknown Service Config"

runWithServiceConfig ::
  ServiceFlow m r =>
  (VerificationServiceConfig -> req -> m resp) ->
  (MerchantServiceUsageConfig -> VerificationService) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  req ->
  m resp
runWithServiceConfig func getCfg _merchantId merchantOpCityId req = do
  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  callService merchantOpCityId (getCfg merchantServiceUsageConfig) func req

callService :: ServiceFlow m r => Id DMOC.MerchantOperatingCity -> VerificationService -> (VerificationServiceConfig -> req -> m resp) -> req -> m resp
callService merchantOpCityId vsc func req = do
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity (DMSC.VerificationService vsc) merchantOpCityId
      >>= fromMaybeM (InternalError $ "No verification service provider configured for the merchant, merchantOpCityId:" <> merchantOpCityId.getId <> " Service : " <> T.pack (show vsc))
  case merchantServiceConfig.serviceConfig of
    DMSC.VerificationServiceConfig vsc' -> func vsc' req
    _ -> throwError $ InternalError "Unknown Service Config"
