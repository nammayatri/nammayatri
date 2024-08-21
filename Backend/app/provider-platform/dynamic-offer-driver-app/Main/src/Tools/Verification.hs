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
    validateFaceImage,
    verifySdkResp,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Domain.Types.MerchantServiceUsageConfig
import Kernel.External.Types (ServiceFlow)
import Kernel.External.Verification as Reexport hiding
  ( extractDLImage,
    extractRCImage,
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

verifyDLAsync ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  VerifyDLAsyncReq ->
  m VerifyDLAsyncResp
verifyDLAsync = runWithServiceConfig Verification.verifyDLAsync (.verificationService)

verifyRC ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  VerifyRCReq ->
  m VerifyRCResp
verifyRC merchantId merchantOptCityId req = do
  config <- CQMSUC.findByMerchantOpCityId merchantOptCityId Nothing >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOptCityId.getId)
  runWithServiceConfig (Verification.verifyRC config.verificationProvidersPriorityList) (.verificationService) merchantId merchantOptCityId req

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
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity (DMSC.VerificationService $ getCfg merchantServiceUsageConfig) merchantOpCityId
      >>= fromMaybeM (InternalError $ "No verification service provider configured for the merchant, merchantOpCityId:" <> merchantOpCityId.getId)
  case merchantServiceConfig.serviceConfig of
    DMSC.VerificationServiceConfig vsc -> func vsc req
    _ -> throwError $ InternalError "Unknown Service Config"
