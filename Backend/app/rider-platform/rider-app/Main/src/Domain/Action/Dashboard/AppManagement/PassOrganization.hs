{-# LANGUAGE TransformListComp #-}
{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.AppManagement.PassOrganization
  ( getPassOrganizationGetPassOrganization,
    getPassOrganizationPassDetails,
    getPassOrganizationPassDetailsDepot,
    postPassOrganizationPassDetailsVerify,
    postPassOrganizationUpdate,
    getPassOrganizationGetOrganizations,
    getPassOrganizationPassDetailsDocument,
    postPassOrganizationAssignDepot,
  )
where

import qualified API.Types.Dashboard.AppManagement.PassOrganization as PassOrganizationAPI
import qualified Data.List.NonEmpty as NE
import Data.OpenApi (ToSchema)
import qualified Data.Time
import qualified Domain.Action.UI.PassDetails as DPassDetails
import qualified Domain.Types.Merchant as DMerchant
import qualified "this" Domain.Types.PassDetails as DPassDetails
import qualified "this" Domain.Types.PassOrganization as DPassOrganization
import qualified Domain.Types.PassType as DPassType
import qualified "this" Domain.Types.Person as DPerson
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified GHC.Exts
import qualified IssueManagement.Domain.Types.MediaFile as DMF
import qualified IssueManagement.Storage.Queries.MediaFile as QMediaFile
import Kernel.External.Encryption (decrypt)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common (fromMaybeM, generateGUID, getCurrentTime)
import qualified Kernel.Utils.Common as Utils
import Storage.Beam.IssueManagement ()
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantOperatingCity as QMerchantOperatingCity
import qualified Storage.Queries.PassDetails as QPassDetails
import qualified Storage.Queries.PassOrganization as QPassOrganization
import qualified Storage.Queries.Person as QPerson
import Tools.Error

allVerificationStatuses :: [DPassDetails.VerificationStatus]
allVerificationStatuses = [DPassDetails.PENDING, DPassDetails.CLG_VERIFIED, DPassDetails.MTC_VERIFIED, DPassDetails.REJECTED, DPassDetails.EXPIRED]

getPassOrganizationGetPassOrganization :: (Id.ShortId DMerchant.Merchant -> Context.City -> Id.Id DPerson.Person -> Environment.Flow PassOrganizationAPI.GetOrganizationResp)
getPassOrganizationGetPassOrganization merchantShortId opCity personId = do
  callerMoc <- QMerchantOperatingCity.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (InvalidRequest "Merchant Operating City not found")
  passOrganization <- QPassOrganization.findByPersonId personId >>= fromMaybeM (InvalidRequest "Pass organization not found")
  unless (passOrganization.merchantOperatingCityId == callerMoc.id) $ Utils.throwError AccessDenied
  pure $ mkGetOrganizationResp passOrganization

getPassOrganizationPassDetails ::
  ( Id.ShortId DMerchant.Merchant ->
    Context.City ->
    Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe (Id.Id DPassOrganization.PassOrganization) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Environment.Flow PassOrganizationAPI.PassDetailsListResp
  )
getPassOrganizationPassDetails merchantShortId opCity passEnumText mbPassOrganizationId mbStatusText limit offset = do
  passEnum <- DPassDetails.parsePassEnum passEnumText
  mbStatus <- traverse DPassDetails.parseVerificationStatus mbStatusText
  callerMoc <- QMerchantOperatingCity.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (InvalidRequest "Merchant Operating City not found")
  let statuses = maybe allVerificationStatuses (: []) mbStatus
      effectiveLimit = min 100 (fromMaybe 10 limit)
      cappedLimit = Just effectiveLimit
  passDetails <- case mbPassOrganizationId of
    Just passOrganizationId -> do
      passOrganization <- QPassOrganization.findById passOrganizationId >>= fromMaybeM (PassOrganizationNotFound passOrganizationId.getId)
      unless (passOrganization.merchantOperatingCityId == callerMoc.id) $ Utils.throwError AccessDenied
      unless (passOrganization.passEnum == passEnum) $ Utils.throwError (InvalidRequest "Organization's passEnum does not match the path passEnum")
      QPassDetails.findAllByPassOrganizationIdAndVerificationStatus cappedLimit offset passOrganizationId statuses
    Nothing ->
      QPassDetails.findAllByMerchantIdAndMerchantOperatingCityId cappedLimit offset callerMoc.merchantId callerMoc.id passEnum statuses
  let offset' = fromMaybe 0 offset
  passDetailsInfo <- forM passDetails $ \pd -> do
    decGuardianMobile <- mapM decrypt pd.guardianMobileNumber
    pure $ mkPassDetailsInfoResp decGuardianMobile pd
  pure $
    PassOrganizationAPI.PassDetailsListResp
      { PassOrganizationAPI.passDetails = passDetailsInfo,
        PassOrganizationAPI.limit = effectiveLimit,
        PassOrganizationAPI.offset = offset'
      }

mkPassDetailsInfoResp :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> DPassDetails.PassDetails -> PassOrganizationAPI.PassDetailsInfoResp
mkPassDetailsInfoResp decGuardianMobile passDetails =
  PassOrganizationAPI.PassDetailsInfoResp
    { PassOrganizationAPI.passDetailsId = passDetails.id,
      PassOrganizationAPI.personId = passDetails.personId,
      PassOrganizationAPI.name = passDetails.name,
      PassOrganizationAPI.idCardPicture = passDetails.idCardPicture,
      PassOrganizationAPI.selfImage = passDetails.selfImage,
      PassOrganizationAPI.verificationStatus = passDetails.verificationStatus,
      PassOrganizationAPI.validTill = passDetails.validTill,
      PassOrganizationAPI.address = passDetails.address,
      PassOrganizationAPI.gender = passDetails.gender,
      PassOrganizationAPI.pincode = passDetails.pincode,
      PassOrganizationAPI.routePairs = passDetails.routePairs,
      PassOrganizationAPI.age = passDetails.age,
      PassOrganizationAPI.guardianName = passDetails.guardianName,
      PassOrganizationAPI.guardianMobileNumber = decGuardianMobile,
      PassOrganizationAPI.department = passDetails.department,
      PassOrganizationAPI.year = passDetails.year,
      PassOrganizationAPI.academicYearStart = passDetails.academicYearStart,
      PassOrganizationAPI.academicYearEnd = passDetails.academicYearEnd,
      PassOrganizationAPI.numberOfStages = passDetails.numberOfStages,
      PassOrganizationAPI.remark = passDetails.remark,
      PassOrganizationAPI.createdAt = passDetails.createdAt,
      PassOrganizationAPI.updatedAt = passDetails.updatedAt
    }

getPassOrganizationPassDetailsDepot ::
  ( Id.ShortId DMerchant.Merchant ->
    Context.City ->
    Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe (Id.Id DPerson.Person) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Environment.Flow PassOrganizationAPI.PassDetailsListResp
  )
getPassOrganizationPassDetailsDepot merchantShortId opCity passEnumText mbDepotPersonId mbStatusText limit offset = do
  passEnum <- DPassDetails.parsePassEnum passEnumText
  depotPersonId <- mbDepotPersonId & fromMaybeM (InvalidRequest "depotPersonId is required")
  statusText <- mbStatusText & fromMaybeM (InvalidRequest "status is required")
  status <- DPassDetails.parseVerificationStatus statusText
  callerMoc <- QMerchantOperatingCity.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (InvalidRequest "Merchant Operating City not found")
  orgs <- QPassOrganization.findByDepotPersonId (Just depotPersonId)
  let scopedOrgIds = [o.id | o <- orgs, o.merchantOperatingCityId == callerMoc.id, o.passEnum == passEnum]
      effectiveLimit = min 100 (fromMaybe 10 limit)
      offset' = fromMaybe 0 offset
  passDetails <-
    if null scopedOrgIds
      then pure []
      else QPassDetails.findAllByPassOrganizationIdsAndVerificationStatus (Just effectiveLimit) offset scopedOrgIds [status]
  passDetailsInfo <- forM passDetails $ \pd -> do
    decGuardianMobile <- mapM decrypt pd.guardianMobileNumber
    pure $ mkPassDetailsInfoResp decGuardianMobile pd
  pure $
    PassOrganizationAPI.PassDetailsListResp
      { PassOrganizationAPI.passDetails = passDetailsInfo,
        PassOrganizationAPI.limit = effectiveLimit,
        PassOrganizationAPI.offset = offset'
      }

postPassOrganizationPassDetailsVerify :: (Id.ShortId DMerchant.Merchant -> Context.City -> PassOrganizationAPI.VerifyPassDetailsReq -> Environment.Flow APISuccess.APISuccess)
postPassOrganizationPassDetailsVerify merchantShortId opCity req = do
  callerMoc <- QMerchantOperatingCity.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (InvalidRequest "Merchant Operating City not found")
  now <- Utils.getCurrentTime
  forM_ req.verifications $ \pdV -> do
    pd <- QPassDetails.findById pdV.passDetailsId >>= fromMaybeM (PassDetailsNotFound pdV.passDetailsId.getId)
    unless (pd.merchantOperatingCityId == callerMoc.id) $ Utils.throwError AccessDenied
    let mergedRemark = pdV.remark <|> pd.remark
        mergedNumberOfStages = pdV.numberOfStages <|> pd.numberOfStages
        mergedAcademicYearStart = pdV.academicYearStart <|> pd.academicYearStart
        mergedAcademicYearEnd = pdV.academicYearEnd <|> pd.academicYearEnd
    validTill <- DPassDetails.computeValidTill now callerMoc.id
    QPassDetails.updateVerificationStatus
      pdV.verificationStatus
      validTill
      mergedRemark
      mergedNumberOfStages
      mergedAcademicYearStart
      mergedAcademicYearEnd
      [pdV.passDetailsId]
  pure APISuccess.Success

postPassOrganizationUpdate :: (Id.ShortId DMerchant.Merchant -> Context.City -> Id.Id DPerson.Person -> PassOrganizationAPI.PassOrganizationUpdateReq -> Environment.Flow APISuccess.APISuccess)
postPassOrganizationUpdate merchantShortId opCity personId req = do
  callerMoc <- QMerchantOperatingCity.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (InvalidRequest "Merchant Operating City not found")
  mbExisting <- QPassOrganization.findByPersonId personId
  case mbExisting of
    Just existing -> do
      unless (existing.merchantOperatingCityId == callerMoc.id) $ Utils.throwError AccessDenied
      QPassOrganization.updatePassOrganization req.name req.address existing.id
    Nothing -> do
      newPassOrganization <- mkPassOrganization req personId merchantShortId opCity
      QPassOrganization.create newPassOrganization
  pure APISuccess.Success

mkPassOrganization :: PassOrganizationAPI.PassOrganizationUpdateReq -> Id.Id DPerson.Person -> Id.ShortId DMerchant.Merchant -> Context.City -> Environment.Flow DPassOrganization.PassOrganization
mkPassOrganization req personId merchantShortId opCity = do
  now <- Utils.getCurrentTime
  newId <- Utils.generateGUID
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (InvalidRequest "Merchant not found")
  merchantOperatingCity <- QMerchantOperatingCity.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (InvalidRequest "Merchant Operating City not found")
  return $
    DPassOrganization.PassOrganization
      { DPassOrganization.name = req.name,
        DPassOrganization.address = req.address,
        DPassOrganization.merchantOperatingCityId = merchantOperatingCity.id,
        DPassOrganization.merchantId = merchant.id,
        DPassOrganization.personId = personId,
        DPassOrganization.passEnum = req.passEnum,
        DPassOrganization.id = newId,
        DPassOrganization.createdAt = now,
        DPassOrganization.updatedAt = now,
        DPassOrganization.depotPersonId = Nothing,
        DPassOrganization.depotId = Nothing
      }

getPassOrganizationGetOrganizations ::
  ( Id.ShortId DMerchant.Merchant ->
    Context.City ->
    Kernel.Prelude.Text ->
    Kernel.Prelude.Maybe (Id.Id DPerson.Person) ->
    Environment.Flow [PassOrganizationAPI.GetOrganizationResp]
  )
getPassOrganizationGetOrganizations merchantShortId opCity passEnumText mbDepotPersonId = do
  passEnum <- DPassDetails.parsePassEnum passEnumText
  merchantOperatingCity <- QMerchantOperatingCity.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (InvalidRequest "Merchant Operating City not found")
  organizations <- case mbDepotPersonId of
    Just depotPersonId -> do
      orgs <- QPassOrganization.findByDepotPersonId (Just depotPersonId)
      pure $ filter (\o -> o.passEnum == passEnum) orgs
    Nothing ->
      QPassOrganization.findByMerchantOperatingCityIdAndPassEnum merchantOperatingCity.id passEnum
  pure $ map mkGetOrganizationResp $ filter (\o -> o.merchantOperatingCityId == merchantOperatingCity.id) organizations

mkGetOrganizationResp :: DPassOrganization.PassOrganization -> PassOrganizationAPI.GetOrganizationResp
mkGetOrganizationResp org =
  PassOrganizationAPI.GetOrganizationResp
    { PassOrganizationAPI.id = org.id,
      PassOrganizationAPI.name = org.name,
      PassOrganizationAPI.address = org.address,
      PassOrganizationAPI.depotId = org.depotId
    }

getPassOrganizationPassDetailsDocument :: (Id.ShortId DMerchant.Merchant -> Context.City -> Id.Id DMF.MediaFile -> Environment.Flow Kernel.Prelude.Text)
getPassOrganizationPassDetailsDocument merchantShortId opCity documentId = do
  _ <- QMerchantOperatingCity.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (InvalidRequest "Merchant Operating City not found")
  mediaFile <- QMediaFile.findById documentId >>= fromMaybeM (InvalidRequest "Document not found")
  s3FilePath <- mediaFile.s3FilePath & fromMaybeM (InvalidRequest "Document has no associated S3 path")
  DPassDetails.fetchPassDocumentFromS3 s3FilePath

postPassOrganizationAssignDepot ::
  ( Id.ShortId DMerchant.Merchant ->
    Context.City ->
    PassOrganizationAPI.AssignDepotReq ->
    Environment.Flow APISuccess.APISuccess
  )
postPassOrganizationAssignDepot merchantShortId opCity req = do
  callerMoc <- QMerchantOperatingCity.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (InvalidRequest "Merchant Operating City not found")
  when (null req.passOrganizations) $ Utils.throwError (InvalidRequest "passOrganizations must not be empty")
  forM_ req.passOrganizations $ \pa -> do
    org <- QPassOrganization.findById pa.id >>= fromMaybeM (PassOrganizationNotFound pa.id.getId)
    unless (org.merchantOperatingCityId == callerMoc.id) $ Utils.throwError AccessDenied
    if pa.isAdd
      then
        unless (isNothing org.depotPersonId || org.depotPersonId == Just req.depotPersonId) $
          Utils.throwError (InvalidRequest "Already assigned to another depot person")
      else
        unless (org.depotPersonId == Just req.depotPersonId) $
          Utils.throwError (InvalidRequest "You don't own this assignment")
  let addIds = [pa.id | pa <- req.passOrganizations, pa.isAdd]
      removeIds = [pa.id | pa <- req.passOrganizations, not pa.isAdd]
  unless (null addIds) $
    QPassOrganization.updateDepotAssignment (Just req.depotPersonId) req.depotId addIds
  unless (null removeIds) $
    QPassOrganization.updateDepotAssignment Nothing Nothing removeIds
  pure APISuccess.Success
