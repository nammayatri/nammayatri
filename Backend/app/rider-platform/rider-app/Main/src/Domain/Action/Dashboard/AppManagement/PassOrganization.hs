{-# LANGUAGE TransformListComp #-}
{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.AppManagement.PassOrganization
  ( getPassOrganizationGetPassOrganization,
    getPassOrganizationPassDetails,
    postPassOrganizationPassDetailsVerify,
    postPassOrganizationUpdate,
    getPassOrganizationGetOrganizations,
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
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common (fromMaybeM, generateGUID, getCurrentTime)
import qualified Kernel.Utils.Common as Utils
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantOperatingCity as QMerchantOperatingCity
import qualified Storage.Queries.PassDetails as QPassDetails
import qualified Storage.Queries.PassOrganization as QPassOrganization
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
      passDetailsInfo = map mkPassDetailsInfoResp passDetails
  pure $
    PassOrganizationAPI.PassDetailsListResp
      { PassOrganizationAPI.passDetails = passDetailsInfo,
        PassOrganizationAPI.limit = effectiveLimit,
        PassOrganizationAPI.offset = offset'
      }

mkPassDetailsInfoResp :: DPassDetails.PassDetails -> PassOrganizationAPI.PassDetailsInfoResp
mkPassDetailsInfoResp passDetails =
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
      PassOrganizationAPI.studentClass = passDetails.studentClass,
      PassOrganizationAPI.graduationDate = passDetails.graduationDate,
      PassOrganizationAPI.numberOfStages = passDetails.numberOfStages,
      PassOrganizationAPI.remark = passDetails.remark,
      PassOrganizationAPI.createdAt = passDetails.createdAt,
      PassOrganizationAPI.updatedAt = passDetails.updatedAt
    }

postPassOrganizationPassDetailsVerify :: (Id.ShortId DMerchant.Merchant -> Context.City -> PassOrganizationAPI.VerifyPassDetailsReq -> Environment.Flow APISuccess.APISuccess)
postPassOrganizationPassDetailsVerify merchantShortId opCity req = do
  callerMoc <- QMerchantOperatingCity.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (InvalidRequest "Merchant Operating City not found")
  forM_ req.verifications $ \v -> do
    pd <- QPassDetails.findById v.passDetailsId >>= fromMaybeM (PassDetailsNotFound v.passDetailsId.getId)
    unless (pd.merchantOperatingCityId == callerMoc.id) $ Utils.throwError AccessDenied
  now <- Utils.getCurrentTime
  let validTill = Data.Time.addUTCTime (730 * Data.Time.nominalDay) now

  forM_ (GHC.Exts.groupWith (\pdV -> (pdV.verificationStatus, pdV.graduationDate, pdV.remark, pdV.numberOfStages)) req.verifications) $ \pdVs -> do
    let pdV = Kernel.Prelude.head pdVs
    let passDetailsIds = map (\pd -> pd.passDetailsId) pdVs
    QPassDetails.updateVerificationStatus
      pdV.verificationStatus
      validTill
      pdV.remark
      pdV.graduationDate
      pdV.numberOfStages
      passDetailsIds
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
        DPassOrganization.updatedAt = now
      }

getPassOrganizationGetOrganizations :: (Id.ShortId DMerchant.Merchant -> Context.City -> Kernel.Prelude.Text -> Environment.Flow [PassOrganizationAPI.GetOrganizationResp])
getPassOrganizationGetOrganizations merchantShortId opCity passEnumText = do
  passEnum <- DPassDetails.parsePassEnum passEnumText
  merchantOperatingCity <- QMerchantOperatingCity.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (InvalidRequest "Merchant Operating City not found")
  organizations <- QPassOrganization.findByMerchantOperatingCityIdAndPassEnum merchantOperatingCity.id passEnum
  pure $ map mkGetOrganizationResp organizations

mkGetOrganizationResp :: DPassOrganization.PassOrganization -> PassOrganizationAPI.GetOrganizationResp
mkGetOrganizationResp org =
  PassOrganizationAPI.GetOrganizationResp
    { PassOrganizationAPI.id = org.id,
      PassOrganizationAPI.name = org.name,
      PassOrganizationAPI.address = org.address
    }
