{-# OPTIONS_GHC -Wwarn=unused-imports #-}
{-# LANGUAGE TransformListComp #-}

module Domain.Action.Dashboard.AppManagement.PassOrganization
  ( getPassOrganizationGetPassOrganizationId,
    getPassOrganizationPassDetails,
    postPassOrganizationPassDetailsVerify,
    postPassOrganizationUpdate,
    getPassOrganizationGetOrganizations
  )
where

import qualified API.Types.Dashboard.AppManagement.PassOrganization as PassOrganizationAPI
import Data.OpenApi (ToSchema)
import qualified Data.List.NonEmpty as NE
import qualified GHC.Exts
import qualified Data.Time
import qualified Domain.Types.Merchant as DMerchant
import qualified "this" Domain.Types.PassDetails as DPassDetails
import qualified "this" Domain.Types.PassOrganization as DPassOrganization
import qualified "this" Domain.Types.Person as DPerson
import qualified Domain.Types.PassType as DPassType
import qualified Environment
import EulerHS.Prelude hiding (id)
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

getPassOrganizationGetPassOrganizationId :: (Id.ShortId DMerchant.Merchant -> Context.City -> Id.Id DPerson.Person -> Environment.Flow (Id.Id DPassOrganization.PassOrganization))
getPassOrganizationGetPassOrganizationId _merchantShortId _opCity personId = do
  mbPassOrganization <- QPassOrganization.findByPersonId personId
  case mbPassOrganization of
    Nothing -> Utils.throwError $ InvalidRequest "Pass organization not found"
    Just passOrganization -> pure passOrganization.id

getPassOrganizationPassDetails :: (Id.ShortId DMerchant.Merchant -> Context.City -> Id.Id DPassOrganization.PassOrganization -> Kernel.Prelude.Maybe (DPassDetails.VerificationStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.Flow PassOrganizationAPI.PassDetailsListResp)
getPassOrganizationPassDetails _merchantShortId _opCity passOrganizationId status limit offset = do
  _ <- QPassOrganization.findById passOrganizationId >>= fromMaybeM (PassOrganizationNotFound passOrganizationId.getId)
  passDetails <-
    case status of
      Nothing -> QPassDetails.findAllByPassOrganizationId limit offset passOrganizationId
      Just s -> QPassDetails.findAllByPassOrganizationIdAndVerificationStatus limit offset passOrganizationId s
  let totalCount = length passDetails
  let offset' = fromMaybe 0 offset
  let passDetailsInfo = map mkPassDetailsInfoResp passDetails
  pure $
    PassOrganizationAPI.PassDetailsListResp
      { PassOrganizationAPI.passDetails = passDetailsInfo,
        PassOrganizationAPI.totalCount = totalCount,
        PassOrganizationAPI.offset = offset'
      }

mkPassDetailsInfoResp :: DPassDetails.PassDetails -> PassOrganizationAPI.PassDetailsInfoResp
mkPassDetailsInfoResp passDetails =
  PassOrganizationAPI.PassDetailsInfoResp
    { PassOrganizationAPI.passDetailsId = passDetails.id,
      PassOrganizationAPI.personId = passDetails.personId,
      PassOrganizationAPI.name = passDetails.name,
      PassOrganizationAPI.idCardPicture = passDetails.idCardPicture,
      PassOrganizationAPI.verificationStatus = passDetails.verificationStatus,
      PassOrganizationAPI.verificationDate = passDetails.verificationDate,
      PassOrganizationAPI.address = passDetails.address,
      PassOrganizationAPI.age = passDetails.age,
      PassOrganizationAPI.guardianName = passDetails.guardianName,
      PassOrganizationAPI.studentClass = passDetails.studentClass,
      PassOrganizationAPI.graduationDate = passDetails.graduationDate,
      PassOrganizationAPI.numberOfStages = passDetails.numberOfStages,
      PassOrganizationAPI.createdAt = passDetails.createdAt,
      PassOrganizationAPI.updatedAt = passDetails.updatedAt
    }

postPassOrganizationPassDetailsVerify :: (Id.ShortId DMerchant.Merchant -> Context.City -> PassOrganizationAPI.VerifyPassDetailsReq -> Environment.Flow APISuccess.APISuccess)
postPassOrganizationPassDetailsVerify _merchantShortId _opCity req = do
  now <- Utils.getCurrentTime
  let verificationDate = Data.Time.addUTCTime (730 * Data.Time.nominalDay) now

  forM_ (GHC.Exts.groupWith (\pdV -> (pdV.verificationStatus, pdV.graduationDate, pdV.remark)) req.verifications) $ \pdVs -> do
    let pdV = Kernel.Prelude.head pdVs
    let passDetailsIds = map (\pd -> pd.passDetailsId) pdVs
    QPassDetails.updateVerificationStatus
      pdV.verificationStatus
      (Just verificationDate)
      pdV.remark
      pdV.graduationDate
      passDetailsIds
  pure APISuccess.Success

postPassOrganizationUpdate :: (Id.ShortId DMerchant.Merchant -> Context.City -> Id.Id DPerson.Person -> PassOrganizationAPI.PassOrganizationUpdateReq -> Environment.Flow APISuccess.APISuccess)
postPassOrganizationUpdate merchantShortId opCity personId req = do
  mbPassOrganization <- QPassOrganization.findByPersonId personId
  case mbPassOrganization of
    Nothing -> do
      newPassOrganization <- mkPassOrganization req personId merchantShortId opCity
      QPassOrganization.create newPassOrganization
      pure APISuccess.Success
    Just passOrganization -> do
      QPassOrganization.updatePassOrganization req.name req.address req.passEnum passOrganization.id
      pure APISuccess.Success

mkPassOrganization :: PassOrganizationAPI.PassOrganizationUpdateReq -> Id.Id DPerson.Person -> Id.ShortId DMerchant.Merchant -> Context.City -> Environment.Flow DPassOrganization.PassOrganization
mkPassOrganization req personId merchantShortId opCity = do
  now <- Utils.getCurrentTime
  newId <- Utils.generateGUID
  mbMerchant <- QMerchant.findByShortId merchantShortId
  mbMerchantOperatingCity <- QMerchantOperatingCity.findByMerchantShortIdAndCity merchantShortId opCity

  merchant <- case mbMerchant of
    Just m -> pure m
    Nothing -> Utils.throwError $ InvalidRequest "Merchant not found"

  merchantOperatingCity <- case mbMerchantOperatingCity of
    Just moc -> pure moc
    Nothing -> Utils.throwError $ InvalidRequest "Merchant Operating City not found"

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

getPassOrganizationGetOrganizations :: (Id.ShortId DMerchant.Merchant -> Context.City -> DPassType.PassEnum -> Environment.Flow [PassOrganizationAPI.GetOrganizationResp])
getPassOrganizationGetOrganizations merchantShortId opCity passEnum = do
  mbMerchantOperatingCity <- QMerchantOperatingCity.findByMerchantShortIdAndCity merchantShortId opCity
  merchantOperatingCity <- case mbMerchantOperatingCity of
    Just moc -> pure moc
    Nothing -> Utils.throwError $ InvalidRequest "Merchant Operating City not found"

  organizations <- QPassOrganization.findByMerchantOperatingCityIdAndPassEnum merchantOperatingCity.id passEnum
  pure $ map mkGetOrganizationResp organizations

mkGetOrganizationResp :: DPassOrganization.PassOrganization -> PassOrganizationAPI.GetOrganizationResp
mkGetOrganizationResp org =
  PassOrganizationAPI.GetOrganizationResp
    { PassOrganizationAPI.id = org.id,
      PassOrganizationAPI.name = org.name,
      PassOrganizationAPI.address = org.address
    }