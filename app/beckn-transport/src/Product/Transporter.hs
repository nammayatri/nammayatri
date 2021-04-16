{-# LANGUAGE OverloadedLabels #-}

module Product.Transporter where

import App.Types
import Beckn.TypeClass.Transform
import Beckn.Types.Common
import Beckn.Types.Id (Id (..))
import qualified Beckn.Types.Storage.Organization as SO
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Types.Storage.Vehicle as SVehicle
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Storage.Queries.FarePolicy as QFarePolicy
import qualified Storage.Queries.Organization as QO
import qualified Storage.Queries.Person as QP
import Types.API.Transporter
import qualified Types.Domain.FarePolicy as DFarePolicy
import Types.Error
import qualified Types.Storage.FarePolicy as SFarePolicy

createTransporter :: SR.RegistrationToken -> TransporterReq -> FlowHandler TransporterRes
createTransporter SR.RegistrationToken {..} req = withFlowHandler $ do
  person <- QP.findPersonById (Id _EntityId)
  validate person
  organization <- createTransform req
  validateReq
  sedanFarePolicy <- mkFarePolicy (organization ^. #_id) SVehicle.SEDAN (organization ^. #_createdAt)
  suvFarePolicy <- mkFarePolicy (organization ^. #_id) SVehicle.SUV (organization ^. #_createdAt)
  hatchbackFarePolicy <- mkFarePolicy (organization ^. #_id) SVehicle.HATCHBACK (organization ^. #_createdAt)
  QO.create organization
  traverse_ QFarePolicy.create [sedanFarePolicy, suvFarePolicy, hatchbackFarePolicy]
  QP.updateOrganizationIdAndMakeAdmin (Id _EntityId) (getId $ SO._id organization)
  updatedPerson <- QP.findPersonById (Id _EntityId)
  return $ TransporterRes updatedPerson organization
  where
    validate person = do
      unless (SP._verified person) $
        throwError AccessDenied
      when (SP._role person /= SP.ADMIN) $
        throwError Unauthorized
      when (isJust $ SP._organizationId person) $
        throwError PersonOrgExists
    validateReq = do
      let countryCode = req ^. #_mobileCountryCode
          mobileNumber = req ^. #_mobileNumber
      whenJustM (QO.findOrgByMobileNumber countryCode mobileNumber) $
        \_ -> throwError OrgMobilePhoneUsed
    mkFarePolicy orgId vehicleVariant now = do
      farePolicyId <- L.generateGUID
      pure $
        SFarePolicy.FarePolicy
          { _id = Id farePolicyId,
            _vehicleVariant = vehicleVariant, -- TODO: variants should be looked up from DB
            _organizationId = orgId,
            _baseFare = Just DFarePolicy.defaultBaseFare,
            _baseDistance = Just DFarePolicy.defaultBaseDistance,
            _perExtraKmRate = DFarePolicy.defaultPerExtraKmRate,
            _nightShiftStart = Nothing,
            _nightShiftEnd = Nothing,
            _nightShiftRate = Nothing,
            _createdAt = now,
            _updatedAt = now
          }

updateTransporter :: SR.RegistrationToken -> Id SO.Organization -> UpdateTransporterReq -> FlowHandler TransporterRec
updateTransporter SR.RegistrationToken {..} orgId req = withFlowHandler $ do
  maybePerson <- QP.findPersonByIdAndRoleAndOrgId (Id _EntityId) SP.ADMIN orgId
  now <- getCurrentTime
  case maybePerson of
    Just person -> do
      validate person
      org <- QO.findOrganizationById orgId
      organization <-
        if req ^. #enabled /= Just False
          then modifyTransform req org >>= addTime (Just now)
          else modifyTransform req org
      QO.updateOrganizationRec organization
      return $ TransporterRec organization
    Nothing -> throwError PersonDoesNotExist
  where
    validate person =
      unless (SP._verified person) $ throwError AccessDenied
    addTime fromTime org =
      return $ org {SO._fromTime = fromTime}

getTransporter :: SR.RegistrationToken -> FlowHandler TransporterRec
getTransporter SR.RegistrationToken {..} = withFlowHandler $ do
  person <- QP.findPersonById (Id _EntityId)
  validate person
  case person ^. #_organizationId of
    Just orgId -> TransporterRec <$> QO.findOrganizationById (Id orgId)
    Nothing -> throwError PersonOrgIdNotPresent
  where
    validate person =
      unless (SP._verified person) $ throwError AccessDenied
