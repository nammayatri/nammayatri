{-# LANGUAGE OverloadedLabels #-}

module Product.Transporter where

import App.Types
import Beckn.TypeClass.Transform
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
import qualified Types.Storage.FarePolicy as SFarePolicy

createTransporter :: SR.RegistrationToken -> TransporterReq -> FlowHandler TransporterRes
createTransporter SR.RegistrationToken {..} req = withFlowHandler $ do
  person <- QP.findPersonById (Id _EntityId)
  validate person
  organization <- createTransform req
  validateReq req
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
        throwError400 "USER_NOT_VERIFIED"
      when (isJust $ SP._organizationId person) $
        throwErrorWithInfo400 "ORG_ALREADY_EXISTS" "User already registered an organization"
      when (SP._role person /= SP.ADMIN) $
        throwError401 "UNAUTHORIZED"
    validateReq treq =
      unless (all (== True) (isJust <$> transporterMandatoryFields treq)) $
        throwError400 "MISSING_MANDATORY_FIELDS"
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

updateTransporter :: SR.RegistrationToken -> Text -> UpdateTransporterReq -> FlowHandler TransporterRec
updateTransporter SR.RegistrationToken {..} orgId req = withFlowHandler $ do
  maybePerson <- QP.findPersonByIdAndRoleAndOrgId (Id _EntityId) SP.ADMIN orgId
  now <- getCurrTime
  case maybePerson of
    Just person -> do
      validate person
      org <- QO.findOrganizationById $ Id orgId
      organization <-
        if req ^. #enabled /= Just False
          then modifyTransform req org >>= addTime (Just now)
          else modifyTransform req org
      QO.updateOrganizationRec organization
      return $ TransporterRec organization
    Nothing -> throwError400 "USER_NOT_ELIGIBLE"
  where
    validate person =
      unless (SP._verified person) $ throwError400 "USER_NOT_VERIFIED"
    addTime fromTime org =
      return $ org {SO._fromTime = fromTime}

getTransporter :: SR.RegistrationToken -> FlowHandler TransporterRec
getTransporter SR.RegistrationToken {..} = withFlowHandler $ do
  person <- QP.findPersonById (Id _EntityId)
  validate person
  case person ^. #_organizationId of
    Just orgId -> TransporterRec <$> QO.findOrganizationById (Id orgId)
    Nothing -> throwErrorWithInfo400 "ORG_NOT_EXISTS" "User not registered an organization"
  where
    validate person =
      unless (SP._verified person) $ throwError400 "USER_NOT_VERIFIED"

transporterMandatoryFields :: TransporterReq -> [Maybe Text]
transporterMandatoryFields req =
  [ req ^. #_mobileNumber,
    req ^. #_mobileCountryCode,
    req ^. #_district,
    req ^. #_city,
    req ^. #_country
  ]
