module Product.Transporter where

import App.Types
import Beckn.TypeClass.Transform
import Beckn.Types.Common
import Beckn.Types.Id (Id (..))
import qualified Beckn.Types.Storage.Organization as SO
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Types.Storage.Vehicle as SVehicle
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.FarePolicy as QFarePolicy
import qualified Storage.Queries.Organization as QO
import qualified Storage.Queries.Person as QP
import Types.API.Transporter
import qualified Types.Domain.FarePolicy as DFarePolicy
import Types.Error
import qualified Types.Storage.FarePolicy as SFarePolicy
import Utils.Common

createTransporter :: SR.RegistrationToken -> TransporterReq -> FlowHandler TransporterRes
createTransporter SR.RegistrationToken {..} req = withFlowHandlerAPI $ do
  person <- QP.findPersonById (Id entityId)
  validate person
  organization <- createTransform req
  validateReq
  sedanFarePolicy <- mkFarePolicy (organization.id) SVehicle.SEDAN (organization.createdAt)
  suvFarePolicy <- mkFarePolicy (organization.id) SVehicle.SUV (organization.createdAt)
  hatchbackFarePolicy <- mkFarePolicy (organization.id) SVehicle.HATCHBACK (organization.createdAt)
  QO.create organization
  traverse_ QFarePolicy.create [sedanFarePolicy, suvFarePolicy, hatchbackFarePolicy]
  QP.updateOrganizationIdAndMakeAdmin (Id entityId) (SO.id organization)
  updatedPerson <- QP.findPersonById (Id entityId) >>= SP.buildDecryptedPerson
  return $ TransporterRes updatedPerson organization
  where
    validate person = do
      unless (person.verified) $
        throwError AccessDenied
      when (person.role /= SP.ADMIN) $
        throwError Unauthorized
      when (isJust person.organizationId) $
        throwError PersonOrgExists
    validateReq = do
      let countryCode = req.mobileCountryCode
          mobileNumber = req.mobileNumber
      whenJustM (QO.findOrgByMobileNumber countryCode mobileNumber) $
        \_ -> throwError OrgMobilePhoneUsed
    mkFarePolicy orgId vehicleVariant now = do
      farePolicyId <- L.generateGUID
      pure $
        SFarePolicy.FarePolicy
          { id = Id farePolicyId,
            vehicleVariant = vehicleVariant, -- TODO: variants should be looked up from DB
            organizationId = orgId,
            baseFare = Just DFarePolicy.defaultBaseFare,
            baseDistance = Just DFarePolicy.defaultBaseDistance,
            perExtraKmRate = DFarePolicy.defaultPerExtraKmRate,
            nightShiftStart = Nothing,
            nightShiftEnd = Nothing,
            nightShiftRate = Nothing,
            createdAt = now,
            updatedAt = now
          }

updateTransporter :: SR.RegistrationToken -> Id SO.Organization -> UpdateTransporterReq -> FlowHandler TransporterRec
updateTransporter SR.RegistrationToken {..} orgId req = withFlowHandlerAPI $ do
  maybePerson <- QP.findPersonByIdAndRoleAndOrgId (Id entityId) SP.ADMIN orgId
  now <- getCurrentTime
  case maybePerson of
    Just person -> do
      validate person
      org <- QO.findOrganizationById orgId
      organization <-
        if req.enabled /= Just False
          then modifyTransform req org >>= addTime (Just now)
          else modifyTransform req org
      QO.updateOrganizationRec organization
      return $ TransporterRec organization
    Nothing -> throwError PersonDoesNotExist
  where
    validate person =
      unless (person.verified) $ throwError AccessDenied
    addTime fromTime org =
      return $ org {SO.fromTime = fromTime}

getTransporter :: SR.RegistrationToken -> FlowHandler TransporterRec
getTransporter SR.RegistrationToken {..} = withFlowHandlerAPI $ do
  person <- QP.findPersonById (Id entityId)
  validate person
  case person.organizationId of
    Just orgId -> TransporterRec <$> QO.findOrganizationById orgId
    Nothing -> throwError (PersonFieldNotPresent "organization_id")
  where
    validate person =
      unless (person.verified) $ throwError AccessDenied
