module Product.Transporter where

import App.Types
import Beckn.External.Encryption
import Beckn.Types.Common
import Beckn.Types.Id (Id (..))
import Beckn.Utils.Validation (runRequestValidation)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.FarePolicy as QFarePolicy
import qualified Storage.Queries.Organization as QO
import qualified Storage.Queries.Person as QP
import Types.API.Registration (makeUserInfoRes)
import Types.API.Transporter
import qualified Types.Domain.FarePolicy as DFarePolicy
import Types.Error
import qualified Types.Storage.FarePolicy as SFarePolicy
import qualified Types.Storage.Organization as SO
import qualified Types.Storage.Person as SP
import qualified Types.Storage.Vehicle as SVehicle
import Utils.Common

createTransporter :: Id SP.Person -> TransporterReq -> FlowHandler TransporterRes
createTransporter personId req = withFlowHandlerAPI $ do
  runRequestValidation validateTransporterReq req
  person <- QP.findPersonById personId >>= fromMaybeM PersonNotFound
  validate person
  organization <- createOrganization req
  validateReq
  sedanFarePolicy <- mkFarePolicy (organization.id) SVehicle.SEDAN (organization.createdAt)
  suvFarePolicy <- mkFarePolicy (organization.id) SVehicle.SUV (organization.createdAt)
  hatchbackFarePolicy <- mkFarePolicy (organization.id) SVehicle.HATCHBACK (organization.createdAt)
  QO.create organization
  traverse_ QFarePolicy.create [sedanFarePolicy, suvFarePolicy, hatchbackFarePolicy]
  QP.updateOrganizationIdAndMakeAdmin personId (SO.id organization)
  updatedPerson <-
    QP.findPersonById personId
      >>= fromMaybeM PersonNotFound
      >>= decrypt
  return $ TransporterRes (makeUserInfoRes updatedPerson) organization
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

updateTransporter :: Id SP.Person -> Id SO.Organization -> UpdateTransporterReq -> FlowHandler TransporterRec
updateTransporter personId orgId req = withFlowHandlerAPI $ do
  runRequestValidation validateUpdateTransporterReq req
  maybePerson <- QP.findPersonByIdAndRoleAndOrgId personId SP.ADMIN orgId
  now <- getCurrentTime
  case maybePerson of
    Just person -> do
      validate person
      org <-
        QO.findOrganizationById orgId
          >>= fromMaybeM OrgDoesNotExist
      organization <-
        if req.enabled /= Just False
          then modifyOrganization req org >>= addTime (Just now)
          else modifyOrganization req org
      QO.updateOrganizationRec organization
      return $ TransporterRec organization
    Nothing -> throwError PersonDoesNotExist
  where
    validate person =
      unless (person.verified) $ throwError AccessDenied
    addTime fromTime org =
      return $ org {SO.fromTime = fromTime}

getTransporter :: Id SP.Person -> FlowHandler TransporterRec
getTransporter personId = withFlowHandlerAPI $ do
  person <-
    QP.findPersonById personId
      >>= fromMaybeM PersonNotFound
  validate person
  case person.organizationId of
    Just orgId -> TransporterRec <$> (QO.findOrganizationById orgId >>= fromMaybeM OrgNotFound)
    Nothing -> throwError (PersonFieldNotPresent "organization_id")
  where
    validate person =
      unless (person.verified) $ throwError AccessDenied
