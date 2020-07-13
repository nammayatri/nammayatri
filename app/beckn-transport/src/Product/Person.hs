{-# LANGUAGE OverloadedLabels #-}

module Product.Person where

import App.Types
import qualified Beckn.External.MyValueFirst.Flow as SF
import qualified Beckn.External.MyValueFirst.Types as SMS
import Beckn.TypeClass.Transform
import Beckn.Types.App
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import Data.Aeson (encode)
import Data.Generics.Labels
import Data.Maybe
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Organization as OQ
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import System.Environment
import Types.API.Person

updatePerson :: SR.RegistrationToken -> Text -> UpdatePersonReq -> FlowHandler UpdatePersonRes
updatePerson SR.RegistrationToken {..} personId req = withFlowHandler $ do
  verifyPerson personId _EntityId
  person <- QP.findPersonById (PersonId _EntityId)
  updatedPerson <- transformFlow2 req person
  QP.updatePersonRec (PersonId _EntityId) updatedPerson
  return $ UpdatePersonRes updatedPerson
  where
    verifyPerson personId entityId =
      when (personId /= entityId)
        $ L.throwException
        $ err400 {errBody = "PERSON_ID_MISMATCH"}

createPerson :: Text -> CreatePersonReq -> FlowHandler UpdatePersonRes
createPerson orgId req = withFlowHandler $ do
  validateDriver req
  person <- addOrgId orgId <$> transformFlow req
  QP.create person
  org <- OQ.findOrganizationById (OrganizationId orgId)
  let mobileNumber = fromJust $ person ^. #_mobileNumber
      countryCode = fromJust $ person ^. #_mobileCountryCode
  sendInviteSms (countryCode <> mobileNumber) (org ^. #_name)
  return $ UpdatePersonRes person
  where
    validateDriver :: CreatePersonReq -> Flow ()
    validateDriver req =
      when (req ^. #_role == Just SP.DRIVER) $
        case (req ^. #_mobileNumber, req ^. #_mobileCountryCode) of
          (Just mobileNumber, Just countryCode) ->
            whenM (isJust <$> QP.findByMobileNumber countryCode mobileNumber)
              $ L.throwException
              $ err400 {errBody = "DRIVER_ALREADY_CREATED"}
          _ -> L.throwException $ err400 {errBody = "MOBILE_NUMBER_AND_COUNTRY_CODE_MANDATORY"}

listPerson :: Text -> [SP.Role] -> Maybe Integer -> Maybe Integer -> FlowHandler ListPersonRes
listPerson orgId roles limitM offsetM =
  withFlowHandler $
    ListPersonRes <$> QP.findAllWithLimitOffsetByOrgIds limitM offsetM roles [orgId]

getPerson ::
  SR.RegistrationToken ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe SP.IdentifierType ->
  FlowHandler PersonRes
getPerson SR.RegistrationToken {..} idM mobileM countryCodeM emailM identifierM identifierTypeM =
  withFlowHandler $ do
    user <- QP.findPersonById (PersonId _EntityId)
    -- TODO: fix this to match based on identifierType
    -- And maybe have a way to handle the case when ID is
    -- passed and identifierType is null. Throw validation errors
    person <- case identifierTypeM of
      Nothing -> QP.findPersonById (PersonId $ fromJust idM)
      Just SP.MOBILENUMBER -> do
        countryCode <- fromMaybeM400 "MOBILE_COUNTRY_CODE_REQUIRED" countryCodeM
        mobile <- fromMaybeM400 "MOBILE_NUMBER_REQUIRED" mobileM
        QP.findByMobileNumber countryCode mobile
          >>= fromMaybeM400 "PERSON_NOT_FOUND"
      Just SP.EMAIL ->
        fromMaybeM400 "EMAIL_REQUIRED" emailM
          >>= QP.findByEmail
          >>= fromMaybeM400 "PERSON_NOT_FOUND"
      Just SP.AADHAAR ->
        fromMaybeM400 "IDENTIFIER_REQUIRED" identifierM
          >>= QP.findByIdentifier
          >>= fromMaybeM400 "PERSON_NOT_FOUND"
    hasAccess user person
    return $ PersonRes person
  where
    hasAccess :: SP.Person -> SP.Person -> Flow ()
    hasAccess user person =
      when
        ( (user ^. #_role) /= SP.ADMIN && (user ^. #_id) /= (person ^. #_id)
            || (user ^. #_organizationId) /= (person ^. #_organizationId)
        )
        $ L.throwException
        $ err401 {errBody = "Unauthorized"}

deletePerson :: Text -> Text -> FlowHandler DeletePersonRes
deletePerson orgId personId = withFlowHandler $ do
  person <- QP.findPersonById (PersonId personId)
  if person ^. #_organizationId == Just orgId
    then do
      QP.deleteById (PersonId personId)
      QR.deleteByEntitiyId personId
      return $ DeletePersonRes personId
    else L.throwException $ err401 {errBody = "Unauthorized"}

-- Utility Functions --

addOrgId :: Text -> SP.Person -> SP.Person
addOrgId orgId person = person {SP._organizationId = Just orgId}

sendInviteSms :: Text -> Text -> Flow ()
sendInviteSms phoneNumber orgName = do
  username <- L.runIO $ getEnv "SMS_GATEWAY_USERNAME"
  password <- L.runIO $ getEnv "SMS_GATEWAY_PASSWORD"
  res <-
    SF.submitSms
      SF.defaultBaseUrl
      SMS.SubmitSms
        { SMS._username = T.pack username,
          SMS._password = T.pack password,
          SMS._from = SMS.JUSPAY,
          SMS._to = phoneNumber,
          SMS._category = SMS.BULK,
          SMS._text = SF.constructInviteSms orgName
        }
  whenLeft res $ \err -> L.throwException err503 {errBody = encode err}
