{-# LANGUAGE OverloadedLabels #-}

module Utils.Common where

import Beckn.TypeClass.Transform
import Beckn.Types.App
import qualified Beckn.Types.Storage.Organization as SO
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import Beckn.Utils.Servant.Auth
import Data.Generics.Labels
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Organization as QO
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import Types.API.Person

-- | Performs simple token verification.
type TokenAuth = TokenAuth' VerifyToken

data VerifyToken = VerifyToken

instance VerificationMethod VerifyToken where
  type VerificationResult VerifyToken = SR.RegistrationToken
  verifyToken = QR.verifyToken
  verificationDescription =
    "Checks whether token is registered.\
    \If you don't have a token, use registration endpoints."

-- | Verifies org's token
type OrgTokenAuth = TokenAuth' OrgVerifyToken

data OrgVerifyToken = OrgVerifyToken

instance VerificationMethod OrgVerifyToken where
  type VerificationResult OrgVerifyToken = SO.Organization
  verifyToken = QO.verifyToken
  verificationDescription =
    "Checks whether token is registered.\
    \If you don't have a token, use registration endpoints."

-- | Verifies admin's token.
type AdminTokenAuth = TokenAuth' AdminVerifyToken

data AdminVerifyToken

instance VerificationMethod AdminVerifyToken where
  type VerificationResult AdminVerifyToken = Text
  verifyToken = validateAdmin
  verificationDescription =
    "Checks whether token is registered and belongs to a person with admin role."

-- | Verifies admin or driver's token.
type DriverTokenAuth = TokenAuth' DriverVerifyToken

data DriverVerifyToken

instance VerificationMethod DriverVerifyToken where
  type VerificationResult DriverVerifyToken = Text
  verifyToken = validateDriver
  verificationDescription =
    "Checks whether token is registered and belongs to a person with admin or driver role."

verifyAdmin :: SP.Person -> L.Flow Text
verifyAdmin user = do
  when (user ^. #_role /= SP.ADMIN) $
    L.throwException $
      err400 {errBody = "NEED_ADMIN_ACCESS"}
  case user ^. #_organizationId of
    Just orgId -> return orgId
    Nothing -> L.throwException $ err400 {errBody = "NO_ORGANIZATION_FOR_THIS_USER"}

verifyDriver :: SP.Person -> L.Flow Text
verifyDriver user = do
  unless ((user ^. #_role) `elem` [SP.ADMIN, SP.DRIVER]) $
    L.throwException $
      err400 {errBody = "NEED_ADMIN_OR_DRIVER_ACCESS"}
  case user ^. #_organizationId of
    Just orgId -> return orgId
    Nothing -> L.throwException $ err400 {errBody = "NO_ORGANIZATION_FOR_THIS_USER"}

validateAdmin :: RegToken -> L.Flow Text
validateAdmin regToken = do
  SR.RegistrationToken {..} <- QR.verifyToken regToken
  user <- QP.findPersonById (PersonId _EntityId)
  verifyAdmin user

validateDriver :: RegToken -> L.Flow Text
validateDriver regToken = do
  SR.RegistrationToken {..} <- QR.verifyToken regToken
  user <- QP.findPersonById (PersonId _EntityId)
  verifyDriver user
