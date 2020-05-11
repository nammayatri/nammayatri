module Product.Transporter where

import Beckn.TypeClass.Transform
import Beckn.Types.App
import qualified Beckn.Types.Storage.Organization as SO
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Organization as QO
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import Types.API.Transporter
import Types.App

createTransporter :: Text -> TransporterReq -> FlowHandler TransporterRes
createTransporter regToken req = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.findRegistrationTokenByToken regToken
  person <- QP.findPersonById (PersonId _EntityId)
  validation person
  organization <- transformFlow req
  QP.updateOrganizationId (PersonId _EntityId) (_getOrganizationId $ SO._id organization)
  QO.create organization
  return $ TransporterRes (Just person) organization
  where
    validation person = do
      whenM (return $ not $ SP._verified person) $ L.throwException $ err400 {errBody = "user not verified"}
      whenM (return $ SP._organizationId person /= Nothing) $ L.throwException $ err400 {errBody = "user already registered an organization"}

createGateway :: Maybe Text -> TransporterReq -> FlowHandler TransporterRes
createGateway auth req = withFlowHandler $ do
  QO.verifyAuth auth
  organization              <- transformFlow req
  QO.create organization
  return $ TransporterRes Nothing organization
