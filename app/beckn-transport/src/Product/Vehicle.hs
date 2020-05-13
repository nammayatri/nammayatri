module Product.Vehicle where

import Beckn.TypeClass.Transform
import Beckn.Types.App
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import EulerHS.Prelude
import qualified Storage.Queries.Vehicle as QV
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.Person as QP
import Types.API.Vehicle
import Servant
import qualified EulerHS.Language as L

createVehicle :: Maybe Text -> CreateVehicleReq -> FlowHandler CreateVehicleRes
createVehicle token req = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth token
  verifyAdmin _EntityId
  vehicle <- transformFlow req
  QV.create vehicle
  return $ CreateVehicleRes vehicle

  where
    verifyAdmin entityId = do
      user <- QP.findPersonById (PersonId entityId)
      whenM (return $ SP._role user /= SP.ADMIN) $ L.throwException $ err400 {errBody = "NEED_ADMIN_ACCESS"}
