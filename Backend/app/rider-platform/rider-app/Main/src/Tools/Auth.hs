{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Auth where

import qualified Domain.Types.Person as Person
import qualified Domain.Types.RegistrationToken as SR
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.App
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Common as Utils
import Kernel.Utils.Monitoring.Prometheus.Servant
import Kernel.Utils.Servant.HeaderAuth
import Servant hiding (Context, throwError)
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import Tools.Error

-- | Performs simple token verification.
type TokenAuth = HeaderAuth "token" VerifyToken

data VerifyToken = VerifyToken

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (TokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

instance VerificationMethod VerifyToken where
  type VerificationResult VerifyToken = Id Person.Person
  verificationDescription =
    "Checks whether token is registered.\
    \If you don't have a token, use registration endpoints."

verifyPerson ::
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasField "authTokenCacheExpiry" r Seconds
  ) =>
  RegToken ->
  m (Id Person.Person)
verifyPerson token = do
  let key = authTokenCacheKey token
  authTokenCacheExpiry <- getSeconds <$> asks (.authTokenCacheExpiry)
  mbPersonId <- Redis.get key
  case mbPersonId of
    Just personId -> return personId
    Nothing -> do
      sr <- verifyToken token
      let expiryTime = min sr.tokenExpiry authTokenCacheExpiry
      let personId = Id sr.entityId
      Redis.setExp key personId expiryTime
      return personId

authTokenCacheKey :: RegToken -> Text
authTokenCacheKey regToken =
  "rider-platform:authTokenCacheKey:" <> regToken

verifyPersonAction ::
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasField "authTokenCacheExpiry" r Seconds
  ) =>
  VerificationAction VerifyToken m
verifyPersonAction = VerificationAction verifyPerson

verifyToken :: forall m r. EsqDBFlow m r => RegToken -> m SR.RegistrationToken
verifyToken token =
  RegistrationToken.findByToken token (Proxy @m)
    >>= Utils.fromMaybeM (InvalidToken token)
    >>= validateToken

validateToken :: EsqDBFlow m r => SR.RegistrationToken -> m SR.RegistrationToken
validateToken sr@SR.RegistrationToken {..} = do
  let nominal = realToFrac $ tokenExpiry * 24 * 60 * 60
  expired <- Utils.isExpired nominal updatedAt
  unless verified $ Utils.throwError TokenIsNotVerified
  when expired $ Utils.throwError TokenExpired
  return sr

-- TODO Next logic is the same for rider-app, beckn-transport and driver-offer-bpp. Move it to Lib

type DashboardTokenAuth = HeaderAuth "token" DashboardVerifyToken

data DashboardVerifyToken = DashboardVerifyToken

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (DashboardTokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

data Dashboard = Dashboard

instance VerificationMethod DashboardVerifyToken where
  type VerificationResult DashboardVerifyToken = Dashboard
  verificationDescription =
    "Checks whether dashboard token is registered."

verifyDashboardAction :: HasFlowEnv m r '["dashboardToken" ::: Text] => VerificationAction DashboardVerifyToken m
verifyDashboardAction = VerificationAction verifyDashboard

-- Do we need some expiry time for dashboard token?
verifyDashboard :: HasFlowEnv m r '["dashboardToken" ::: Text] => RegToken -> m Dashboard
verifyDashboard incomingToken = do
  dashboardToken <- asks (.dashboardToken)
  if incomingToken == dashboardToken
    then pure Dashboard
    else throwError (InvalidToken incomingToken)
