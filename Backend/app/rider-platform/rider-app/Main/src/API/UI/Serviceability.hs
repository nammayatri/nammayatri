{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Serviceability
  ( API,
    handler,
    ServiceabilityReq (..),
    DServiceability.ServiceabilityRes (..),
  )
where

import qualified Domain.Action.UI.Serviceability as DServiceability
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.Person as Person
import Environment
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified SharedLogic.CallBPPInternal as BPPInternal
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant as CQM
import Tools.Auth

-------- Serviceability----------
type API =
  "serviceability"
    :> TokenAuth
    :> ( "origin"
           :> ReqBody '[JSON] ServiceabilityReq
           :> Post '[JSON] DServiceability.ServiceabilityRes
           :<|> "destination"
             :> ReqBody '[JSON] ServiceabilityReq
             :> Post '[JSON] DServiceability.ServiceabilityRes
           :<|> "isInterCity"
             :> ReqBody '[JSON] BPPInternal.IsIntercityReq
             :> Post '[JSON] BPPInternal.IsIntercityResp
       )

newtype ServiceabilityReq = ServiceabilityReq
  { location :: LatLong
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

handler :: FlowServer API
handler regToken =
  checkOrignServiceability origin regToken
    :<|> checkDestinationServiceability destination regToken
    :<|> checkForIsInterCity regToken

checkOrignServiceability ::
  (GeofencingConfig -> GeoRestriction) ->
  (Id Person.Person, Id Merchant.Merchant) ->
  ServiceabilityReq ->
  FlowHandler DServiceability.ServiceabilityRes
checkOrignServiceability settingAccessor (personId, merchantId) ServiceabilityReq {..} = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  DServiceability.checkServiceability settingAccessor (personId, merchantId) location True True

checkDestinationServiceability ::
  (GeofencingConfig -> GeoRestriction) ->
  (Id Person.Person, Id Merchant.Merchant) ->
  ServiceabilityReq ->
  FlowHandler DServiceability.ServiceabilityRes
checkDestinationServiceability settingAccessor (personId, merchantId) ServiceabilityReq {..} = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  DServiceability.checkServiceability settingAccessor (personId, merchantId) location True False

checkForIsInterCity ::
  (Id Person.Person, Id Merchant.Merchant) ->
  BPPInternal.IsIntercityReq ->
  FlowHandler BPPInternal.IsIntercityResp
checkForIsInterCity (personId, merchantId) req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  eitherResp <- withTryCatch "getIsInterCity:checkForIsInterCity" (BPPInternal.getIsInterCity merchant req)
  case eitherResp of
    Left err -> do
      logDebug $ "Intercity API failed: " <> show err
      throwError RideNotServiceable
    Right resp -> return resp
