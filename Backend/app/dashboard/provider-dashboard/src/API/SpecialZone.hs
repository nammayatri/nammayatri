{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module API.SpecialZone
  ( API,
    handler,
  )
where

import qualified "special-zone" API.Types as SzAPI
import Dashboard.Common (HideSecrets)
import qualified "dashboard-helper-api" Dashboard.Common.SpecialZone as Common
import Domain.Types.ServerName
import "special-zone" Domain.Types.SpecialZone
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.External.Maps (LatLong)
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, withFlowHandlerAPI')
import qualified ProviderPlatformClient.SpecialZone as Client
import Servant
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Tools.Auth

type API =
  "specialZone"
    :> ( (ApiAuth 'SPECIAL_ZONE 'SPECIAL_ZONES 'SPECIAL_ZONE_LOOKUP :> SzAPI.RegionLookupAPI)
           :<|> (ApiAuth 'SPECIAL_ZONE 'SPECIAL_ZONES 'SPECIAL_ZONE_CREATE :> SzAPI.CreateSpecialZoneAPI)
           :<|> (ApiAuth 'SPECIAL_ZONE 'SPECIAL_ZONES 'SPECIAL_ZONE_UPDATE :> SzAPI.UpdateSpecialZoneAPI)
           :<|> (ApiAuth 'SPECIAL_ZONE 'SPECIAL_ZONES 'SPECIAL_ZONE_DELETE :> SzAPI.DeleteSpecialZoneAPI)
       )

handler :: FlowServer API
handler = lookupSpecialZone :<|> createSpecialZone :<|> updateSpecialZone :<|> deleteSpecialZone

instance HideSecrets SpecialZoneAPIEntity where
  hideSecrets = identity

instance HideSecrets SpecialZone where
  hideSecrets = identity

instance HideSecrets (Id SpecialZone) where
  hideSecrets = identity

buildTransaction ::
  ( MonadFlow m,
    HideSecrets request
  ) =>
  Common.SpecialZoneEndpoint ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint =
  T.buildTransaction (DT.SpecialZoneAPI endpoint) (Just SPECIAL_ZONE) Nothing Nothing Nothing

lookupSpecialZone :: ApiTokenInfo -> LatLong -> LatLong -> FlowHandler [SpecialZone]
lookupSpecialZone _ minLatLng maxLatLng = withFlowHandlerAPI' $ do
  Client.callSpecialZone (.lookupSpecialZone) minLatLng maxLatLng

createSpecialZone :: ApiTokenInfo -> SpecialZoneAPIEntity -> FlowHandler APISuccess
createSpecialZone _ req = withFlowHandlerAPI' $ do
  transaction <- buildTransaction Common.CreateSpecialZoneEndpoint (Just req)
  T.withTransactionStoring transaction $
    Client.callSpecialZone (.createSpecialZone) req

updateSpecialZone :: ApiTokenInfo -> SpecialZone -> FlowHandler APISuccess
updateSpecialZone _ req = withFlowHandlerAPI' $ do
  transaction <- buildTransaction Common.UpdateSpecialZoneEndpoint (Just req)
  T.withTransactionStoring transaction $
    Client.callSpecialZone (.updateSpecialZone) req

deleteSpecialZone :: ApiTokenInfo -> Id SpecialZone -> FlowHandler APISuccess
deleteSpecialZone _ req = withFlowHandlerAPI' $ do
  transaction <- buildTransaction Common.DeleteSpecialZoneEndpoint (Just req)
  T.withTransactionStoring transaction $
    Client.callSpecialZone (.deleteSpecialZone) req
