{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnTrack (API, handler) where

import qualified Beckn.ACL.OnTrack as ACL
import qualified Beckn.Types.Core.Taxi.API.OnTrack as OnTrack
import qualified Domain.Action.Beckn.OnTrack as DOnTrack
import Environment
import Kernel.Prelude
import Kernel.Types.Beckn.Ack
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.SystemConfigs ()

type API = OnTrack.OnTrackAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onTrack

onTrack ::
  SignatureAuthResult ->
  OnTrack.OnTrackReq ->
  FlowHandler AckResponse
onTrack _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnTrackReq <- ACL.buildOnTrackReq req
  whenJust mbDOnTrackReq \onTrackReq -> do
    validatedReq <- DOnTrack.validateRequest onTrackReq
    fork "on track processing" $
      DOnTrack.onTrack validatedReq
  pure Ack
