{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module API.UI.Sos
  ( API,
    handler,
  )
where

import qualified API.Action.UI.Sos as GeneratedSos
import qualified Domain.Action.UI.Sos as DSos
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude
import Kernel.ServantMultipart
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Safety.Domain.Types.Sos as Sos
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type UploadAPI =
  TokenAuth
    :> "sos"
    :> Capture "sosId" (Id Sos.Sos)
    :> "upload"
    :> MultipartForm Tmp DSos.SOSVideoUploadReq
    :> Post '[JSON] DSos.AddSosVideoRes

type API = GeneratedSos.API :<|> UploadAPI

handler :: Environment.FlowServer API
handler = GeneratedSos.handler :<|> uploadMediaHandler

uploadMediaHandler ::
  (Id Person.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) ->
  Id Sos.Sos ->
  DSos.SOSVideoUploadReq ->
  Environment.FlowHandler DSos.AddSosVideoRes
uploadMediaHandler (personId, _, _) sosId = withFlowHandlerAPI . DSos.uploadMedia sosId personId
