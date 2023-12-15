{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.GoogleTranslate
  ( API,
    handler,
  )
where

import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment (FlowHandler, FlowServer)
import EulerHS.Prelude
import qualified Kernel.External.GoogleTranslate.Types as GoogleTranslate
import Kernel.Types.App
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI, withPersonIdLogTag)
import Servant
import qualified SharedLogic.GoogleTranslate as GoogleTranslate
import Storage.Beam.SystemConfigs ()
import Tools.Auth
import qualified Tools.Maps as Maps

type API =
  "language"
    :> ( "translate"
           :> TokenAuth
           :> MandatoryQueryParam "source" Maps.Language
           :> MandatoryQueryParam "target" Maps.Language
           :> MandatoryQueryParam "q" Text
           :> Get '[JSON] GoogleTranslate.TranslateResp
       )

handler :: FlowServer API
handler =
  translate

translate :: (Id Person.Person, Id Merchant.Merchant) -> Maps.Language -> Maps.Language -> Text -> FlowHandler GoogleTranslate.TranslateResp
translate (personId, _) source target = withFlowHandlerAPI . withPersonIdLogTag personId . GoogleTranslate.translate source target
