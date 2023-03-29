{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Infobip.API.SendSms where

import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Lib.Infobip.Types
import Servant

type ServiceAPI =
  "sms" :> "1" :> "text" :> "query"
    :> QueryParam "username" Text
    :> QueryParam "password" Text
    :> QueryParam "from" Text
    :> QueryParam "to" Text
    :> QueryParam "text" Text
    :> QueryParam "indiaDltPrincipalEntityId" Text
    :> QueryParam "indiaDltContentTemplateId" Text
    :> QueryParam "notifyUrl" Text
    :> Post '[JSON] SMSRes

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

sendSms :: Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> ET.EulerClient SMSRes
sendSms username password from to text indiaDltPrincipalEntityId indiaDltContentTemplateId url = ET.client serviceAPI (Just username) (Just password) (Just from) (Just to) (Just text) (Just indiaDltContentTemplateId) (Just indiaDltPrincipalEntityId) (Just url)
