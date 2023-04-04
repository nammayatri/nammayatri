{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SMS.MyValueFirst.API where

import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.Types.App
  ( MandatoryQueryParam,
  )
import Kernel.Types.Servant
import SMS.MyValueFirst.Types
import Servant

type ServiceAPI =
  "smpp"
    :> "sendsms"
    :> MandatoryQueryParam "username" Text
    :> MandatoryQueryParam "password" Text
    :> MandatoryQueryParam "from" Text
    :> MandatoryQueryParam "to" Text
    :> MandatoryQueryParam "text" Text
    :> Post '[PlainText_ISO_8859_1] SubmitSmsRes

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

submitSms :: SubmitSms -> ET.EulerClient SubmitSmsRes
submitSms SubmitSms {..} = ET.client serviceAPI username password from to text
