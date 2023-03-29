{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.GoogleTranslate.API where

import EulerHS.Prelude
import EulerHS.Types (EulerClient, client)
import Kernel.Utils.Common
import qualified Lib.GoogleTranslate.Types as GoogleTranslate
import Servant

type GoogleTranslateAPI =
  "language" :> "translate" :> "v2"
    :> MandatoryQueryParam "key" Text
    :> MandatoryQueryParam "source" Text
    :> MandatoryQueryParam "target" Text
    :> MandatoryQueryParam "q" Text
    :> Get '[JSON] GoogleTranslate.TranslateResp

translate :: Text -> Text -> Text -> Text -> EulerClient GoogleTranslate.TranslateResp
translate = client googleTranslateAPI

googleTranslateAPI :: Proxy GoogleTranslateAPI
googleTranslateAPI = Proxy
