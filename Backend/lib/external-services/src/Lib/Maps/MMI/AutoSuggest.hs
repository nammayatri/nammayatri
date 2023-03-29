{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Lib.Maps.MMI.AutoSuggest where

import Data.Maybe
import EulerHS.Prelude
import EulerHS.Types as ET
import Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common
import Lib.Error
import qualified Lib.Maps.MMI.MapsClient.Types as MMI
import Lib.Types (Language)
import Servant

type MMIAutoSuggestAPI =
  "api" :> "places" :> "search" :> "json"
    :> Header "Authorization" MMI.MMIAuthToken
    :> MandatoryQueryParam "query" Text
    :> MandatoryQueryParam "location" Text
    :> MandatoryQueryParam "region" Text
    :> MandatoryQueryParam "responseLang" Language
    :> Get '[JSON] MMI.AutoSuggestResp

mmiAutoSuggestAPI :: Proxy MMIAutoSuggestAPI
mmiAutoSuggestAPI = Proxy

mmiAutoSuggestClient :: Maybe MMI.MMIAuthToken -> Text -> Text -> Text -> Language -> ET.EulerClient MMI.AutoSuggestResp
mmiAutoSuggestClient = ET.client mmiAutoSuggestAPI

mmiAutoSuggest ::
  ( CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  BaseUrl ->
  Maybe MMI.MMIAuthToken ->
  Text ->
  Text ->
  Text ->
  Language ->
  m MMI.AutoSuggestResp
mmiAutoSuggest url authToken query location region lang = do
  callMMIAutoSuggestAPI
    url
    (mmiAutoSuggestClient authToken query location region lang)
    "mmi-auto-suggest"

callMMIAutoSuggestAPI :: CallAPI env a
callMMIAutoSuggestAPI =
  callApiUnwrappingApiError
    (identity @MMIError)
    Nothing
    (Just "MMI_AUTO_SUGGEST_ERROR")
