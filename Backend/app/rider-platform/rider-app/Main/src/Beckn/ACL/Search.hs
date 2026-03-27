{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Search
  ( buildSearchReqV2,
  )
where

import qualified Beckn.OnDemand.Transformer.Search as Search
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Types as Spec
import Data.Maybe
import EulerHS.Prelude hiding (state, (%~))
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import SharedLogic.Search as SLS
import Storage.ConfigPilot.Config.BecknConfig (BecknConfigDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)

buildSearchReqV2 ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  SLS.SearchRes ->
  m Spec.SearchReq
buildSearchReqV2 res@SLS.SearchRes {..} = do
  bapUri <- Utils.mkBapUri merchant.id
  bapConfig <- (listToMaybe <$> getConfig (BecknConfigDimensions {merchantOperatingCityId = res.merchantOperatingCityId.getId, merchantId = merchant.id.getId, domain = Just "MOBILITY", vehicleCategory = Nothing})) >>= fromMaybeM (InternalError $ "Beckn Config not found for merchantId:-" <> show merchant.id.getId <> "merchantOperatingCityId:-" <> show merchantOperatingCityId.getId)
  messageId <- generateGUIDText
  let eBecknSearchReq = Search.buildBecknSearchReqV2 res bapConfig bapUri messageId
  case eBecknSearchReq of
    Left err -> throwError $ InternalError err
    Right becknSearchReq -> pure becknSearchReq
