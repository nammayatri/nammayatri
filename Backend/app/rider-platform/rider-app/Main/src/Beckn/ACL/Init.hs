{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Init (buildInitReqV2) where

import qualified Beckn.OnDemand.Transformer.Init as TF
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils (computeTtlISO8601)
import Control.Lens ((%~))
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.App
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified SharedLogic.Confirm as SConfirm
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.CachedQueries.ValueAddNP as VNP
import Tools.Error

buildInitReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl], EncFlow m r, CacheFlow m r, EsqDBFlow m r) =>
  SConfirm.DConfirmRes ->
  m Spec.InitReq
buildInitReqV2 res = do
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack res.merchant.id.getId)
  moc <- CQMOC.findByMerchantIdAndCity res.merchant.id res.city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> res.merchant.id.getId <> "-city-" <> show res.city)
  riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = moc.id.getId, txnId = Nothing}) >>= fromMaybeM (RiderConfigDoesNotExist moc.id.getId)
  bapConfigs <- QBC.findByMerchantIdDomainandMerchantOperatingCityId res.merchant.id "MOBILITY" moc.id
  bapConfig <- listToMaybe bapConfigs & fromMaybeM (InvalidRequest $ "BecknConfig not found for merchantId " <> show res.merchant.id.getId <> " merchantOperatingCityId " <> show moc.id.getId) -- Using findAll for backward compatibility, TODO : Remove findAll and use findOne
  let action = Context.INIT
  let domain = Context.MOBILITY
  isValueAddNP <- VNP.isValueAddNP res.providerId
  ttl <- bapConfig.initTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
  TF.buildInitReq res bapUrl action domain isValueAddNP bapConfig riderConfig ttl
