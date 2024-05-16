{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module SharedLogic.DriverPool.Config
  {-# WARNING
    "This module contains direct calls to the table and redis. \
  \ But most likely you need a version from Cac with inMem results feature."
    #-}
  ( module SharedLogic.DriverPool.Config,
    module CDP,
  )
where

import Control.Applicative ((<|>))
import Data.Text as Text hiding (find)
import Domain.Types.DriverPoolConfig
import Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.ServiceTierType as DVST
import Kernel.Prelude as KP
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow)
import Kernel.Utils.Error
import qualified Lib.Types.SpecialLocation as SL
import Storage.Beam.SystemConfigs ()
import Storage.CachedQueries.Merchant.DriverPoolConfig as CDP

getDriverPoolConfigFromDB ::
  KvDbFlow m r =>
  Id MerchantOperatingCity ->
  Maybe DVST.ServiceTierType ->
  String ->
  SL.Area ->
  Maybe Meters ->
  m (Maybe DriverPoolConfig)
getDriverPoolConfigFromDB merchantOpCityId serviceTier tripCategory area mbDist = do
  let distance = fromMaybe 0 mbDist
  configs <- CDP.findAllByMerchantOpCityId merchantOpCityId
  let mbApplicableConfig =
        find (filterByDistAndDvehAndArea serviceTier (Text.pack tripCategory) distance area) configs
          <|> find (filterByDistAndDvehAndArea serviceTier (Text.pack tripCategory) distance SL.Default) configs
  case configs of
    [] -> throwError $ InvalidRequest $ "DriverPool Configs not found for MerchantOperatingCity: " <> merchantOpCityId.getId
    _ ->
      case mbApplicableConfig of
        Just applicableConfig -> return $ Just applicableConfig
        Nothing -> do
          let alternativeConfigs =
                find (filterByDistAndDvehAndArea serviceTier "All" distance area) configs
                  <|> find (filterByDistAndDvehAndArea serviceTier "All" distance SL.Default) configs
          case alternativeConfigs of
            Just cfg -> return $ Just cfg
            Nothing -> Just <$> findDriverPoolConfig configs Nothing "All" distance area

filterByDistAndDvehAndArea :: Maybe DVST.ServiceTierType -> Text -> Meters -> SL.Area -> DriverPoolConfig -> Bool
filterByDistAndDvehAndArea serviceTier tripCategory dist area cfg =
  dist >= cfg.tripDistance && cfg.vehicleVariant == serviceTier && cfg.tripCategory == tripCategory && cfg.area == area

findDriverPoolConfig :: (KvDbFlow m r) => [DriverPoolConfig] -> Maybe DVST.ServiceTierType -> Text -> Meters -> SL.Area -> m DriverPoolConfig
findDriverPoolConfig configs serviceTier tripCategory dist area = do
  find (filterByDistAndDvehAndArea serviceTier tripCategory dist area) configs
    <|> find (filterByDistAndDvehAndArea serviceTier tripCategory dist SL.Default) configs
      & fromMaybeM (InvalidRequest $ "DriverPool Config not found: " <> show serviceTier <> show tripCategory <> show dist)
