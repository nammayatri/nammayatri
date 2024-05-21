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
import Data.Text as Text hiding (filter, find, foldl)
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import Domain.Types.DriverPoolConfig
import qualified Domain.Types.Extra.TimeBound as DTB
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ServiceTierType as DVST
import Kernel.Prelude as KP
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation as SL
import Storage.Beam.SystemConfigs ()
import qualified Storage.Cac.TransporterConfig as CTC
import Storage.CachedQueries.Merchant.DriverPoolConfig as CDP

getDriverPoolConfigFromDB ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe DVST.ServiceTierType ->
  String ->
  SL.Area ->
  Maybe Meters ->
  Maybe Seconds ->
  m (Maybe DriverPoolConfig)
getDriverPoolConfigFromDB merchantOpCityId serviceTier tripCategory area mbDist mbDuration = do
  let distance = fromMaybe 0 mbDist
  configs <- CDP.findAllByMerchantOpCityId merchantOpCityId
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc -- bounds, all these params, timeDiffFromUTC
  let longestConfig = case (mbDuration, transporterConfig.interCityAdvancedPoolingDurationThreshold) of
        (Just duration, Just threshold) -> (secondsToNominalDiffTime duration) >= threshold
        _ -> False
  let mbApplicableConfig =
        findBoundedConfigByDistVehAndArea serviceTier (Text.pack tripCategory) distance longestConfig area configs localTime -- Need to disable CAC for this table till we can handle findAll
          <|> find (filterByDistAndDvehAndArea serviceTier (Text.pack tripCategory) distance area) configs
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

findBoundedConfigByDistVehAndArea :: Maybe DVST.ServiceTierType -> Text -> Meters -> Bool -> SL.Area -> [DriverPoolConfig] -> UTCTime -> Maybe DriverPoolConfig
findBoundedConfigByDistVehAndArea serviceTier tripCategory dist longestConfig area configs localTime = do
  let suitableConfigs = filter (\cfg -> cfg.vehicleVariant == serviceTier && cfg.tripCategory == tripCategory && cfg.area == area) configs
      filteredConfigs = if (longestConfig && tripCategory == "InterCity_OnDemandStaticOffer") then suitableConfigs else filter (\cfg -> dist >= cfg.tripDistance) suitableConfigs
  DTB.findBoundedDomain filteredConfigs localTime

findDriverPoolConfig :: (EsqDBFlow m r) => [DriverPoolConfig] -> Maybe DVST.ServiceTierType -> Text -> Meters -> SL.Area -> m DriverPoolConfig
findDriverPoolConfig configs serviceTier tripCategory dist area = do
  find (filterByDistAndDvehAndArea serviceTier tripCategory dist area) configs
    <|> find (filterByDistAndDvehAndArea serviceTier tripCategory dist SL.Default) configs
      & fromMaybeM (InvalidRequest $ "DriverPool Config not found: " <> show serviceTier <> show tripCategory <> show dist)
