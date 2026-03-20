{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.CancelLogic
  ( CancellationReasonConfig (..),
    CancellationReasonInput (..),
    computeCancellationReasons,
  )
where

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Tools.DynamicLogic as DynamicLogic

data CancellationReasonConfig = CancellationReasonConfig
  { code :: Text,
    iconUrl :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CancellationReasonInput = CancellationReasonInput
  { hasRideAssigned :: Bool,
    isAirConditioned :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

computeCancellationReasons ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Bool ->
  Bool ->
  m [CancellationReasonConfig]
computeCancellationReasons merchantOpCityId hasRideAssigned isAC = do
  localTime <- getLocalCurrentTime (19800 :: Seconds)
  let inputData =
        CancellationReasonInput
          { hasRideAssigned = hasRideAssigned,
            isAirConditioned = isAC
          }
  (allLogics, _mbVersion) <-
    DynamicLogic.getAppDynamicLogic
      (cast merchantOpCityId)
      LYT.CANCELLATION_REASONS
      localTime
      Nothing
      Nothing
  if null allLogics
    then do
      logTagInfo ("MOC-" <> merchantOpCityId.getId) "No CancellationReasons logic configured; returning []."
      return []
    else do
      response <- withTryCatch "runLogics:CancellationReasons" $ LYTU.runLogics allLogics inputData
      case response of
        Left e -> do
          logError $ "Error in running CancellationReasons logics - " <> show e
          return []
        Right resp -> do
          let parsed = A.fromJSON @(HM.HashMap Text [CancellationReasonConfig]) resp.result
          case parsed of
            A.Success hm -> do
              let reasons = fromMaybe [] $ HM.lookup "reasons" hm
              logInfo $ "CancellationReasons result: " <> show reasons
              return reasons
            A.Error err -> do
              logWarning $ "Error parsing CancellationReasons - " <> show err <> " - " <> show resp.result
              return []
