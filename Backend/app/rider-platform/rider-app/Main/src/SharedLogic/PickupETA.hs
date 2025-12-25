{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.PickupETA
  ( getPickupETAFromModel,
    PickupETAInput (..),
    PickupETAResult (..),
  )
where

import qualified Data.Aeson as A
import Data.Default.Class
import Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import Tools.DynamicLogic

data PickupETAInput = PickupETAInput
  { estimatedSpeedInMps :: Double,
    distanceToPickupInMeters :: Double
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance Default PickupETAInput where
  def = PickupETAInput {estimatedSpeedInMps = 0.0, distanceToPickupInMeters = 0.0}

data PickupETAResult = PickupETAResult
  {etaInMinutes :: Maybe Int}
  deriving (Generic, Show, ToJSON, FromJSON)

instance Default PickupETAResult where
  def = PickupETAResult {etaInMinutes = Nothing}

getPickupETAFromModel ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  Seconds ->
  Double ->
  Double ->
  Maybe Int ->
  Id DMOC.MerchantOperatingCity ->
  m (Maybe (Int, Int))
getPickupETAFromModel timeDiffFromUtc speedInMps distanceInMeters mbVersion merchantOperatingCityId = do
  localTime <- getLocalCurrentTime timeDiffFromUtc
  (allLogics, mbVersionReturned) <-
    getAppDynamicLogic
      (cast merchantOperatingCityId)
      LYT.PICKUP_ETA_CALCULATION
      localTime
      mbVersion
      Nothing

  if null allLogics
    then do
      logInfo $ "No PickupETALogics found for merchantOperatingCityId: " <> show merchantOperatingCityId
      return Nothing
    else do
      let inputData =
            PickupETAInput
              { estimatedSpeedInMps = speedInMps,
                distanceToPickupInMeters = distanceInMeters
              }

      response <- withTryCatch "runLogics:getPickupETAFromModel" $ LYTU.runLogics allLogics inputData
      logInfo $ "PickupETA Req Logics: " <> show allLogics <> " and data is: " <> show inputData <> " and response is: " <> show response

      case response of
        Left e -> do
          logError $ "Error in running PickupETALogics - " <> show e <> " - " <> show inputData <> " - " <> show allLogics
          return Nothing
        Right resp ->
          case (A.fromJSON resp.result :: A.Result PickupETAResult) of
            A.Success result -> do
              return $ (,) <$> result.etaInMinutes <*> mbVersionReturned
            A.Error err -> do
              logWarning $ "Error parsing PickupETAResult - " <> show err <> " - " <> show resp <> " - " <> show inputData <> " - " <> show allLogics
              return Nothing
