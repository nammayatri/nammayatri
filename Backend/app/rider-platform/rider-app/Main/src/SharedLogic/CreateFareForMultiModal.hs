{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CreateFareForMultiModal where

import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Types as JPT
import qualified Storage.Queries.Journey as QJourney
import Tools.Error

fareProcessingLockKey :: Text -> Text
fareProcessingLockKey journeyId = "Fare:Processing:JourneyId" <> journeyId

createFares :: Maybe JPT.JourneySearchData -> Price -> Flow () -> Flow ()
createFares journeyLegInfo requiredPrice updateInSearchReqFunc = do
  case journeyLegInfo of
    Just journeySearchData -> do
      Redis.withWaitOnLockRedisWithExpiry (fareProcessingLockKey journeySearchData.journeyId) 10 10 $ do
        let journeyId = journeySearchData.journeyId
        journey <- QJourney.findByPrimaryKey (Id journeyId) >>= fromMaybeM (InvalidRequest $ "Journey not found")
        case journey.estimatedFare of
          Just initialEstimatedFare -> do
            updatedEstimatedFare <- initialEstimatedFare `addPrice` requiredPrice
            QJourney.updateEstimatedFare (Just updatedEstimatedFare) (Id journeyId)
          Nothing -> do
            QJourney.updateEstimatedFare (Just requiredPrice) (Id journeyId)
        let legsDoneInitially = journey.legsDone
        QJourney.updateNumberOfLegs (legsDoneInitially + 1) (Id journeyId)
        updateInSearchReqFunc
    Nothing -> pure ()
