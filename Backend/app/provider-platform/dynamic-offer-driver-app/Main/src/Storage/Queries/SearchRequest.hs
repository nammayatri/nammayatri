{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.SearchRequest where

import Data.Ord
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.SearchRequest as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified SharedLogic.Location as SL
import qualified Storage.Beam.SearchRequest as BeamSR
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM

createDSReq' :: MonadFlow m => SearchRequest -> m ()
createDSReq' = createWithKV

create :: MonadFlow m => SearchRequest -> m ()
create dsReq = do
  SL.createLocation dsReq.fromLocation >> SL.createLocation dsReq.toLocation >> createDSReq' dsReq

createDSReq :: MonadFlow m => SearchRequest -> m ()
createDSReq searchRequest = do
  SL.createPickupLocationMapping searchRequest.fromLocation.id searchRequest.id.getId DLM.SEARCH_REQUEST
    >> SL.createDropLocationMapping searchRequest.toLocation.id searchRequest.id.getId DLM.SEARCH_REQUEST
    >> create searchRequest

findById :: MonadFlow m => Id SearchRequest -> m (Maybe SearchRequest)
findById (Id searchRequestId) = findOneWithKV [Se.Is BeamSR.id $ Se.Eq searchRequestId]

getRequestIdfromTransactionId :: MonadFlow m => Id SearchRequest -> m (Maybe (Id SearchRequest))
getRequestIdfromTransactionId (Id tId) = findOneWithKV [Se.Is BeamSR.transactionId $ Se.Eq tId] <&> fmap Domain.id

findByTransactionId :: MonadFlow m => Text -> m (Maybe (Id SearchRequest))
findByTransactionId transactionId = findOneWithKV [Se.And [Se.Is BeamSR.transactionId $ Se.Eq transactionId]] <&> (Domain.id <$>)

updateAutoAssign ::
  MonadFlow m =>
  Id SearchRequest ->
  Bool ->
  m ()
updateAutoAssign searchRequestId autoAssignedEnabled =
  updateOneWithKV
    [Se.Set BeamSR.autoAssignEnabled $ Just autoAssignedEnabled]
    [Se.Is BeamSR.id (Se.Eq $ getId searchRequestId)]

instance FromTType' BeamSR.SearchRequest SearchRequest where
  fromTType' BeamSR.SearchRequestT {..} = do
    mappings <- QLM.findByEntityId id
    (fl, tl) <-
      if null mappings -- HANDLING OLD DATA : TO BE REMOVED AFTER SOME TIME
        then do
          logInfo "Accessing Search Request Location Table"
          SL.backfillLocationAndLocationMapping fromLocationId toLocationId id DLM.SEARCH_REQUEST
        else do
          let fromLocationMapping = filter (\loc -> loc.order == 0) mappings
              toLocationMappings = filter (\loc -> loc.order /= 0) mappings

          fromLocMap <- listToMaybe fromLocationMapping & fromMaybeM (InternalError "Entity Mappings For FromLocation Not Found")
          fl <- QL.findById fromLocMap.locationId >>= fromMaybeM (InternalError $ "FromLocation not found in search request for fromLocationId: " <> fromLocMap.locationId.getId)

          when (null toLocationMappings) $ throwError (InternalError "Entity Mappings For ToLocation Not Found")
          let toLocMap = maximumBy (comparing (.order)) toLocationMappings
          tl <- QL.findById toLocMap.locationId >>= fromMaybeM (InternalError $ "ToLocation not found in search request for toLocationId: " <> toLocMap.locationId.getId)
          return (fl, tl)
    pUrl <- parseBaseUrl bapUri
    pure $
      Just
        SearchRequest
          { id = Id id,
            transactionId = transactionId,
            providerId = Id providerId,
            fromLocation = fl,
            toLocation = tl,
            area = area,
            bapId = bapId,
            bapUri = pUrl,
            bapCity = bapCity,
            bapCountry = bapCountry,
            estimatedDistance = estimatedDistance,
            estimatedDuration = estimatedDuration,
            customerLanguage = customerLanguage,
            disabilityTag = disabilityTag,
            device = device,
            createdAt = createdAt,
            specialLocationTag = specialLocationTag,
            autoAssignEnabled = autoAssignEnabled
          }

instance ToTType' BeamSR.SearchRequest SearchRequest where
  toTType' SearchRequest {..} = do
    BeamSR.SearchRequestT
      { BeamSR.id = getId id,
        BeamSR.transactionId = transactionId,
        BeamSR.providerId = getId providerId,
        BeamSR.fromLocationId = Just $ getId fromLocation.id,
        BeamSR.toLocationId = Just $ getId toLocation.id,
        BeamSR.area = area,
        BeamSR.bapId = bapId,
        BeamSR.bapUri = showBaseUrl bapUri,
        BeamSR.bapCity = bapCity,
        BeamSR.bapCountry = bapCountry,
        BeamSR.estimatedDistance = estimatedDistance,
        BeamSR.estimatedDuration = estimatedDuration,
        BeamSR.customerLanguage = customerLanguage,
        BeamSR.disabilityTag = disabilityTag,
        BeamSR.device = device,
        BeamSR.createdAt = createdAt,
        BeamSR.autoAssignEnabled = autoAssignEnabled,
        BeamSR.specialLocationTag = specialLocationTag
      }
