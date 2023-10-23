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
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.SearchRequest as Domain
import qualified Domain.Types.SearchRequest.SearchReqLocation as DSSL
import EulerHS.Prelude (whenNothingM_)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Beam.SearchRequest as BeamSR
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.SearchRequest.SearchReqLocation as QSRL

createDSReq' :: MonadFlow m => SearchRequest -> m ()
createDSReq' = createWithKV

create :: MonadFlow m => SearchRequest -> m ()
create dsReq = do
  case dsReq.searchRequestDetails of
    SearchReqDetailsOnDemand SearchRequestDetailsOnDemand {fromLocation, toLocation} -> do
      _ <- whenNothingM_ (QL.findById fromLocation.id) $ do QL.create fromLocation
      _ <- whenNothingM_ (QL.findById toLocation.id) $ do QL.create toLocation
      pure ()
    SearchReqDetailsRental SearchRequestDetailsRental {rentalFromLocation} -> do
      _ <- whenNothingM_ (QL.findById rentalFromLocation.id) $ do QL.create rentalFromLocation
      pure ()
  createDSReq' dsReq

createDSReq :: MonadFlow m => SearchRequest -> m ()
createDSReq searchRequest = do
  case searchRequest.searchRequestDetails of
    SearchReqDetailsOnDemand details -> do
      fromLocationMap <- SLM.buildPickUpLocationMapping details.fromLocation.id searchRequest.id.getId DLM.SEARCH_REQUEST
      toLocationMaps <- SLM.buildDropLocationMapping details.toLocation.id searchRequest.id.getId DLM.SEARCH_REQUEST
      QLM.create fromLocationMap >> QLM.create toLocationMaps >> create searchRequest
    SearchReqDetailsRental details -> do
      fromLocationMap <- SLM.buildPickUpLocationMapping details.rentalFromLocation.id searchRequest.id.getId DLM.SEARCH_REQUEST
      QLM.create fromLocationMap >> create searchRequest

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
    case tag of
      ON_DEMAND -> do
        mappings <- QLM.findByEntityId id
        (fl, tl) <-
          if null mappings -- HANDLING OLD DATA : TO BE REMOVED AFTER SOME TIME
            then do
              logInfo "Accessing Search Request Location Table"
              pickupLoc <- upsertLocationForOldData (Id <$> fromLocationId) id
              pickupLocMapping <- SLM.buildPickUpLocationMapping pickupLoc.id id DLM.SEARCH_REQUEST
              QLM.create pickupLocMapping

              dropLoc <- upsertLocationForOldData (Id <$> toLocationId) id
              dropLocMapping <- SLM.buildDropLocationMapping dropLoc.id id DLM.SEARCH_REQUEST
              QLM.create dropLocMapping
              return (pickupLoc, dropLoc)
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
                searchRequestDetails =
                  SearchReqDetailsOnDemand
                    SearchRequestDetailsOnDemand
                      { fromLocation = fl,
                        toLocation = tl,
                        estimatedDistance = estimatedDistance,
                        estimatedDuration = estimatedDuration,
                        specialLocationTag = specialLocationTag,
                        autoAssignEnabled = autoAssignEnabled
                      },
                area = area,
                bapId = bapId,
                bapUri = pUrl,
                bapCity = bapCity,
                bapCountry = bapCountry,
                customerLanguage = customerLanguage,
                disabilityTag = disabilityTag,
                device = device,
                createdAt = createdAt,
                tag = tag
              }
      RENTAL -> do
        mappings <- QLM.findByEntityId id
        fl <-
          if null mappings -- HANDLING OLD DATA : TO BE REMOVED AFTER SOME TIME
            then do
              logInfo "Accessing Search Request Location Table"
              pickupLoc <- upsertLocationForOldData (Id <$> fromLocationId) id
              pickupLocMapping <- SLM.buildPickUpLocationMapping pickupLoc.id id DLM.SEARCH_REQUEST
              QLM.create pickupLocMapping
              return pickupLoc
            else do
              let fromLocationMapping = filter (\loc -> loc.order == 0) mappings
              fromLocMap <- listToMaybe fromLocationMapping & fromMaybeM (InternalError "Entity Mappings For FromLocation Not Found")
              QL.findById fromLocMap.locationId >>= fromMaybeM (InternalError $ "FromLocation not found in search request for fromLocationId: " <> fromLocMap.locationId.getId)
        pUrl <- parseBaseUrl bapUri
        pure $
          Just
            SearchRequest
              { id = Id id,
                transactionId = transactionId,
                providerId = Id providerId,
                searchRequestDetails =
                  SearchReqDetailsRental
                    SearchRequestDetailsRental
                      { rentalFromLocation = fl
                      },
                area = area,
                bapId = bapId,
                bapUri = pUrl,
                bapCity = bapCity,
                bapCountry = bapCountry,
                customerLanguage = customerLanguage,
                disabilityTag = disabilityTag,
                device = device,
                createdAt = createdAt,
                tag = tag
              }

instance ToTType' BeamSR.SearchRequest SearchRequest where
  toTType' SearchRequest {..} = do
    case searchRequestDetails of
      SearchReqDetailsOnDemand details ->
        BeamSR.SearchRequestT
          { BeamSR.id = getId id,
            BeamSR.transactionId = transactionId,
            BeamSR.providerId = getId providerId,
            BeamSR.fromLocationId = Just $ getId details.fromLocation.id,
            BeamSR.toLocationId = Just $ getId details.toLocation.id,
            BeamSR.area = area,
            BeamSR.bapId = bapId,
            BeamSR.bapUri = showBaseUrl bapUri,
            BeamSR.bapCity = bapCity,
            BeamSR.bapCountry = bapCountry,
            BeamSR.estimatedDistance = details.estimatedDistance,
            BeamSR.estimatedDuration = details.estimatedDuration,
            BeamSR.customerLanguage = customerLanguage,
            BeamSR.disabilityTag = disabilityTag,
            BeamSR.device = device,
            BeamSR.createdAt = createdAt,
            BeamSR.autoAssignEnabled = details.autoAssignEnabled,
            BeamSR.specialLocationTag = details.specialLocationTag,
            BeamSR.tag = tag
          }
      SearchReqDetailsRental details ->
        BeamSR.SearchRequestT
          { BeamSR.id = getId id,
            BeamSR.transactionId = transactionId,
            BeamSR.providerId = getId providerId,
            BeamSR.fromLocationId = Just $ getId details.rentalFromLocation.id,
            BeamSR.toLocationId = Nothing,
            BeamSR.area = area,
            BeamSR.bapId = bapId,
            BeamSR.bapUri = showBaseUrl bapUri,
            BeamSR.bapCity = bapCity,
            BeamSR.bapCountry = bapCountry,
            BeamSR.estimatedDistance = 0,
            BeamSR.estimatedDuration = 0,
            BeamSR.customerLanguage = customerLanguage,
            BeamSR.disabilityTag = disabilityTag,
            BeamSR.device = device,
            BeamSR.createdAt = createdAt,
            BeamSR.autoAssignEnabled = Nothing,
            BeamSR.specialLocationTag = Nothing,
            BeamSR.tag = tag
          }

-- FUNCTIONS FOR HANDLING OLD DATA : TO BE REMOVED AFTER SOME TIME

buildLocation :: MonadFlow m => DSSL.SearchReqLocation -> m DL.Location
buildLocation DSSL.SearchReqLocation {..} =
  return $
    DL.Location
      { id = cast id,
        address =
          DL.LocationAddress
            { fullAddress = full_address,
              ..
            },
        ..
      }

upsertLocationForOldData :: MonadFlow m => Maybe (Id DSSL.SearchReqLocation) -> Text -> m DL.Location
upsertLocationForOldData locationId searchReqId = do
  loc <- QSRL.findById `mapM` locationId >>= fromMaybeM (InternalError "LocationId Not Found in Search Request Location Table")
  location <- maybe (throwError $ InternalError $ "Location not found in SearchRequest for Search Request Id:" <> show searchReqId) buildLocation loc
  void $ QL.create location
  return location
