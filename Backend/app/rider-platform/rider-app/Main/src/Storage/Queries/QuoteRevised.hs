{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.QuoteRevised where

import Domain.Types.DriverOffer as DDO
import Domain.Types.Estimate
import Domain.Types.FarePolicy.FareProductType as DFFP
import Domain.Types.QuoteRevised as DQR
import Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOffer as BeamDO
import qualified Storage.Beam.QuoteRevised as BeamQR
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.DriverOffer as QueryDO
import Storage.Queries.RentalDetails as QueryRD
import Storage.Queries.SpecialZoneQuote as QuerySZQ
import qualified Storage.Queries.TripTerms as QTT

createDetails :: MonadFlow m => QuoteRevisedDetails -> m ()
createDetails = \case
  OneWayDetails _ -> pure ()
  RentalDetails rentalDetails -> QueryRD.createRentalDetails rentalDetails
  DriverOfferDetails driverOffer -> QueryDO.createDriverOffer driverOffer
  OneWaySpecialZoneDetails specialZoneQuote -> QuerySZQ.createSpecialZoneQuote specialZoneQuote
  InterCityDetails specialZoneQuote -> QuerySZQ.createSpecialZoneQuote specialZoneQuote

createQuoteRevised :: MonadFlow m => QuoteRevised -> m ()
createQuoteRevised = createWithKV

create :: MonadFlow m => QuoteRevised -> m ()
create quoteRevised = do
  logDebug $ "hello world onUpdate quote create start"
  traverse_ QTT.createTripTerms (quoteRevised.tripTerms)
  _ <- createDetails (quoteRevised.quoteRevisedDetails)
  createQuoteRevised quoteRevised
  logDebug $ "hello world onUpdate quote create end"

createMany :: MonadFlow m => [QuoteRevised] -> m ()
createMany = traverse_ create

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id QuoteRevised -> m (Maybe QuoteRevised)
findById quoteRevisedId = findOneWithKV [Se.Is BeamQR.id $ Se.Eq (getId quoteRevisedId)]

findByBppIdAndBPPQuoteRevisedId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Text -> m (Maybe QuoteRevised)
findByBppIdAndBPPQuoteRevisedId bppId bppQuoteRevisedId = do
  dOffer <- QueryDO.findByBPPQuoteId bppQuoteRevisedId
  quoteRevisedList <- findAllWithKV [Se.And [Se.Is BeamQR.providerId $ Se.Eq bppId, Se.Is BeamQR.driverOfferId $ Se.In (map (Just . getId . DDO.id) dOffer)]]
  let quoteWithDoOfferId = foldl' (getQuoteWithDOffer dOffer) [] quoteRevisedList
  pure $ listToMaybe quoteWithDoOfferId
  where
    getQuoteWithDOffer dOffer res quoteRevised = do
      let doId = case quoteRevised.quoteRevisedDetails of
            DQR.DriverOfferDetails driverOffer -> Just $ getId driverOffer.id
            _ -> Nothing
      ( if isJust doId
          then
            ( do
                let doffer' = filter (\d -> getId (d.id) == fromJust doId) dOffer
                 in res <> (quoteRevised <$ doffer')
            )
          else res
        )

findAllBySRId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id SearchRequest -> m [QuoteRevised]
findAllBySRId searchRequestId = findAllWithKV [Se.Is BeamQR.requestId $ Se.Eq (getId searchRequestId)]

findAllByEstimateId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Estimate -> DriverOfferStatus -> m [QuoteRevised]
findAllByEstimateId estimateId status = do
  driverOffers <- findDOfferByEstimateId estimateId status
  let offerIds = map (Just . getId . DDO.id) driverOffers
  findAllWithKV [Se.Is BeamQR.driverOfferId (Se.In offerIds)]

findDOfferByEstimateId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Estimate -> DriverOfferStatus -> m [DriverOffer]
findDOfferByEstimateId (Id estimateId) status = findAllWithKV [Se.And [Se.Is BeamDO.estimateId $ Se.Eq estimateId, Se.Is BeamDO.status $ Se.Eq status]]

instance FromTType' BeamQR.QuoteRevised QuoteRevised where
  fromTType' BeamQR.QuoteRevisedT {..} = do
    trip <- if isJust tripTermsId then QTT.findById'' (Id (fromJust tripTermsId)) else pure Nothing
    pUrl <- parseBaseUrl providerUrl

    quoteRevisedDetails <- case fareProductType of
      DFFP.ONE_WAY -> do
        distanceToNearestDriver' <- distanceToNearestDriver & fromMaybeM (QuoteFieldNotPresent "distanceToNearestDriver")
        pure . DQR.OneWayDetails $
          DQR.OneWayQuoteRevisedDetails
            { distanceToNearestDriver = distanceToNearestDriver'
            }
      DFFP.RENTAL -> do
        qd <- getRentalDetails rentalDetailsId
        maybe (throwError (InternalError "No rental details")) return qd
      DFFP.DRIVER_OFFER -> do
        qd <- getDriverOfferDetails driverOfferId
        maybe (throwError (InternalError "No driver offer details")) return qd
      DFFP.ONE_WAY_SPECIAL_ZONE -> do
        qd <- getSpecialZoneQuote specialZoneQuoteId
        maybe (throwError (InternalError "No special zone details")) return qd
      DFFP.INTER_CITY -> do
        qd <- getInterCityQuote specialZoneQuoteId
        maybe (throwError (InternalError "No inter city details")) return qd
    merchantOperatingCityId' <- backfillMOCId merchantOperatingCityId
    pure $
      Just
        QuoteRevised
          { id = Id id,
            requestId = Id requestId,
            estimatedFare = roundToIntegral estimatedFare,
            discount = roundToIntegral <$> discount,
            estimatedTotalFare = roundToIntegral estimatedTotalFare,
            merchantId = Id merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            quoteRevisedDetails = quoteRevisedDetails,
            providerId = providerId,
            itemId = itemId,
            providerUrl = pUrl,
            vehicleVariant = vehicleVariant,
            tripTerms = trip,
            specialLocationTag = specialLocationTag,
            createdAt = createdAt
          }
    where
      getRentalDetails rentalDetailsId' = do
        res <- maybe (pure Nothing) (QueryRD.findById . Id) rentalDetailsId'
        maybe (pure Nothing) (pure . Just . DQR.RentalDetails) res

      getDriverOfferDetails driverOfferId' = do
        res <- maybe (pure Nothing) (QueryDO.findById . Id) driverOfferId'
        maybe (pure Nothing) (pure . Just . DQR.DriverOfferDetails) res

      getSpecialZoneQuote specialZoneQuoteId' = do
        res <- maybe (pure Nothing) (QuerySZQ.findById . Id) specialZoneQuoteId'
        maybe (pure Nothing) (pure . Just . DQR.OneWaySpecialZoneDetails) res

      backfillMOCId = \case
        Just mocId -> pure $ Id mocId
        Nothing -> (.id) <$> CQM.getDefaultMerchantOperatingCity (Id merchantId)

      getInterCityQuote specialZoneQuoteId' = do
        res <- maybe (pure Nothing) (QuerySZQ.findById . Id) specialZoneQuoteId'
        maybe (pure Nothing) (pure . Just . DQR.InterCityDetails) res

instance ToTType' BeamQR.QuoteRevised QuoteRevised where
  toTType' QuoteRevised {..} =
    let (fareProductType, distanceToNearestDriver, rentalDetailsId, driverOfferId, specialZoneQuoteId) = case quoteRevisedDetails of
          DQR.OneWayDetails details -> (DFFP.ONE_WAY, Just $ details.distanceToNearestDriver, Nothing, Nothing, Nothing)
          DQR.RentalDetails rentalDetails -> (DFFP.RENTAL, Nothing, Just $ getId rentalDetails.id, Nothing, Nothing)
          DQR.DriverOfferDetails driverOffer -> (DFFP.DRIVER_OFFER, Nothing, Nothing, Just $ getId driverOffer.id, Nothing)
          DQR.OneWaySpecialZoneDetails specialZoneQuote -> (DFFP.ONE_WAY_SPECIAL_ZONE, Nothing, Nothing, Nothing, Just $ getId specialZoneQuote.id)
          DQR.InterCityDetails details -> (DFFP.INTER_CITY, Nothing, Nothing, Nothing, Just $ getId details.id)
     in BeamQR.QuoteRevisedT
          { BeamQR.id = getId id,
            BeamQR.fareProductType = fareProductType,
            BeamQR.requestId = getId requestId,
            BeamQR.estimatedFare = realToFrac estimatedFare,
            BeamQR.discount = realToFrac <$> discount,
            BeamQR.estimatedTotalFare = realToFrac estimatedTotalFare,
            BeamQR.providerId = providerId,
            BeamQR.itemId = itemId,
            BeamQR.providerUrl = showBaseUrl providerUrl,
            BeamQR.distanceToNearestDriver = distanceToNearestDriver,
            BeamQR.vehicleVariant = vehicleVariant,
            BeamQR.tripTermsId = getId <$> (tripTerms <&> (.id)),
            BeamQR.rentalDetailsId = rentalDetailsId,
            BeamQR.driverOfferId = driverOfferId,
            BeamQR.merchantId = getId merchantId,
            BeamQR.merchantOperatingCityId = Just $ getId merchantOperatingCityId,
            BeamQR.specialZoneQuoteId = specialZoneQuoteId,
            BeamQR.specialLocationTag = specialLocationTag,
            BeamQR.createdAt = createdAt
          }
