{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Quote where

import Domain.Types.DriverOffer as DDO
import Domain.Types.Estimate
import Domain.Types.FarePolicy.FareProductType as DFFP
import Domain.Types.Quote as DQ
import Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Error
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOffer as BeamDO
import qualified Storage.Beam.Quote as BeamQ
import qualified Storage.Queries.DriverOffer as QueryDO
import qualified Storage.Queries.PublicTransportQuote as QueryPTQ
import Storage.Queries.RentalSlab as QueryRS
import Storage.Queries.SpecialZoneQuote as QuerySZQ
import qualified Storage.Queries.TripTerms as QTT

createDetails :: MonadFlow m => QuoteDetails -> m ()
createDetails = \case
  OneWayDetails _ -> pure ()
  RentalDetails rentalSlab -> QueryRS.createRentalSlab rentalSlab
  DriverOfferDetails driverOffer -> QueryDO.createDriverOffer driverOffer
  OneWaySpecialZoneDetails specialZoneQuote -> QuerySZQ.createSpecialZoneQuote specialZoneQuote
  PublicTransportQuoteDetails publicTransportQuote -> QueryPTQ.createPublicTransportQuote publicTransportQuote

createQuote :: MonadFlow m => Quote -> m ()
createQuote = createWithKV

create :: MonadFlow m => Quote -> m ()
create quote = do
  traverse_ QTT.createTripTerms (quote.tripTerms)
  _ <- createDetails (quote.quoteDetails)
  createQuote quote

createMany :: MonadFlow m => [Quote] -> m ()
createMany = traverse_ create

findById :: MonadFlow m => Id Quote -> m (Maybe Quote)
findById quoteId = findOneWithKV [Se.Is BeamQ.id $ Se.Eq (getId quoteId)]

findByBppIdAndBPPQuoteId :: MonadFlow m => Text -> Text -> m (Maybe Quote)
findByBppIdAndBPPQuoteId bppId bppQuoteId = do
  dOffer <- QueryDO.findByBPPQuoteId bppQuoteId
  quoteList <- findAllWithKV [Se.And [Se.Is BeamQ.providerId $ Se.Eq bppId, Se.Is BeamQ.driverOfferId $ Se.In (map (Just . getId . DDO.id) dOffer)]]
  let quoteWithDoOfferId = foldl' (getQuoteWithDOffer dOffer) [] quoteList
  pure $ listToMaybe quoteWithDoOfferId
  where
    getQuoteWithDOffer dOffer res quote = do
      let doId = case quote.quoteDetails of
            DQ.DriverOfferDetails driverOffer -> Just $ getId driverOffer.id
            _ -> Nothing
      ( if isJust doId
          then
            ( do
                let doffer' = filter (\d -> getId (d.id) == fromJust doId) dOffer
                 in res <> (quote <$ doffer')
            )
          else res
        )

findAllBySRId :: MonadFlow m => Id SearchRequest -> m [Quote]
findAllBySRId searchRequestId = findAllWithKV [Se.Is BeamQ.requestId $ Se.Eq (getId searchRequestId)]

findAllByEstimateId :: MonadFlow m => Id Estimate -> DriverOfferStatus -> m [Quote]
findAllByEstimateId estimateId status = do
  driverOffers <- findDOfferByEstimateId estimateId status
  let offerIds = map (Just . getId . DDO.id) driverOffers
  findAllWithKV [Se.Is BeamQ.driverOfferId (Se.In offerIds)]

findDOfferByEstimateId :: MonadFlow m => Id Estimate -> DriverOfferStatus -> m [DriverOffer]
findDOfferByEstimateId (Id estimateId) status = findAllWithKV [Se.And [Se.Is BeamDO.estimateId $ Se.Eq estimateId, Se.Is BeamDO.status $ Se.Eq status]]

instance FromTType' BeamQ.Quote Quote where
  fromTType' BeamQ.QuoteT {..} = do
    trip <- if isJust tripTermsId then QTT.findById'' (Id (fromJust tripTermsId)) else pure Nothing
    pUrl <- parseBaseUrl providerUrl

    quoteDetails <- case fareProductType of
      DFFP.ONE_WAY -> do
        distanceToNearestDriver' <- distanceToNearestDriver & fromMaybeM (QuoteFieldNotPresent "distanceToNearestDriver")
        pure . DQ.OneWayDetails $
          DQ.OneWayQuoteDetails
            { distanceToNearestDriver = distanceToNearestDriver'
            }
      DFFP.RENTAL -> do
        qd <- getRentalDetails rentalSlabId
        maybe (throwError (InternalError "No rental details")) return qd
      DFFP.DRIVER_OFFER -> do
        qd <- getDriverOfferDetails driverOfferId
        maybe (throwError (InternalError "No driver offer details")) return qd
      DFFP.ONE_WAY_SPECIAL_ZONE -> do
        qd <- getSpecialZoneQuote specialZoneQuoteId
        maybe (throwError (InternalError "No special zone details")) return qd
      DFFP.PUBLIC_TRANSPORT -> do
        qd <- getPublicTransportDetails publicTransportQuoteId
        maybe (throwError (InternalError "No public transport details")) return qd
    pure $
      Just
        Quote
          { id = Id id,
            requestId = Id requestId,
            estimatedFare = roundToIntegral estimatedFare,
            discount = roundToIntegral <$> discount,
            estimatedTotalFare = roundToIntegral estimatedTotalFare,
            merchantId = Id merchantId,
            quoteDetails = quoteDetails,
            providerId = providerId,
            itemId = itemId,
            providerUrl = pUrl,
            providerName = providerName,
            providerMobileNumber = providerMobileNumber,
            providerCompletedRidesCount = providerCompletedRidesCount,
            vehicleVariant = vehicleVariant,
            tripTerms = trip,
            specialLocationTag = specialLocationTag,
            createdAt = createdAt
          }
    where
      getRentalDetails rentalSlabId' = do
        res <- maybe (pure Nothing) (QueryRS.findById . Id) rentalSlabId'
        maybe (pure Nothing) (pure . Just . DQ.RentalDetails) res

      getDriverOfferDetails driverOfferId' = do
        res <- maybe (pure Nothing) (QueryDO.findById . Id) driverOfferId'
        maybe (pure Nothing) (pure . Just . DQ.DriverOfferDetails) res

      getSpecialZoneQuote specialZoneQuoteId' = do
        res <- maybe (pure Nothing) (QuerySZQ.findById . Id) specialZoneQuoteId'
        maybe (pure Nothing) (pure . Just . DQ.OneWaySpecialZoneDetails) res

      getPublicTransportDetails publicTransportQuoteId' = do
        res <- maybe (pure Nothing) (QueryPTQ.findById . Id) publicTransportQuoteId'
        maybe (pure Nothing) (pure . Just . DQ.PublicTransportQuoteDetails) res

instance ToTType' BeamQ.Quote Quote where
  toTType' Quote {..} =
    let (fareProductType, distanceToNearestDriver, rentalSlabId, driverOfferId, specialZoneQuoteId, publicTransportQuoteId) = case quoteDetails of
          DQ.OneWayDetails details -> (DFFP.ONE_WAY, Just $ details.distanceToNearestDriver, Nothing, Nothing, Nothing, Nothing)
          DQ.RentalDetails rentalSlab -> (DFFP.RENTAL, Nothing, Just $ getId rentalSlab.id, Nothing, Nothing, Nothing)
          DQ.DriverOfferDetails driverOffer -> (DFFP.DRIVER_OFFER, Nothing, Nothing, Just $ getId driverOffer.id, Nothing, Nothing)
          DQ.OneWaySpecialZoneDetails specialZoneQuote -> (DFFP.ONE_WAY_SPECIAL_ZONE, Nothing, Nothing, Nothing, Just $ getId specialZoneQuote.id, Nothing)
          DQ.PublicTransportQuoteDetails publicTransportQuote -> (DFFP.PUBLIC_TRANSPORT, Nothing, Nothing, Nothing, Nothing, Just $ getId publicTransportQuote.id)
     in BeamQ.QuoteT
          { BeamQ.id = getId id,
            BeamQ.fareProductType = fareProductType,
            BeamQ.requestId = getId requestId,
            BeamQ.estimatedFare = realToFrac estimatedFare,
            BeamQ.discount = realToFrac <$> discount,
            BeamQ.estimatedTotalFare = realToFrac estimatedTotalFare,
            BeamQ.providerId = providerId,
            BeamQ.itemId = itemId,
            BeamQ.providerUrl = showBaseUrl providerUrl,
            BeamQ.providerName = providerName,
            BeamQ.providerMobileNumber = providerMobileNumber,
            BeamQ.providerCompletedRidesCount = providerCompletedRidesCount,
            BeamQ.distanceToNearestDriver = distanceToNearestDriver,
            BeamQ.vehicleVariant = vehicleVariant,
            BeamQ.tripTermsId = getId <$> (tripTerms <&> (.id)),
            BeamQ.rentalSlabId = rentalSlabId,
            BeamQ.driverOfferId = driverOfferId,
            BeamQ.merchantId = getId merchantId,
            BeamQ.specialZoneQuoteId = specialZoneQuoteId,
            BeamQ.publicTransportQuoteId = publicTransportQuoteId,
            BeamQ.specialLocationTag = specialLocationTag,
            BeamQ.createdAt = createdAt
          }
