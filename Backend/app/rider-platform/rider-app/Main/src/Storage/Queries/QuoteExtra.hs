module Storage.Queries.QuoteExtra where

import Domain.Types.DriverOffer as DDO
import Domain.Types.Estimate
import Domain.Types.Quote as DQ
import Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOffer as BeamDO
import qualified Storage.Beam.Quote as BeamQ
import qualified Storage.Queries.DriverOffer as QueryDO
import Storage.Queries.InterCityDetails as QueryICD
import qualified Storage.Queries.OneWayScheduledQuote as QScheduled
import Storage.Queries.OrphanInstances.Quote ()
import qualified Storage.Queries.QuoteBreakup as QQB
import Storage.Queries.RentalDetails as QueryRD
import Storage.Queries.SpecialZoneQuote as QuerySZQ
import qualified Storage.Queries.TripTerms as QTT

-- Extra code goes here --
createQuote :: (MonadFlow m, EsqDBFlow m r) => Quote -> m ()
createQuote = createWithKV

createDetails :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => QuoteDetails -> m ()
createDetails = \case
  OneWayDetails _ -> pure ()
  DQ.DeliveryDetails driverOffer -> QueryDO.create driverOffer
  AmbulanceDetails driverOffer -> QueryDO.create driverOffer
  RentalDetails rentalDetails -> QueryRD.create rentalDetails
  DriverOfferDetails driverOffer -> QueryDO.create driverOffer
  OneWaySpecialZoneDetails specialZoneQuote -> QuerySZQ.create specialZoneQuote
  InterCityDetails interCityDetails -> QueryICD.create interCityDetails
  OneWayScheduledDetails oneWayScheduledQuote -> QScheduled.create oneWayScheduledQuote

createQuote' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Quote -> m ()
createQuote' quote = do
  traverse_ QTT.create (quote.tripTerms)
  _ <- createDetails (quote.quoteDetails)
  QQB.createMany quote.quoteBreakupList
  createQuote quote

createMany :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Quote] -> m ()
createMany = traverse_ createQuote'

findByBppIdAndBPPQuoteId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Text -> m (Maybe Quote)
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

findAllByEstimateId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Estimate -> DriverOfferStatus -> m [Quote]
findAllByEstimateId estimateId status = do
  driverOffers <- findDOfferByEstimateId estimateId status
  let offerIds = map (Just . getId . DDO.id) driverOffers
  findAllWithKV [Se.Is BeamQ.driverOfferId (Se.In offerIds)]

findDOfferByEstimateId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Estimate -> DriverOfferStatus -> m [DriverOffer]
findDOfferByEstimateId (Id estimateId) status = findAllWithKV [Se.And [Se.Is BeamDO.estimateId $ Se.Eq estimateId, Se.Is BeamDO.status $ Se.Eq status]]

findAllQuotesBySRId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id SearchRequest -> DriverOfferStatus -> m [Quote]
findAllQuotesBySRId (Id srId) status = do
  allQuotes <- findAllWithKVAndConditionalDB [Se.Is BeamQ.requestId $ Se.Eq srId] Nothing
  return $
    mapMaybe
      ( \quote ->
          case quote.quoteDetails of
            DQ.DriverOfferDetails driverOffer
              | DDO.status driverOffer == status -> Just quote
              | otherwise -> Nothing
            _ -> Nothing
      )
      allQuotes
