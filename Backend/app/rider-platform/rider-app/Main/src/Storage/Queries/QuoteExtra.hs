{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.QuoteExtra where

import Domain.Types.DriverOffer as DDO
import Domain.Types.Estimate
import Domain.Types.FarePolicy.FareProductType as DFFP
import Domain.Types.Quote as DQ
import Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOffer as BeamDO
import qualified Storage.Beam.Quote as BeamQ
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.DriverOffer as QueryDO
import Storage.Queries.OrphanInstances.Quote
import Storage.Queries.RentalDetails as QueryRD
import Storage.Queries.SpecialZoneQuote as QuerySZQ
import qualified Storage.Queries.TripTerms as QTT

-- Extra code goes here --
createQuote :: KvDbFlow m r => Quote -> m ()
createQuote = createWithKV

createDetails :: KvDbFlow m r => QuoteDetails -> m ()
createDetails = \case
  OneWayDetails _ -> pure ()
  RentalDetails rentalDetails -> QueryRD.createRentalDetails rentalDetails
  DriverOfferDetails driverOffer -> QueryDO.create driverOffer
  OneWaySpecialZoneDetails specialZoneQuote -> QuerySZQ.create specialZoneQuote
  InterCityDetails specialZoneQuote -> QuerySZQ.create specialZoneQuote

createQuote' :: KvDbFlow m r => Quote -> m ()
createQuote' quote = do
  traverse_ QTT.create (quote.tripTerms)
  _ <- createDetails (quote.quoteDetails)
  createQuote quote

createMany :: KvDbFlow m r => [Quote] -> m ()
createMany = traverse_ createQuote'

findByBppIdAndBPPQuoteId :: KvDbFlow m r => Text -> Text -> m (Maybe Quote)
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

findAllByEstimateId :: KvDbFlow m r => Id Estimate -> DriverOfferStatus -> m [Quote]
findAllByEstimateId estimateId status = do
  driverOffers <- findDOfferByEstimateId estimateId status
  let offerIds = map (Just . getId . DDO.id) driverOffers
  findAllWithKV [Se.Is BeamQ.driverOfferId (Se.In offerIds)]

findDOfferByEstimateId :: KvDbFlow m r => Id Estimate -> DriverOfferStatus -> m [DriverOffer]
findDOfferByEstimateId (Id estimateId) status = findAllWithKV [Se.And [Se.Is BeamDO.estimateId $ Se.Eq estimateId, Se.Is BeamDO.status $ Se.Eq status]]
