{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Quote where

import Domain.Types.DriverOffer as DDO
import Domain.Types.Estimate
import Domain.Types.FarePolicy.FareProductType as DFFP
import Domain.Types.Quote as DQ
import Domain.Types.SearchRequest
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Error
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOffer as BeamDO
import qualified Storage.Beam.Quote as BeamQ
import qualified Storage.Queries.DriverOffer as QueryDO
import Storage.Queries.RentalSlab as QueryRS
import Storage.Queries.SpecialZoneQuote as QuerySZQ
import qualified Storage.Queries.TripTerms as QTT

-- createDetails :: QuoteDetailsT -> FullEntitySqlDB ()
-- createDetails = \case
--   OneWayDetailsT -> pure ()
--   RentalDetailsT rentalSlabT -> do
--     Esq.create' rentalSlabT
--   DriverOfferDetailsT driverOfferT -> do
--     Esq.create' driverOfferT
--   OneWaySpecialZoneDetailsT specialZoneQuoteT -> do
--     Esq.create' specialZoneQuoteT

createDetails :: (L.MonadFlow m, Log m) => QuoteDetails -> m ()
createDetails = \case
  OneWayDetails _ -> pure ()
  RentalDetails rentalSlab -> QueryRS.createRentalSlab rentalSlab
  DriverOfferDetails driverOffer -> QueryDO.createDriverOffer driverOffer
  OneWaySpecialZoneDetails specialZoneQuote -> QuerySZQ.createSpecialZoneQuote specialZoneQuote

-- order of creating entites make sense!
-- create :: Quote -> SqlDB ()
-- create quote =
--   Esq.withFullEntity quote $ \(quoteT, mbTripTermsT, quoteDetailsT) -> do
--     traverse_ Esq.create' mbTripTermsT
--     createDetails quoteDetailsT
--     Esq.create' quoteT

createQuote :: (L.MonadFlow m, Log m) => Quote -> m ()
createQuote = createWithKV

create :: (L.MonadFlow m, Log m) => Quote -> m ()
create quote = do
  traverse_ QTT.createTripTerms (quote.tripTerms)
  _ <- createDetails (quote.quoteDetails)
  createQuote quote

-- createMany :: [Quote] -> SqlDB ()
-- createMany quotes =
--   Esq.withFullEntities quotes $ \list -> do
--     let quoteTs = map fst3 list
--         mbTripTermsTs = mapMaybe snd3 list
--         quoteDetailsTs = map thd3 list
--     Esq.createMany' mbTripTermsTs
--     traverse_ createDetails quoteDetailsTs
--     Esq.createMany' quoteTs

createMany :: (L.MonadFlow m, Log m) => [Quote] -> m ()
createMany = traverse_ create

-- fullQuoteTable ::
--   From
--     ( Table QuoteT
--         :& MbTable TripTermsT
--         :& MbTable RentalSlabT
--         :& MbTable DriverOffer.DriverOfferT
--         :& MbTable SpecialZoneQuoteT
--     )
-- fullQuoteTable =
--   table @QuoteT
--     `leftJoin` table @TripTermsT
--       `Esq.on` ( \(quote :& mbTripTerms) ->
--                    quote ^. QuoteTripTermsId ==. mbTripTerms ?. TripTermsTId
--                )
--     `leftJoin` table @RentalSlabT
--       `Esq.on` ( \(quote :& _ :& mbRentalSlab) ->
--                    quote ^. QuoteRentalSlabId ==. mbRentalSlab ?. RentalSlabTId
--                )
--     `leftJoin` table @DriverOfferT
--       `Esq.on` ( \(quote :& _ :& _ :& mbDriverOffer) ->
--                    quote ^. QuoteDriverOfferId ==. mbDriverOffer ?. DriverOfferTId
--                )
--     `leftJoin` table @SpecialZoneQuoteT
--       `Esq.on` ( \(quote :& _ :& _ :& _ :& mbspecialZoneQuote) ->
--                    quote ^. QuoteSpecialZoneQuoteId ==. mbspecialZoneQuote ?. SpecialZoneQuoteTId
--                )

-- findById :: Transactionable m => Id Quote -> m (Maybe Quote)
-- findById quoteId = Esq.buildDType $ do
--   mbFullQuoteT <- Esq.findOne' $ do
--     (quote :& mbTripTerms :& mbRentalSlab :& mbDriverOffer :& mbspecialZoneQuote) <- from fullQuoteTable
--     where_ $ quote ^. QuoteTId ==. val (toKey quoteId)
--     pure (quote, mbTripTerms, mbRentalSlab, mbDriverOffer, mbspecialZoneQuote)
--   join <$> mapM buildFullQuote mbFullQuoteT

findById :: (L.MonadFlow m, Log m) => Id Quote -> m (Maybe Quote)
findById quoteId = findOneWithKV [Se.Is BeamQ.id $ Se.Eq (getId quoteId)]

findByIdInReplica :: (L.MonadFlow m, Log m) => Id Quote -> m (Maybe Quote)
findByIdInReplica quoteId = findOneWithKvInReplica [Se.Is BeamQ.id $ Se.Eq (getId quoteId)]

-- findByBppIdAndBPPQuoteId :: Transactionable m => Text -> Id BPPQuote -> m (Maybe Quote)
-- findByBppIdAndBPPQuoteId bppId bppQuoteId = buildDType $ do
--   mbFullQuoteT <- Esq.findOne' $ do
--     (quote :& mbTripTerms :& mbRentalSlab :& mbDriverOffer :& mbspecialZoneQuote) <- from fullQuoteTable
--     where_ $
--       quote ^. QuoteProviderId ==. val bppId
--         &&. mbDriverOffer ?. DriverOfferBppQuoteId ==. just (val bppQuoteId.getId)
--     pure (quote, mbTripTerms, mbRentalSlab, mbDriverOffer, mbspecialZoneQuote)
--   join <$> mapM buildFullQuote mbFullQuoteT

findByBppIdAndBPPQuoteId :: (L.MonadFlow m, Log m) => Text -> Id BPPQuote -> m (Maybe Quote)
findByBppIdAndBPPQuoteId bppId bppQuoteId = do
  quoteList <- findAllWithKV [Se.Is BeamQ.providerId $ Se.Eq bppId]
  dOffer <- QueryDO.findByBPPQuoteId bppQuoteId
  let quoteWithDoOfferId = foldl' (getQuoteWithDOffer dOffer) [] quoteList
  pure $ listToMaybe quoteWithDoOfferId
  where
    getQuoteWithDOffer dOffer res quote = do
      let doId = case quote.quoteDetails of
            DQ.DriverOfferDetails driverOffer -> Just $ getId driverOffer.id
            _ -> Nothing
      let doffer' = filter (\d -> getId (d.id) == fromJust doId) dOffer
       in res <> (quote <$ doffer')

findByBppIdAndBPPQuoteIdInReplica :: (L.MonadFlow m, Log m) => Text -> Id BPPQuote -> m (Maybe Quote)
findByBppIdAndBPPQuoteIdInReplica bppId bppQuoteId = do
  quoteList <- findAllWithKvInReplica [Se.Is BeamQ.providerId $ Se.Eq bppId]
  dOffer <- QueryDO.findByBPPQuoteIdInReplica bppQuoteId
  let quoteWithDoOfferId = foldl' (getQuoteWithDOffer dOffer) [] quoteList
  pure $ listToMaybe quoteWithDoOfferId
  where
    getQuoteWithDOffer dOffer res quote = do
      let doId = case quote.quoteDetails of
            DQ.DriverOfferDetails driverOffer -> Just $ getId driverOffer.id
            _ -> Nothing
      let doffer' = filter (\d -> getId (d.id) == fromJust doId) dOffer
       in res <> (quote <$ doffer')

-- lets check this query as bppquoteId is needed to find driver offer in domain

-- findByBppIdAndBPPQuoteId' :: (L.MonadFlow m, Log m) => Text -> Id BPPQuote -> m (Maybe Quote)
-- findByBppIdAndBPPQuoteId' bppId bppQuoteId = do
--   dbConf <- L.getOption KBT.PsqlDbCfg
--   let modelName = Se.modelTableName @BeamQ.QuoteT
--   updatedMeshConfig <- setMeshConfig modelName
--   case dbConf of
--     Just dbConf' -> do
--       result <- KV.findWithKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamQ.providerId $ Se.Eq bppId]]
--       case result of
--         Right (Just q) -> transformBeamQuoteToDomain q
--         _ -> pure Nothing
--     Nothing -> pure Nothing

-- findAllBySRId :: Transactionable m => Id SearchRequest -> m [Quote]
-- findAllBySRId searchRequestId = Esq.buildDType $ do
--   fullQuoteTs <- Esq.findAll' $ do
--     (quote :& mbTripTerms :& mbRentalSlab :& mbDriverOffer :& mbspecialZoneQuote) <- from fullQuoteTable
--     where_ $ quote ^. QuoteRequestId ==. val (toKey searchRequestId)
--     pure (quote, mbTripTerms, mbRentalSlab, mbDriverOffer, mbspecialZoneQuote)
--   catMaybes <$> mapM buildFullQuote fullQuoteTs

findAllBySRId :: (L.MonadFlow m, Log m) => Id SearchRequest -> m [Quote]
findAllBySRId searchRequestId = findAllWithKV [Se.Is BeamQ.requestId $ Se.Eq (getId searchRequestId)]

findAllBySRIdInReplica :: (L.MonadFlow m, Log m) => Id SearchRequest -> m [Quote]
findAllBySRIdInReplica searchRequestId = findAllWithKvInReplica [Se.Is BeamQ.requestId $ Se.Eq (getId searchRequestId)]

-- findAllByEstimateId :: Transactionable m => Id Estimate -> m [Quote]
-- findAllByEstimateId estimateId = buildDType $ do
--   driverOfferTs <- findDOfferByEstimateId' estimateId
--   (catMaybes <$>) $ mapM buildFullQuote' driverOfferTs
--   where
--     buildFullQuote' :: Transactionable m => DriverOfferT -> DTypeBuilder m (Maybe (SolidType FullQuoteT))
--     buildFullQuote' driverOfferT = runMaybeT $ do
--       quoteT <- MaybeT $ findQuotesByDriverOfferId' (Id $ DriverOffer.id driverOfferT)
--       quoteDetailsT <- case fareProductType quoteT of
--         ONE_WAY -> pure OneWayDetailsT
--         RENTAL -> do
--           rentalSlabId <- MaybeT $ pure (fromKey <$> rentalSlabId quoteT)
--           rentalSlab <- MaybeT $ Esq.findById' @RentalSlabT rentalSlabId
--           pure $ RentalDetailsT rentalSlab
--         DRIVER_OFFER -> do
--           pure (DriverOfferDetailsT driverOfferT)
--         ONE_WAY_SPECIAL_ZONE -> do
--           specialZoneQuoteId <- MaybeT $ pure (fromKey <$> specialZoneQuoteId quoteT)
--           specialZoneQuoteT <- MaybeT $ Esq.findById' @SpecialZoneQuoteT specialZoneQuoteId
--           pure (OneWaySpecialZoneDetailsT specialZoneQuoteT)
--       mbTripTermsT <- forM (fromKey <$> tripTermsId quoteT) $ \tripTermsId -> do
--         MaybeT $ Esq.findById' @TripTermsT tripTermsId
--       return $ extractSolidType @Quote (quoteT, mbTripTermsT, quoteDetailsT)

findAllByEstimateId :: (L.MonadFlow m, Log m) => Id Estimate -> DriverOfferStatus -> m [Quote]
findAllByEstimateId estimateId status = do
  driverOffers <- findDOfferByEstimateId estimateId status
  let offerIds = map (Just . getId . DDO.id) driverOffers
  findAllWithKV [Se.Is BeamQ.driverOfferId (Se.In offerIds)]

findAllByEstimateIdInReplica :: (L.MonadFlow m, Log m) => Id Estimate -> DriverOfferStatus -> m [Quote]
findAllByEstimateIdInReplica estimateId status = do
  driverOffers <- findDOfferByEstimateIdInReplica estimateId status
  let offerIds = map (Just . getId . DDO.id) driverOffers
  findAllWithKvInReplica [Se.Is BeamQ.driverOfferId (Se.In offerIds)]

-- findDOfferByEstimateId' :: Transactionable m => Id Estimate -> DTypeBuilder m [DriverOfferT]
-- findDOfferByEstimateId' estimateId =
--   Esq.findAll' $ do
--     driverOffer <- from $ table @DriverOfferT
--     where_ $ driverOffer ^. DriverOfferEstimateId ==. val (toKey estimateId)
--     return driverOffer

findDOfferByEstimateId :: (L.MonadFlow m, Log m) => Id Estimate -> DriverOfferStatus -> m [DriverOffer]
findDOfferByEstimateId (Id estimateId) status = findAllWithKV [Se.And [Se.Is BeamDO.estimateId $ Se.Eq estimateId, Se.Is BeamDO.status $ Se.Eq status]]

findDOfferByEstimateIdInReplica :: (L.MonadFlow m, Log m) => Id Estimate -> DriverOfferStatus -> m [DriverOffer]
findDOfferByEstimateIdInReplica (Id estimateId) status = findAllWithKvInReplica [Se.And [Se.Is BeamDO.estimateId $ Se.Eq estimateId, Se.Is BeamDO.status $ Se.Eq status]]

-- findQuotesByDriverOfferId' :: Transactionable m => Id DriverOffer -> DTypeBuilder m (Maybe QuoteT)
-- findQuotesByDriverOfferId' driverOfferId = Esq.findOne' $ do
--   quote <- from $ table @QuoteT
--   where_ $
--     quote ^. QuoteDriverOfferId ==. just (val (toKey driverOfferId))
--   return quote

-- need to complete this transformation
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

instance ToTType' BeamQ.Quote Quote where
  toTType' Quote {..} =
    let (fareProductType, distanceToNearestDriver, rentalSlabId, driverOfferId, specialZoneQuoteId) = case quoteDetails of
          DQ.OneWayDetails details -> (DFFP.ONE_WAY, Just $ details.distanceToNearestDriver, Nothing, Nothing, Nothing)
          DQ.RentalDetails rentalSlab -> (DFFP.RENTAL, Nothing, Just $ getId rentalSlab.id, Nothing, Nothing)
          DQ.DriverOfferDetails driverOffer -> (DFFP.DRIVER_OFFER, Nothing, Nothing, Just $ getId driverOffer.id, Nothing)
          DQ.OneWaySpecialZoneDetails specialZoneQuote -> (DFFP.ONE_WAY_SPECIAL_ZONE, Nothing, Nothing, Nothing, Just $ getId specialZoneQuote.id)
     in BeamQ.QuoteT
          { BeamQ.id = getId id,
            BeamQ.fareProductType = fareProductType,
            BeamQ.requestId = getId requestId,
            BeamQ.estimatedFare = realToFrac estimatedFare,
            BeamQ.discount = realToFrac <$> discount,
            BeamQ.estimatedTotalFare = realToFrac estimatedTotalFare,
            BeamQ.providerId = providerId,
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
            BeamQ.specialLocationTag = specialLocationTag,
            BeamQ.createdAt = createdAt
          }
