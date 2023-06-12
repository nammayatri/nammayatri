{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Quote where

import Data.Tuple.Extra
import Domain.Types.DriverOffer
import Domain.Types.Estimate
import Domain.Types.FarePolicy.FareProductType as DFFP
import Domain.Types.Quote as DQ
import Domain.Types.SearchRequest
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Error
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.Quote as BeamQ
import Storage.Queries.FullEntityBuilders (buildFullQuote)
import Storage.Queries.RentalSlab as QueryRS
import Storage.Queries.SpecialZoneQuote as QuerySZQ
import qualified Storage.Queries.TripTerms as QTT
import Storage.Tabular.DriverOffer as DriverOffer
import Storage.Tabular.Quote hiding (distanceToNearestDriver)
import Storage.Tabular.Quote.Instances
import Storage.Tabular.RentalSlab
import Storage.Tabular.SpecialZoneQuote
import Storage.Tabular.TripTerms

createDetails :: QuoteDetailsT -> FullEntitySqlDB ()
createDetails = \case
  OneWayDetailsT -> pure ()
  RentalDetailsT rentalSlabT -> do
    Esq.create' rentalSlabT
  DriverOfferDetailsT driverOfferT -> do
    Esq.create' driverOfferT
  OneWaySpecialZoneDetailsT specialZoneQuoteT -> do
    Esq.create' specialZoneQuoteT

-- create :: L.MonadFlow m => Quote -> m (MeshResult ())
-- create Quote = do
--   dbConf <- L.getOption KBT.PsqlDbCfg
--   let modelName = Se.modelTableName @BeamR.RideT
--   let updatedMeshConfig = setMeshConfig modelName
--   case dbConf of
--     Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainRideToBeam Quote)
--     Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- order of creating entites make sense!
create :: Quote -> SqlDB ()
create quote =
  Esq.withFullEntity quote $ \(quoteT, mbTripTermsT, quoteDetailsT) -> do
    traverse_ Esq.create' mbTripTermsT
    createDetails quoteDetailsT
    Esq.create' quoteT

createMany :: [Quote] -> SqlDB ()
createMany quotes =
  Esq.withFullEntities quotes $ \list -> do
    let quoteTs = map fst3 list
        mbTripTermsTs = mapMaybe snd3 list
        quoteDetailsTs = map thd3 list
    Esq.createMany' mbTripTermsTs
    traverse_ createDetails quoteDetailsTs
    Esq.createMany' quoteTs

createMany' :: L.MonadFlow m => [Quote] -> m (MeshResult ())
createMany' = error "createMany' not implemented make sure to implement and change the ' where its being used"

fullQuoteTable ::
  From
    ( Table QuoteT
        :& MbTable TripTermsT
        :& MbTable RentalSlabT
        :& MbTable DriverOffer.DriverOfferT
        :& MbTable SpecialZoneQuoteT
    )
fullQuoteTable =
  table @QuoteT
    `leftJoin` table @TripTermsT
      `Esq.on` ( \(quote :& mbTripTerms) ->
                   quote ^. QuoteTripTermsId ==. mbTripTerms ?. TripTermsTId
               )
    `leftJoin` table @RentalSlabT
      `Esq.on` ( \(quote :& _ :& mbRentalSlab) ->
                   quote ^. QuoteRentalSlabId ==. mbRentalSlab ?. RentalSlabTId
               )
    `leftJoin` table @DriverOfferT
      `Esq.on` ( \(quote :& _ :& _ :& mbDriverOffer) ->
                   quote ^. QuoteDriverOfferId ==. mbDriverOffer ?. DriverOfferTId
               )
    `leftJoin` table @SpecialZoneQuoteT
      `Esq.on` ( \(quote :& _ :& _ :& _ :& mbspecialZoneQuote) ->
                   quote ^. QuoteSpecialZoneQuoteId ==. mbspecialZoneQuote ?. SpecialZoneQuoteTId
               )

-- findByMobileNumberAndMerchantId' :: (L.MonadFlow m, Log m) => Text -> DbHash -> Id Merchant -> m (Maybe Person)
-- findByMobileNumberAndMerchantId' countryCode mobileNumberHash (Id merchantId) = do
--   dbConf <- L.getOption KBT.PsqlDbCfg
--   let modelName = Se.modelTableName @BeamP.PersonT
--   let updatedMeshConfig = setMeshConfig modelName
--   case dbConf of
--     Just dbConf' -> do
--       result <- KV.findWithKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamP.mobileCountryCode $ Se.Eq (Just countryCode), Se.Is BeamP.mobileNumberHash $ Se.Eq (Just mobileNumberHash), Se.Is BeamP.merchantId $ Se.Eq merchantId]]
--       case result of
--         Right (Just p) -> transformBeamPersonToDomain p
--         _ -> pure Nothing
--     Nothing -> pure Nothing
findById :: Transactionable m => Id Quote -> m (Maybe Quote)
findById quoteId = Esq.buildDType $ do
  mbFullQuoteT <- Esq.findOne' $ do
    (quote :& mbTripTerms :& mbRentalSlab :& mbDriverOffer :& mbspecialZoneQuote) <- from fullQuoteTable
    where_ $ quote ^. QuoteTId ==. val (toKey quoteId)
    pure (quote, mbTripTerms, mbRentalSlab, mbDriverOffer, mbspecialZoneQuote)
  join <$> mapM buildFullQuote mbFullQuoteT

findById' :: (L.MonadFlow m, Log m) => Id Quote -> m (Maybe Quote)
findById' quoteId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamQ.QuoteT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamQ.id $ Se.Eq (getId quoteId)]
      case result of
        Right (Just q) -> transformBeamQuoteToDomain q
        _ -> pure Nothing
    Nothing -> pure Nothing

findByBppIdAndBPPQuoteId :: Transactionable m => Text -> Id BPPQuote -> m (Maybe Quote)
findByBppIdAndBPPQuoteId bppId bppQuoteId = buildDType $ do
  mbFullQuoteT <- Esq.findOne' $ do
    (quote :& mbTripTerms :& mbRentalSlab :& mbDriverOffer :& mbspecialZoneQuote) <- from fullQuoteTable
    where_ $
      quote ^. QuoteProviderId ==. val bppId
        &&. mbDriverOffer ?. DriverOfferBppQuoteId ==. just (val bppQuoteId.getId)
    pure (quote, mbTripTerms, mbRentalSlab, mbDriverOffer, mbspecialZoneQuote)
  join <$> mapM buildFullQuote mbFullQuoteT

-- lets check this query as bppquoteId is needed to find driver offer in domain

-- findByBppIdAndBPPQuoteId' :: (L.MonadFlow m, Log m) => Text -> Id BPPQuote -> m (Maybe Quote)
-- findByBppIdAndBPPQuoteId' bppId bppQuoteId = do
--   dbConf <- L.getOption KBT.PsqlDbCfg
--   let modelName = Se.modelTableName @BeamQ.QuoteT
--   let updatedMeshConfig = setMeshConfig modelName
--   case dbConf of
--     Just dbConf' -> do
--       result <- KV.findWithKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamQ.providerId $ Se.Eq bppId]]
--       case result of
--         Right (Just q) -> transformBeamQuoteToDomain q
--         _ -> pure Nothing
--     Nothing -> pure Nothing

findAllBySRId :: Transactionable m => Id SearchRequest -> m [Quote]
findAllBySRId searchRequestId = Esq.buildDType $ do
  fullQuoteTs <- Esq.findAll' $ do
    (quote :& mbTripTerms :& mbRentalSlab :& mbDriverOffer :& mbspecialZoneQuote) <- from fullQuoteTable
    where_ $ quote ^. QuoteRequestId ==. val (toKey searchRequestId)
    pure (quote, mbTripTerms, mbRentalSlab, mbDriverOffer, mbspecialZoneQuote)
  catMaybes <$> mapM buildFullQuote fullQuoteTs

findAllBySRId' :: (L.MonadFlow m, Log m) => Id SearchRequest -> m [Quote]
findAllBySRId' searchRequestId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamQ.QuoteT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <- KV.findAllWithKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamQ.requestId $ Se.Eq (getId searchRequestId)]]
      case result of
        Right res -> catMaybes <$> traverse transformBeamQuoteToDomain res
        _ -> pure []
    Nothing -> pure []

findAllByEstimateId :: Transactionable m => Id Estimate -> m [Quote]
findAllByEstimateId estimateId = buildDType $ do
  driverOfferTs <- findDOfferByEstimateId' estimateId
  (catMaybes <$>) $ mapM buildFullQuote' driverOfferTs
  where
    buildFullQuote' :: Transactionable m => DriverOfferT -> DTypeBuilder m (Maybe (SolidType FullQuoteT))
    buildFullQuote' driverOfferT = runMaybeT $ do
      quoteT <- MaybeT $ findQuotesByDriverOfferId' (Id $ DriverOffer.id driverOfferT)
      quoteDetailsT <- case fareProductType quoteT of
        ONE_WAY -> pure OneWayDetailsT
        RENTAL -> do
          rentalSlabId <- hoistMaybe (rentalSlabId quoteT)
          rentalSlab <- MaybeT $ Esq.findById' @RentalSlabT rentalSlabId
          pure $ RentalDetailsT rentalSlab
        DRIVER_OFFER -> do
          pure (DriverOfferDetailsT driverOfferT)
        ONE_WAY_SPECIAL_ZONE -> do
          specialZoneQuoteId <- hoistMaybe (specialZoneQuoteId quoteT)
          specialZoneQuoteT <- MaybeT $ Esq.findById' @SpecialZoneQuoteT specialZoneQuoteId
          pure (OneWaySpecialZoneDetailsT specialZoneQuoteT)
      mbTripTermsT <- forM (tripTermsId quoteT) $ \tripTermsId -> do
        MaybeT $ Esq.findById' @TripTermsT tripTermsId
      return $ extractSolidType @Quote (quoteT, mbTripTermsT, quoteDetailsT)

findDOfferByEstimateId' :: Transactionable m => Id Estimate -> DTypeBuilder m [DriverOfferT]
findDOfferByEstimateId' estimateId =
  Esq.findAll' $ do
    driverOffer <- from $ table @DriverOfferT
    where_ $ driverOffer ^. DriverOfferEstimateId ==. val (toKey estimateId)
    return driverOffer

-- findDOfferByEstimateId'' :: (L.MonadFlow m, Log m) => Id Estimate -> m [DriverOffer]
-- findDOfferByEstimateId'' estimateId = do
--   dbConf <- L.getOption KBT.PsqlDbCfg
--   let modelName = Se.modelTableName @BeamDO.DriverOfferT
--   let updatedMeshConfig = setMeshConfig modelName
--   case dbConf of
--     Just dbConf' -> do
--       result <- KV.findAllWithKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamDO.estimateId $ Se.Eq (getId estimateId)]]
--       case result of
--         Right res -> catMaybes <$> traverse transformBeamDriverOfferToDomain res
--         _ -> pure []
--     Nothing -> pure []

findQuotesByDriverOfferId' :: Transactionable m => Id DriverOffer -> DTypeBuilder m (Maybe QuoteT)
findQuotesByDriverOfferId' driverOfferId = Esq.findOne' $ do
  quote <- from $ table @QuoteT
  where_ $
    quote ^. QuoteDriverOfferId ==. just (val (toKey driverOfferId))
  return quote

-- need to complete this transformation
transformBeamQuoteToDomain :: (L.MonadFlow m, Log m) => BeamQ.Quote -> m (Maybe Quote)
transformBeamQuoteToDomain BeamQ.QuoteT {..} = do
  trip <- if isJust tripTermsId then QTT.findById'' (Id (fromJust tripTermsId)) else pure Nothing
  pUrl <- parseBaseUrl providerUrl
  rentalDetails <- do
    res <- QueryRS.findById (Id (fromJust rentalSlabId))
    case res of
      Just rentalSlab -> pure $ Just $ DQ.RentalDetails rentalSlab
      Nothing -> pure Nothing
  specialZoneQuote <- do
    res <- QuerySZQ.findById (Id (fromJust specialZoneQuoteId))
    case res of
      Just specialZoneQuote -> pure $ Just $ DQ.OneWaySpecialZoneDetails specialZoneQuote
      Nothing -> pure Nothing

  quoteDetails <- case fareProductType of
    DFFP.ONE_WAY -> do
      distanceToNearestDriver' <- distanceToNearestDriver & fromMaybeM (QuoteFieldNotPresent "distanceToNearestDriver")
      pure . DQ.OneWayDetails $
        DQ.OneWayQuoteDetails
          { distanceToNearestDriver = distanceToNearestDriver'
          }
    DFFP.RENTAL -> pure (fromJust rentalDetails)
    DFFP.DRIVER_OFFER -> DQ.DriverOfferDetails <$> error "" -- findById' driverOfferId
    DFFP.ONE_WAY_SPECIAL_ZONE -> pure (fromJust specialZoneQuote)
  if isJust trip
    then
      pure $
        Just
          Quote
            { id = Id id,
              requestId = Id requestId,
              estimatedFare = roundToIntegral estimatedFare,
              discount = roundToIntegral <$> discount,
              estimatedTotalFare = roundToIntegral estimatedTotalFare,
              providerId = providerId,
              providerUrl = pUrl,
              providerName = providerName,
              providerMobileNumber = providerMobileNumber,
              providerCompletedRidesCount = providerCompletedRidesCount,
              vehicleVariant = vehicleVariant,
              tripTerms = trip,
              quoteDetails = quoteDetails,
              merchantId = Id merchantId,
              specialLocationTag = specialLocationTag,
              createdAt = createdAt
            }
    else pure Nothing

transformDomainQuoteToBeam :: Quote -> BeamQ.Quote
transformDomainQuoteToBeam Quote {..} =
  let (fareProductType, distanceToNearestDriver, rentalSlabId, driverOfferId, specialZoneQuoteId) = case quoteDetails of
        DQ.OneWayDetails details -> (DFFP.ONE_WAY, Just $ details.distanceToNearestDriver, Nothing, Nothing, Nothing)
        DQ.RentalDetails rentalSlab -> (DFFP.RENTAL, Nothing, Just $ getId rentalSlab.id, Nothing, Nothing)
        DQ.DriverOfferDetails driverOffer -> (DFFP.DRIVER_OFFER, Nothing, Nothing, Just $ getId driverOffer.id, Nothing)
        DQ.OneWaySpecialZoneDetails specialZoneQuote -> (DFFP.ONE_WAY_SPECIAL_ZONE, Nothing, Nothing, Nothing, Just $ getId specialZoneQuote.id)
   in BeamQ.defaultQuote
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
