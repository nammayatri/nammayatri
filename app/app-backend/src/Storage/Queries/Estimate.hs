{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Estimate where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Estimate
import Domain.Types.SearchRequest
import Storage.Tabular.Estimate
import Storage.Tabular.TripTerms

-- order of creating entites make sense!
create :: Estimate -> SqlDB ()
create estimate =
  Esq.withFullEntity estimate $ \(estimateT, mbTripTermsT) -> do
    traverse_ Esq.create' mbTripTermsT
    Esq.create' estimateT

createMany :: [Estimate] -> SqlDB ()
createMany estimates =
  Esq.withFullEntities estimates $ \list -> do
    let estimateTs = map fst list
        tripTermsTs = mapMaybe snd list
    Esq.createMany' tripTermsTs
    Esq.createMany' estimateTs

fullEstimateTable ::
  From
    ( Table EstimateT
        :& Esq.MbTable TripTermsT
    )
fullEstimateTable =
  table @EstimateT
    `leftJoin` table @TripTermsT
      `Esq.on` ( \(estimate :& mbTripTerms) ->
                   estimate ^. EstimateTripTermsId ==. mbTripTerms ?. TripTermsTId
               )

findById :: Transactionable m => Id Estimate -> m (Maybe Estimate)
findById estimateId = Esq.buildDType $ do
  mbFullEstimateT <- Esq.findOne' $ do
    estimate <- from $ table @EstimateT
    where_ $ estimate ^. EstimateTId ==. val (toKey estimateId)
    mbTripTerms <- toMaybe <$> from (table @TripTermsT)
    where_ $ mbTripTerms ?. TripTermsTId ==. estimate ^. EstimateTripTermsId
    pure (estimate, mbTripTerms)
  pure $ extractSolidType <$> mbFullEstimateT

findAllByRequestId :: Transactionable m => Id SearchRequest -> m [Estimate]
findAllByRequestId searchRequestId = Esq.buildDType $ do
  fullEstimateTs <- Esq.findAll' $ do
    estimate <- from $ table @EstimateT
    where_ $ estimate ^. EstimateRequestId ==. val (toKey searchRequestId)
    mbTripTerms <- toMaybe <$> from (table @TripTermsT)
    where_ $ mbTripTerms ?. TripTermsTId ==. estimate ^. EstimateTripTermsId
    pure (estimate, mbTripTerms)
  pure $ extractSolidType <$> fullEstimateTs
