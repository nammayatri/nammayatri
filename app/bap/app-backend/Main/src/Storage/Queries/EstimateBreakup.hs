{-# LANGUAGE TypeApplications #-}

module Storage.Queries.EstimateBreakup where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Estimate
import Storage.Tabular.EstimateBreakup as SEB

findAllByEstimateId :: (Transactionable m) => Id Estimate -> DTypeBuilder m [EstimateBreakupT]
findAllByEstimateId estimateId =
  Esq.findAll' $ do
    estimateBreakup <- from $ table @SEB.EstimateBreakupT
    where_ $ estimateBreakup ^. EstimateBreakupEstimateId ==. val (toKey estimateId)
    return estimateBreakup
