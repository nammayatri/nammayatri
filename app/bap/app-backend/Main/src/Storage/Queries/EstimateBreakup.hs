{-# LANGUAGE TypeApplications #-}

module Storage.Queries.EstimateBreakup where

import Domain.Types.Estimate
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.EstimateBreakup as SEB

findAllByEstimateId :: (Transactionable m) => Id Estimate -> DTypeBuilder m [EstimateBreakupT]
findAllByEstimateId estimateId =
  Esq.findAll' $ do
    estimateBreakup <- from $ table @SEB.EstimateBreakupT
    where_ $ estimateBreakup ^. EstimateBreakupEstimateId ==. val (toKey estimateId)
    return estimateBreakup
