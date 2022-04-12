{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FarePolicy.FareProduct where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.FarePolicy.FareProduct
import Domain.Types.Organization (Organization)
import Storage.Tabular.FarePolicy.FareProduct

findEnabledByOrgId ::
  Transactionable m =>
  Id Organization ->
  m [FareProduct]
findEnabledByOrgId orgId =
  Esq.findAll $ do
    fareProduct <- from $ table @FareProductT
    where_ $
      fareProduct ^. FareProductOrganizationId ==. val (toKey orgId)
        &&. fareProduct ^. FareProductEnabled
    pure fareProduct
