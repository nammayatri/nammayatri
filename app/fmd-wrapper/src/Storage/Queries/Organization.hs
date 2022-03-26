{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Organization where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Organization
import Storage.Tabular.Organization

findOrgByShortId :: EsqDBFlow m r => ShortId Organization -> m (Maybe Organization)
findOrgByShortId (ShortId shortId) =
  runTransaction . findOne $ do
    organization <- from $ table @OrganizationT
    where_ $ organization ^. OrganizationShortId ==. val shortId
    return organization
