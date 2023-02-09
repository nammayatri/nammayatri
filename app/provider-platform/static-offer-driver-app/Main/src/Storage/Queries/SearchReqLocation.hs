{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Storage.Queries.SearchReqLocation where

import Domain.Types.SearchRequest.SearchReqLocation
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.SearchRequest.SearchReqLocation ()

create :: SearchReqLocation -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id SearchReqLocation ->
  m (Maybe SearchReqLocation)
findById = Esq.findById
