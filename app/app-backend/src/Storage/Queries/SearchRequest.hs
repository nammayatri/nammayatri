module Storage.Queries.SearchRequest where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Person (Person)
import Domain.Types.SearchRequest
import Storage.Tabular.SearchRequest

create :: SearchRequest -> SqlDB ()
create = create'

findById :: Transactionable m => Id SearchRequest -> m (Maybe SearchRequest)
findById = Esq.findById

findByPersonId :: Transactionable m => Id Person -> Id SearchRequest -> m (Maybe SearchRequest)
findByPersonId personId searchRequestId =
  Esq.findOne $ do
    searchRequest <- from $ table @SearchRequestT
    where_ $
      searchRequest ^. SearchRequestRiderId ==. val (toKey personId)
        &&. searchRequest ^. SearchRequestId ==. val (getId searchRequestId)
    return searchRequest

findAllByPerson :: Transactionable m => Id Person -> m [SearchRequest]
findAllByPerson perId =
  Esq.findAll $ do
    searchRequest <- from $ table @SearchRequestT
    where_ $
      searchRequest ^. SearchRequestRiderId ==. val (toKey perId)
    return searchRequest
