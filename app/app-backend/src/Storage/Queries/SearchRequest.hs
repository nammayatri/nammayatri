module Storage.Queries.SearchRequest where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Person (Person)
import Domain.Types.SearchRequest
import Storage.Tabular.SearchRequest

create :: SearchRequest -> SqlDB ()
create = create'

findAllByPersonIdLimitOffset ::
  Transactionable m =>
  Id Person ->
  Maybe Integer ->
  Maybe Integer ->
  m [SearchRequest]
findAllByPersonIdLimitOffset personId mlimit moffset =
  Esq.findAll $ do
    searchRequest <- from $ table @SearchRequestT
    where_ $
      searchRequest ^. SearchRequestRiderId ==. val (toKey personId)
    limit $ fromIntegral $ fromMaybe 100 mlimit
    offset $ fromIntegral $ fromMaybe 0 moffset
    orderBy [desc $ searchRequest ^. SearchRequestCreatedAt]
    return searchRequest

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
