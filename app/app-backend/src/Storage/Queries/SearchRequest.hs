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
  EsqDBFlow m r =>
  Id Person ->
  Maybe Integer ->
  Maybe Integer ->
  m [SearchRequest]
findAllByPersonIdLimitOffset personId mlimit moffset =
  runTransaction . findAll' $ do
    searchRequest <- from $ table @SearchRequestT
    where_ $
      searchRequest ^. SearchRequestRiderId ==. val (toKey personId)
    limit $ fromIntegral $ fromMaybe 100 mlimit
    offset $ fromIntegral $ fromMaybe 0 moffset
    orderBy [desc $ searchRequest ^. SearchRequestCreatedAt]
    return searchRequest

findById :: EsqDBFlow m r => Id SearchRequest -> m (Maybe SearchRequest)
findById = Esq.findById

findByPersonId :: EsqDBFlow m r => Id Person -> Id SearchRequest -> m (Maybe SearchRequest)
findByPersonId personId searchRequestId =
  runTransaction . findOne' $ do
    searchRequest <- from $ table @SearchRequestT
    where_ $
      searchRequest ^. SearchRequestRiderId ==. val (toKey personId)
        &&. searchRequest ^. SearchRequestId ==. val (getId searchRequestId)
    return searchRequest

findAllByPerson :: EsqDBFlow m r => Id Person -> m [SearchRequest]
findAllByPerson perId =
  runTransaction . findAll' $ do
    searchRequest <- from $ table @SearchRequestT
    where_ $
      searchRequest ^. SearchRequestRiderId ==. val (toKey perId)
    return searchRequest
