module Storage.Queries.SearchReqLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.SearchReqLocation
import Storage.Tabular.SearchReqLocation

create :: SearchReqLocation -> SqlDB ()
create s =
  Esq.insertSelect' $
    pure $
      SearchReqLocationT
        Esq.<# val s.id.getId
        Esq.<#> val s.lat
        Esq.<#> val s.lon
        Esq.<#> Esq.getPoint (val s.lat, val s.lon)
        Esq.<#> val s.street
        Esq.<#> val s.door
        Esq.<#> val s.city
        Esq.<#> val s.state
        Esq.<#> val s.country
        Esq.<#> val s.building
        Esq.<#> val s.areaCode
        Esq.<#> val s.area
        Esq.<#> val s.createdAt
        Esq.<#> val s.updatedAt

findById ::
  Transactionable m =>
  Id SearchReqLocation ->
  m (Maybe SearchReqLocation)
findById = Esq.findById
