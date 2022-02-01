module Storage.Queries.PublicTranport where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Domain.PublicTranport
import Storage.Tabular.PublicTranport

findByStationCode :: EsqDBFlow m r => Text -> m (Maybe PublicTranport)
findByStationCode stationCode =
  runTransaction . findOne' $ do
    publicTransport <- from $ table @PublicTranportT
    where_ $ publicTransport ^. PublicTranportStationCode ==. val stationCode
    return publicTransport

create :: PublicTranport -> SqlDB ()
create = create'

findAll :: EsqDBFlow m r => m [PublicTranport]
findAll =
  runTransaction . findAll' $ do
    from $ table @PublicTranportT

findById :: EsqDBFlow m r => Id PublicTranport -> m (Maybe PublicTranport)
findById publicTransportId =
  runTransaction . findOne' $ do
    publicTransport <- from $ table @PublicTranportT
    where_ $ publicTransport ^. PublicTranportTId ==. val (PublicTranportTKey $ getId publicTransportId)
    return publicTransport