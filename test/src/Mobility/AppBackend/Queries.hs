module Mobility.AppBackend.Queries where

import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (findById)
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Geofencing (GeoRestriction (Regions))
import "app-backend" Storage.Tabular.Merchant

updateOrigAndDestRestriction :: [Text] -> [Text] -> SqlDB ()
updateOrigAndDestRestriction originList destinationList =
  Esq.update $ \tbl -> do
    set
      tbl
      [ MerchantOriginRestriction =. val (Regions originList),
        MerchantDestinationRestriction =. val (Regions destinationList)
      ]
    where_ $ tbl ^. MerchantId ==. val "da4e23a5-3ce6-4c37-8b9b-41377c3c1a51"
