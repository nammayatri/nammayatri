module Mobility.AppBackend.Queries where

import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (findById)
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Geofencing (GeoRestriction (Regions))
import Beckn.Types.Id
import qualified "app-backend" Domain.Types.Merchant as DM
import "app-backend" Storage.Tabular.Merchant

updateOrigAndDestRestriction :: Id DM.Merchant -> [Text] -> [Text] -> SqlDB ()
updateOrigAndDestRestriction merchantId originList destinationList =
  Esq.update $ \tbl -> do
    set
      tbl
      [ MerchantOriginRestriction =. val (Regions originList),
        MerchantDestinationRestriction =. val (Regions destinationList)
      ]
    where_ $ tbl ^. MerchantTId ==. val (toKey merchantId)
