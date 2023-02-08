module Mobility.AppBackend.Queries where

import qualified "app-backend" Domain.Types.Merchant as DM
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (findById)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Geofencing (GeoRestriction (Regions))
import Kernel.Types.Id
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
