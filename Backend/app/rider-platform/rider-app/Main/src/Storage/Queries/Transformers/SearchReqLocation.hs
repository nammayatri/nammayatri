module Storage.Queries.Transformers.SearchReqLocation where

import qualified Domain.Types.LocationAddress
import Kernel.Prelude

mkLocationAddress :: (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.LocationAddress.LocationAddress)
mkLocationAddress area areaCode building city country door placeId state street ward = Domain.Types.LocationAddress.LocationAddress {..}