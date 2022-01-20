module Types.Beckn.API.OnSearch (module Types.Beckn.API.OnSearch, module Reexport) where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))
import Types.Beckn.Catalog as Reexport (Catalog (..))
import Types.Beckn.Category as Reexport (Category (..))
import Types.Beckn.DecimalValue as Reexport
  ( DecimalValue (..),
    convertAmountToDecimalValue,
    convertDecimalValueToAmount,
  )
import Types.Beckn.Descriptor as Reexport (Descriptor (..))
import Types.Beckn.Item as Reexport (Item (..))
import Types.Beckn.Price as Reexport (Price (..))
import Types.Beckn.Provider as Reexport (DescriptorInfo (..), Provider (..))

type OnSearchAPI =
  "on_search"
    :> ReqBody '[JSON] (BecknCallbackReq OnSearchCatalog)
    :> Post '[JSON] AckResponse

onSearchAPI :: Proxy OnSearchAPI
onSearchAPI = Proxy

newtype OnSearchCatalog = OnSearchCatalog
  { catalog :: Catalog
  }
  deriving (Generic, Show, FromJSON, ToJSON)
