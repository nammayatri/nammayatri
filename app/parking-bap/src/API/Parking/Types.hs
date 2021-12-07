module API.Parking.Types where

import qualified API.Parking.Search.Types as Search
import qualified API.Parking.SearchId.Quotes.Types as Quotes
import Servant

type API =
  "parking"
    :> ( Search.API
           :<|> Quotes.API
       )
