module Domain.Types.SearchSource where

import Data.Aeson
import Kernel.Prelude

data SearchSource = ALL | DASHBOARD | MOBILE_APP deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

getSearchSources :: Bool -> [SearchSource]
getSearchSources isDashboard = [ALL] <> (if isDashboard then [DASHBOARD] else [MOBILE_APP])
