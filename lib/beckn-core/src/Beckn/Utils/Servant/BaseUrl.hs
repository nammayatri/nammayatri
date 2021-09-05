module Beckn.Utils.Servant.BaseUrl where

import Data.Text
import EulerHS.Prelude
import Servant.Client.Core

showBaseUrlText :: BaseUrl -> Text
showBaseUrlText = toText . showBaseUrl
