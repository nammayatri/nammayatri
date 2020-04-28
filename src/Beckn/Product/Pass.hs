module Beckn.Product.Pass where

import qualified Beckn.Data.Accessor    as Accessor
import           Beckn.Types.API.Common
import           Beckn.Types.API.Pass
import           Beckn.Types.App
import           Data.Aeson
import           EulerHS.Prelude

getPassById :: Maybe Text -> Text -> FlowHandler PassRes
getPassById regToken passId = undefined

updatePass :: Maybe Text -> Text -> UpdatePassReq -> FlowHandler PassRes
updatePass regToken passId req = undefined

listPass :: Maybe Text -> ListPassReq -> FlowHandler ListPassRes
listPass regToken req = undefined
