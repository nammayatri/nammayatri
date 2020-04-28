module Beckn.Product.Pass where

import qualified Beckn.Data.Accessor      as Accessor
import           Beckn.Types.API.Pass
import           Beckn.Types.App
import           Beckn.Types.Common
import           Beckn.Types.Storage.Pass
import           Data.Aeson
import           EulerHS.Prelude

getPassById :: Maybe Text -> Text -> FlowHandler PassRes
getPassById regToken passId = undefined

updatePass :: Maybe Text -> Text -> UpdatePassReq -> FlowHandler PassRes
updatePass regToken passId req = undefined

listPass ::
  Maybe Text
  -> Maybe PassIDType
  -> Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> [Status]
  -> [PassType]
  -> FlowHandler ListPassRes
listPass regToken limitM offsetM statusM typeM = undefined
