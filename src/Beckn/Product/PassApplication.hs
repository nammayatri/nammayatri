module Beckn.Product.PassApplication where

import qualified Beckn.Data.Accessor             as Accessor
import           Beckn.Types.API.Common
import           Beckn.Types.API.PassApplication
import           Beckn.Types.Storage.PassApplication
import           Beckn.Types.App
import           Data.Aeson
import           EulerHS.Prelude

createPassApplication ::
  Maybe Text -> CreatePassApplicationReq -> FlowHandler PassApplicationRes
createPassApplication regToken req = undefined

listPassApplication ::
  Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> [Status]
  -> [PassType]
  -> FlowHandler ListPassApplicationRes
listPassApplication regToken offsetM limitM status passType = undefined

getPassApplicationById :: Maybe Text -> Text -> FlowHandler PassApplicationRes
getPassApplicationById regToken applicationId = undefined

updatePassApplication ::
  Maybe Text ->
  Text ->
  UpdatePassApplicationReq ->
  FlowHandler PassApplicationRes
updatePassApplication regToken passApplicationId req = undefined

