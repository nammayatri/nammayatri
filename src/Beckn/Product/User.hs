module Beckn.Product.User where

import qualified Beckn.Data.Accessor      as Accessor
import           Beckn.Types.API.Common
import           Beckn.Types.API.User
import           Beckn.Types.App
import           Beckn.Types.Storage.User
import           Data.Aeson
import           EulerHS.Prelude

create ::
  Maybe Text -> CreateReq -> FlowHandler CreateRes
create regToken req = undefined

list ::
  Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> FlowHandler ListRes
list regToken offsetM limitM = undefined

getUserById :: Maybe Text -> Text -> FlowHandler User
getUserById regToken applicationId = undefined

update ::
  Maybe Text ->
  UserId ->
  UpdateReq ->
  FlowHandler UpdateRes
update regToken userId req = undefined

