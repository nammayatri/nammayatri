module Beckn.Product.Quota where

import qualified Beckn.Data.Accessor    as Accessor
import           Beckn.Types.API.Common
import           Beckn.Types.API.Quota
import           Beckn.Types.App
import           Data.Aeson
import           EulerHS.Prelude

create :: Maybe RegistrationToken -> CreateReq -> FlowHandler CreateRes
create mRegToken CreateReq {..} = error "Not implemented"

update :: Maybe RegistrationToken -> QuotaId -> UpdateReq -> FlowHandler UpdateRes
update mRegToken id UpdateReq {..} = error "Not implemented"

list ::
  Maybe RegistrationToken
  -> Maybe Text
  -> Maybe Limit
  -> Maybe Offset -> FlowHandler ListRes
list mRegToken mtype mlimit moffset =
  error "Not implemented"
