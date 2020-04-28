module Beckn.Product.Quota where

import qualified Beckn.Data.Accessor       as Accessor
import           Beckn.Types.API.Quota
import           Beckn.Types.App
import           Beckn.Types.Common
import           Beckn.Types.Storage.Quota as Storage
import           Data.Aeson
import           Data.Default
import           EulerHS.Prelude


create :: Maybe RegistrationToken -> CreateReq -> FlowHandler CreateRes
create mRegToken CreateReq {..} =  pure $ def CreateRes

update :: Maybe RegistrationToken -> QuotaId -> UpdateReq -> FlowHandler UpdateRes
update mRegToken id UpdateReq {..} = error "Not implemented"

list ::
  Maybe RegistrationToken
  -> Maybe Text
  -> Maybe Limit
  -> Maybe Offset -> FlowHandler ListRes
list mRegToken mtype mlimit moffset = pure $ ListRes {_quotas = [def Storage.Quota]}

get :: Maybe Text -> QuotaId -> FlowHandler GetRes
get regToken userId = pure $ def Quota
