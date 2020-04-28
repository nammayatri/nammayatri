{-# LANGUAGE TypeFamilies #-}


module Beckn.Product.User where

import qualified Beckn.Data.Accessor         as Accessor
import           Beckn.Types.API.User
import           Beckn.Types.App
import           Beckn.Types.Storage.User
import           Beckn.Utils.Common
import           Data.Aeson
import           Data.Default
import           Data.Time
import qualified Database.Beam.Schema.Tables as B
import qualified EulerHS.Language            as L
import           EulerHS.Prelude
import           Beckn.Utils.Routes


create ::
  Maybe Text -> CreateReq -> FlowHandler CreateRes
create regToken (CreateReq {..}) = do
  now  <- withFlowHandler $ getCurrTime
  pure $ def CreateRes

list ::
  Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> FlowHandler ListRes
list regToken offsetM limitM = pure $ def ListRes

get :: Maybe Text -> UserId -> FlowHandler GetRes
get regToken userId = pure $ def User

update ::
  Maybe Text ->
  UserId ->
  UpdateReq ->
  FlowHandler UpdateRes
update regToken userId req = pure $ def UpdateRes
