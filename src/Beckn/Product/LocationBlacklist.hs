{-# LANGUAGE TypeFamilies #-}


module Beckn.Product.LocationBlacklist where

import qualified Beckn.Data.Accessor                     as Accessor
import qualified Beckn.Storage.Queries.LocationBlacklist as DB
import           Beckn.Types.API.LocationBlacklist
import           Beckn.Types.App
import           Beckn.Types.Storage.LocationBlacklist   as Storage
import           Beckn.Utils.Common
import           Beckn.Utils.Common
import           Beckn.Utils.Routes
import           Beckn.Utils.Storage
import           Data.Aeson
import           Data.Default
import           Data.Time
import qualified Database.Beam.Schema.Tables             as B
import qualified EulerHS.Language                        as L
import           EulerHS.Prelude
import           Servant



create :: Maybe RegistrationToken -> CreateReq -> FlowHandler CreateRes
create mRegToken CreateReq {..} =  withFlowHandler $ do
  --  verifyToken mRegToken
   id <- generateGUID
   locationBlacklist <- locationBlacklistRec id
   DB.create locationBlacklist
   eres <- DB.findById id
   case eres of
     Right (Just locationBlacklistDb) -> return $ CreateRes locationBlacklistDb
     _                 -> L.throwException $ err500 {errBody = "Could not create LocationBlacklist"}
    where
      locationBlacklistRec id = do
        now  <- getCurrTime
        return Storage.LocationBlacklist
          { _id         = id
          , _createdAt  = now
          , _updatedAt  = now
          , _info       = Nothing
          ,..
          }

list ::
  Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> Maybe Int
  -> FlowHandler ListRes
list regToken maybeWard maybeDistrict maybeCity maybeState maybePincode offsetM limitM =
  pure $ ListRes {_location_blacklists = [def Storage.LocationBlacklist]}

get :: Maybe Text -> LocationBlacklistId -> FlowHandler GetRes
get regToken userId = pure $ def LocationBlacklist

update ::
  Maybe Text ->
  LocationBlacklistId ->
  UpdateReq ->
  FlowHandler UpdateRes
update regToken userId req = pure $ def UpdateRes
