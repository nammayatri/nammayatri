{-# LANGUAGE TypeFamilies #-}


module Beckn.Product.LocationBlacklist where

import qualified Beckn.Data.Accessor                   as Accessor
import           Beckn.Types.API.LocationBlacklist
import           Beckn.Types.App
import           Beckn.Types.Storage.LocationBlacklist as Storage
import           Beckn.Utils.Common
import           Data.Aeson
import           Data.Default
import           Data.Time
import qualified Database.Beam.Schema.Tables           as B
import qualified EulerHS.Language                      as L
import           EulerHS.Prelude


create ::
  Maybe Text -> CreateReq -> FlowHandler CreateRes
create regToken (CreateReq {..}) = do
  pure $ def CreateRes

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
