{-# LANGUAGE TypeFamilies #-}


module Beckn.Product.LocationBlacklist where

import qualified Beckn.Data.Accessor                     as Accessor
import qualified Beckn.Storage.Queries.LocationBlacklist as DB
import qualified Beckn.Storage.Queries.RegistrationToken as RegToken
import           Beckn.Types.API.LocationBlacklist
import           Beckn.Types.App
import           Beckn.Types.Common
import           Beckn.Types.Storage.LocationBlacklist   as Storage
import qualified Beckn.Types.Storage.RegistrationToken   as RegToken
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
   verifyToken mRegToken
   id <- generateGUID
   regToken <- fromMaybeM400 "INVALID_TOKEN" mRegToken
    >>= RegToken.findRegistrationTokenByToken
    >>= fromMaybeM400 "INVALID_TOKEN"
   case (RegToken._entityType regToken) of
     RegToken.USER -> do
        locationBlacklist <- locationBlacklistRec id $ RegToken._EntityId regToken
        DB.create locationBlacklist
        eres <- DB.findById id
        case eres of
          Right (Just locationBlacklistDb) -> return $ CreateRes locationBlacklistDb
          _                 -> L.throwException $ err500 {errBody = "Could not create LocationBlacklist"}
     RegToken.CUSTOMER -> L.throwException $ err401 {errBody = "Unauthorized"}
    where
      locationBlacklistRec id userId = do
        now  <- getCurrTime
        return Storage.LocationBlacklist
          { _id         = id
          , _createdAt  = now
          , _updatedAt  = now
          , _info       = Nothing
          , _BlacklistedBy = UserId userId
          ,..
          }

list ::
  Maybe RegistrationToken
  -> Maybe Limit
  -> Maybe Offset
  -> EntityType
  -> Text
  -> FlowHandler ListRes
list mRegToken mlimit moffset entityType entityId = withFlowHandler $ do
  verifyToken mRegToken
  DB.findAllWithLimitOffset mlimit moffset entityType entityId
  >>= \case
      Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
      Right v -> return $ ListRes v


get :: Maybe Text -> LocationBlacklistId -> FlowHandler GetRes
get mRegToken locationBlacklistId = withFlowHandler $ do
  verifyToken mRegToken
  DB.findById locationBlacklistId
  >>= \case
    Right (Just user) -> return user
    Right Nothing -> L.throwException $ err400 {errBody = "LocationBlacklist not found"}
    Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}


update ::
  Maybe Text ->
  LocationBlacklistId ->
  UpdateReq ->
  FlowHandler UpdateRes
update mRegToken locationBlacklistId lb@UpdateReq{..} = withFlowHandler $ do
  verifyToken mRegToken
  eres <- DB.update locationBlacklistId lb
  case eres of
    Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
    Right _ ->
      DB.findById locationBlacklistId
      >>= \case
        Right (Just v) -> return $ UpdateRes v
        Right Nothing -> L.throwException $ err400 {errBody = "LocationBlacklist not found"}
        Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}



delete :: Maybe Text -> LocationBlacklistId -> FlowHandler Ack
delete mRegToken locationBlacklistId = withFlowHandler $ do
  verifyToken mRegToken
  mres <- DB.deleteById locationBlacklistId
  case mres of
    Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
    Right () -> sendAck


