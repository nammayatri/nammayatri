{-# LANGUAGE TypeFamilies #-}


module Beckn.Product.Blacklist where

import qualified Beckn.Data.Accessor                     as Accessor
import qualified Beckn.Storage.Queries.Blacklist         as DB
import qualified Beckn.Storage.Queries.RegistrationToken as RegToken
import           Beckn.Types.API.Blacklist
import           Beckn.Types.App
import           Beckn.Types.Common
import           Beckn.Types.Storage.Blacklist           as Storage
import qualified Beckn.Types.Storage.RegistrationToken   as RegToken
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


create :: Maybe RegistrationTokenText -> CreateReq -> FlowHandler CreateRes
create mRegToken CreateReq {..} =  withFlowHandler $ do
   verifyToken mRegToken
   id <- generateGUID
   regToken <- fromMaybeM400 "INVALID_TOKEN" mRegToken
    >>= RegToken.findRegistrationTokenByToken
    >>= fromMaybeM400 "INVALID_TOKEN"
   case (RegToken._entityType regToken) of
     RegToken.USER -> do
        blacklist <- blacklistRec id $ RegToken._EntityId regToken
        DB.create blacklist
        eres <- DB.findById id
        case eres of
          Right (Just blacklistDb) -> return $ CreateRes blacklistDb
          _                 -> L.throwException $ err500 {errBody = "Could not create Blacklist"}
     RegToken.CUSTOMER -> L.throwException $ err401 {errBody = "Unauthorized"}
    where
      blacklistRec id userId = do
        now  <- getCurrTime
        return Storage.Blacklist
          { _id         = id
          , _createdAt  = now
          , _updatedAt  = now
          , _info       = Nothing
          , _BlacklistedBy = UserId userId
          ,..
          }

list ::
  Maybe RegistrationTokenText
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


get :: Maybe Text -> BlacklistId -> FlowHandler GetRes
get mRegToken blacklistId = withFlowHandler $ do
  verifyToken mRegToken
  DB.findById blacklistId
  >>= \case
    Right (Just user) -> return user
    Right Nothing -> L.throwException $ err400 {errBody = "Blacklist not found"}
    Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}


update ::
  Maybe Text ->
  BlacklistId ->
  UpdateReq ->
  FlowHandler UpdateRes
update mRegToken blacklistId lb@UpdateReq{..} = withFlowHandler $ do
  verifyToken mRegToken
  eres <- DB.update blacklistId lb
  case eres of
    Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
    Right _ ->
      DB.findById blacklistId
      >>= \case
        Right (Just v) -> return $ UpdateRes v
        Right Nothing -> L.throwException $ err400 {errBody = "Blacklist not found"}
        Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}



delete :: Maybe Text -> BlacklistId -> FlowHandler Ack
delete mRegToken blacklistId = withFlowHandler $ do
  verifyToken mRegToken
  mres <- DB.deleteById blacklistId
  case mres of
    Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
    Right () -> sendAck


