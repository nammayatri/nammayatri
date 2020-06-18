{-# LANGUAGE TypeFamilies #-}

module Epass.Product.Blacklist where

import Beckn.Types.Common
import qualified Beckn.Types.Storage.RegistrationToken as RegToken
import Beckn.Utils.Common
import Beckn.Utils.Extra (getCurrentTimeUTC)
import Data.Aeson
import Data.Default
import Data.Time
import qualified Database.Beam.Schema.Tables as B
import qualified Epass.Data.Accessor as Accessor
import qualified Epass.Storage.Queries.Blacklist as DB
import Epass.Types.API.Blacklist
import Epass.Types.App
import Epass.Types.Common
import Epass.Types.Storage.Blacklist as Storage
import Epass.Utils.Common
import Epass.Utils.Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.RegistrationToken as RegToken

create :: Maybe RegistrationTokenText -> CreateReq -> FlowHandler CreateRes
create mRegToken CreateReq {..} = withFlowHandler $ do
  verifyToken mRegToken
  id <- generateGUID
  regToken <-
    fromMaybeM400 "INVALID_TOKEN" mRegToken
      >>= RegToken.findByToken
      >>= fromMaybeM400 "INVALID_TOKEN"
  case (RegToken._entityType regToken) of
    RegToken.USER -> do
      blacklist <- blacklistRec id $ RegToken._EntityId regToken
      DB.create blacklist
      eres <- DB.findById id
      case eres of
        Right (Just blacklistDb) -> return $ CreateRes blacklistDb
        _ -> L.throwException $ err500 {errBody = "Could not create Blacklist"}
    RegToken.CUSTOMER -> L.throwException $ err401 {errBody = "Unauthorized"}
  where
    blacklistRec id userId = do
      now <- getCurrentTimeUTC
      return
        Storage.Blacklist
          { _id = id,
            _createdAt = now,
            _updatedAt = now,
            _info = Nothing,
            __BlacklistedBy = UserId userId,
            ..
          }

list ::
  Maybe RegistrationTokenText ->
  Maybe Limit ->
  Maybe Offset ->
  EntityType ->
  Text ->
  FlowHandler ListRes
list mRegToken mlimit moffset entityType entityId = withFlowHandler $
  do
    verifyToken mRegToken
    DB.findAllWithLimitOffset mlimit moffset entityType entityId
    >>= \case
      Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
      Right v -> return $ ListRes v

get :: Maybe Text -> BlacklistId -> FlowHandler GetRes
get mRegToken blacklistId = withFlowHandler $
  do
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
update mRegToken blacklistId lb@UpdateReq {..} = withFlowHandler $ do
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
