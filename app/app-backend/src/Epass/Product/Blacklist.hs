{-# LANGUAGE TypeFamilies #-}

module Epass.Product.Blacklist where

import App.Types
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

create :: RegToken -> CreateReq -> FlowHandler CreateRes
create regToken CreateReq {..} = withFlowHandler $ do
  token <- verifyToken regToken
  id <- generateGUID
  case RegToken._entityType token of
    RegToken.USER -> do
      blacklist <- blacklistRec id $ RegToken._EntityId token
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
  RegToken ->
  Maybe Limit ->
  Maybe Offset ->
  EntityType ->
  Text ->
  FlowHandler ListRes
list regToken mlimit moffset entityType entityId =
  withFlowHandler $
    do
      verifyToken regToken
      DB.findAllWithLimitOffset mlimit moffset entityType entityId
      >>= \case
        Left err -> L.throwException $ err500 {errBody = "DBError: " <> show err}
        Right v -> return $ ListRes v

get :: RegToken -> BlacklistId -> FlowHandler GetRes
get regToken blacklistId =
  withFlowHandler $
    do
      verifyToken regToken
      DB.findById blacklistId
      >>= \case
        Right (Just user) -> return user
        Right Nothing -> L.throwException $ err400 {errBody = "Blacklist not found"}
        Left err -> L.throwException $ err500 {errBody = "DBError: " <> show err}

update ::
  RegToken ->
  BlacklistId ->
  UpdateReq ->
  FlowHandler UpdateRes
update regToken blacklistId lb@UpdateReq {..} = withFlowHandler $ do
  verifyToken regToken
  eres <- DB.update blacklistId lb
  case eres of
    Left err -> L.throwException $ err500 {errBody = "DBError: " <> show err}
    Right _ ->
      DB.findById blacklistId
        >>= \case
          Right (Just v) -> return $ UpdateRes v
          Right Nothing -> L.throwException $ err400 {errBody = "Blacklist not found"}
          Left err -> L.throwException $ err500 {errBody = "DBError: " <> show err}

delete :: RegToken -> BlacklistId -> FlowHandler Ack
delete regToken blacklistId = withFlowHandler $ do
  verifyToken regToken
  mres <- DB.deleteById blacklistId
  case mres of
    Left err -> L.throwException $ err500 {errBody = "DBError: " <> show err}
    Right () -> sendAck
