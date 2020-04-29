{-# LANGUAGE TypeFamilies #-}


module Beckn.Product.User where

import qualified Beckn.Data.Accessor         as Accessor
import qualified Beckn.Storage.Queries.User  as DB
import           Beckn.Types.API.User
import           Beckn.Types.App
import           Beckn.Types.Storage.User    as Storage
import           Beckn.Utils.Common
import           Beckn.Utils.Routes
import           Beckn.Utils.Storage
import           Data.Aeson
import           Data.Default
import           Data.Time
import qualified Database.Beam.Schema.Tables as B
import qualified EulerHS.Language            as L
import           EulerHS.Prelude
import           Servant

create ::
  Maybe Text -> CreateReq -> FlowHandler CreateRes
create regToken CreateReq {..} = withFlowHandler $ do
  verifyToken regToken
  id <- generateGUID
  user <- userInfo id
  DB.create user
  eres <- DB.findById id
  case eres of
    Right (Just user) -> return $ CreateRes user
    _                 -> L.throwException $ err500 {errBody = "Could not create PassApplication"}
  where
    userInfo id = do
      now  <- getCurrTime
      return Storage.User
        { _id = id
        , _verified = False
        , _status = INACTIVE
        , _info = Nothing
        , _createdAt = now
        , _updatedAt = now
        , ..
        }

list ::
  Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> FlowHandler ListRes
list regToken offsetM limitM = withFlowHandler $ do
  verifyToken regToken
  DB.findAllWithLimitOffset limitM offsetM
  >>= \case
      Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
      Right v -> return $ ListRes v

get :: Maybe Text -> UserId -> FlowHandler GetRes
get regToken userId = withFlowHandler $ do
  verifyToken regToken
  DB.findById userId
  >>= \case
    Right (Just user) -> return user
    Right Nothing -> L.throwException $ err400 {errBody = "User not found"}
    Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}

update ::
  Maybe Text ->
  UserId ->
  UpdateReq ->
  FlowHandler UpdateRes
update regToken userId UpdateReq{..} = withFlowHandler $ do
  verifyToken regToken
  eres <- DB.update userId _status _name _email _role
  case eres of
    Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
    Right _ ->
      DB.findById userId
      >>= \case
        Right (Just v) -> return $ UpdateRes v
        Right Nothing -> L.throwException $ err400 {errBody = "User not found"}
        Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
