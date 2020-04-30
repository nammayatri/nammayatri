module Beckn.Product.Quota where

import qualified Beckn.Storage.Queries.Quota as DB
import           Beckn.Types.API.Quota
import           Beckn.Types.App
import           Beckn.Types.Common
import           Beckn.Types.Storage.Quota   as Storage
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

create :: Maybe RegistrationTokenText -> CreateReq -> FlowHandler CreateRes
create mRegToken CreateReq {..} =  withFlowHandler $ do
   verifyToken mRegToken
   id <- generateGUID
   quota <-  quotaRec id
   DB.create quota
   eres <- DB.findById id
   case eres of
     Right (Just quotaDb) -> return $ CreateRes quotaDb
     _                 -> L.throwException $ err500 {errBody = "Could not create Quota"}
    where
      quotaRec id = do
        now  <- getCurrTime
        return Storage.Quota
          { _id         = id
          , _createdAt  = now
          , _updatedAt  = now
          , _info       = Nothing
          ,..
          }

update :: Maybe RegistrationTokenText -> QuotaId -> UpdateReq -> FlowHandler UpdateRes
update mRegToken id UpdateReq {..} =  withFlowHandler $ do
  verifyToken mRegToken
  eres <- DB.update id _maxAllowed _startTime _endTime
  case eres of
    Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
    Right _ ->
      DB.findById id
      >>= \case
        Right (Just v) -> return $ UpdateRes v
        Right Nothing -> L.throwException $ err400 {errBody = "Quota not found"}
        Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}

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

get :: Maybe Text -> QuotaId -> FlowHandler GetRes
get mRegToken quotaId = withFlowHandler $ do
  verifyToken mRegToken
  DB.findById quotaId
  >>= \case
    Right (Just user) -> return user
    Right Nothing -> L.throwException $ err400 {errBody = "Quota not found"}
    Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
