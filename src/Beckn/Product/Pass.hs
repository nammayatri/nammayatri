module Beckn.Product.Pass where

import qualified Beckn.Data.Accessor      as Accessor
import           Beckn.Types.API.Pass
import           Beckn.Types.App
import           Beckn.Types.Common
import           Beckn.Types.Storage.Pass
import qualified Beckn.Storage.Queries.Pass as QP
import           Beckn.Utils.Routes
import           Beckn.Utils.Storage
import           Data.Aeson
import           EulerHS.Prelude
import qualified EulerHS.Language as L
import           Servant

getPassById :: Maybe Text -> Text -> FlowHandler PassRes
getPassById regToken passId = withFlowHandler $ do
  reg <- verifyToken regToken
  QP.findPassById (PassId passId) >>=
    maybe
      (L.throwException $ err400 {errBody = "INVALID_DATA"})
      (return . PassRes)

updatePass :: Maybe Text -> Text -> UpdatePassReq -> FlowHandler PassRes
updatePass regToken passId req = withFlowHandler $ do
  reg <- verifyToken regToken
  pass <-
    QP.findPassById (PassId passId) >>=
      maybe
        (L.throwException $ err400 {errBody = "INVALID_DATA"})
        return
  QP.updatePassStatus (action req) (PassId passId)
  return $ PassRes (pass { _status = action req })

listPass ::
  Maybe Text
  -> Maybe PassIDType
  -> Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> [Status]
  -> [PassType]
  -> FlowHandler ListPassRes
listPass regToken passIdType passV limitM offsetM statusM typeM = withFlowHandler $ do
  reg <- verifyToken regToken
  undefined
  --case (limitM, offsetM) of
    --(Just l, Just o) ->
    --_ -> QP.listAllPasses

