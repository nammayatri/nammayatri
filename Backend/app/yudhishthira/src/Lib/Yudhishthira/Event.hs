module Lib.Yudhishthira.Event where

import qualified Data.Aeson as A
import Data.Scientific
import qualified Data.Text.Lazy as DTE
import qualified Data.Text.Lazy.Encoding as DTE
import qualified Domain.Types.NammaTag as DNT
import EulerHS.Types as Euler
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Yudhishthira.Types
import Servant hiding (Application, throwError)
import qualified Storage.Queries.NammaTag as SQNT
import Tools.Utils

-- Should we add auth?
type YudhishthiraDecideAPI =
  "v1"
    :> "yudhishthira"
    :> "decide"
    :> ReqBody '[JSON] YudhishthiraDecideReq
    :> Post '[JSON] YudhishthiraDecideResp

yudhishthiraDecideAPI ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    HasCacConfig r,
    HasFlowEnv m r '["yudhishthiraUrl" ::: BaseUrl]
  ) =>
  YudhishthiraDecideReq ->
  m YudhishthiraDecideResp
yudhishthiraDecideAPI req = do
  let proxy = Proxy @YudhishthiraDecideAPI
      eulerClient = Euler.client proxy req
  url <- asks (.yudhishthiraUrl)
  callAPI url eulerClient "yudhishthira-decide" proxy
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_YUDHISHTHIRA_DECIDE") url)

yudhishthiraDecide ::
  ( MonadFlow m,
    Metrics.CoreMetrics m,
    EsqDBFlow m r,
    CacheFlow m r
    -- HasCacConfig r
  ) =>
  YudhishthiraDecideReq ->
  m YudhishthiraDecideResp
yudhishthiraDecide req = do
  nammaTags <-
    case req.source of
      Application event -> SQNT.findAllByApplicationEvent event
      KaalChakra chakra -> SQNT.findAllByChakra chakra
  tags <- convertToTagResponses nammaTags
  return $ YudhishthiraDecideResp {..}
  where
    convertToTagResponses ::
      (MonadFlow m) =>
      [DNT.NammaTag] ->
      m [NammaTagResponse]
    convertToTagResponses tags = do
      mbTagResponses <- mapM convertToTagResponse tags
      return $ catMaybes mbTagResponses

    convertToTagResponse :: (MonadFlow m) => DNT.NammaTag -> m (Maybe NammaTagResponse)
    convertToTagResponse tag = do
      let tagValidity = case tag.info of
            DNT.Application _ -> Nothing
            DNT.KaalChakra (DNT.KaalChakraTagInfo _ validity) -> validity
      respValue <-
        case tag.rule of
          LLM context -> throwError $ InternalError $ "LLM not supported yet: " <> show context
          RuleEngine rule -> runJsonLogic req.sourceData rule
      tagValue <- case respValue of
        A.String text -> return $ TextValue text
        A.Number number -> do
          let mbValue :: Maybe Int = toBoundedInteger number
          maybe (throwError (InternalError "Invalid number value")) (return . NumberValue) mbValue
        _ -> throwError $ InternalError "Invalid response from rule engine"
      return $
        Just $
          NammaTagResponse
            { tagName = tag.name,
              tagValue,
              tagCategory = tag.category,
              tagValidity
            }

data Handle m a = Handle
  { updateTags :: (Text -> m ()),
    getData :: m a
  }

addEvent ::
  ( MonadFlow m,
    Metrics.CoreMetrics m,
    HasFlowEnv m r '["yudhishthiraUrl" ::: BaseUrl],
    EsqDBFlow m r,
    CacheFlow m r,
    ToJSON a
  ) =>
  ApplicationEvent ->
  Handle m a ->
  m ()
addEvent event Handle {..} = do
  sourceData_ <- getData
  let sourceData = DTE.toStrict . DTE.decodeUtf8 $ A.encode sourceData_
  let req = YudhishthiraDecideReq {source = Application event, sourceData}
  resp <- yudhishthiraDecide req
  resp.tags `forM_` \tag -> do
    tagValue <- case tag.tagValue of
      TextValue text -> return text
      NumberValue number -> return $ show number
    let tagText = tag.tagName <> "#" <> tagValue
    updateTags tagText
