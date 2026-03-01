module Lib.Yudhishthira.Event where

import qualified Data.Aeson as A
import Data.Scientific
import JsonLogic
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Utils.Common
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Storage.Queries.NammaTag as SQNT
import qualified Lib.Yudhishthira.Storage.Queries.NammaTagTrigger as SQNTT
import Lib.Yudhishthira.Tools.Utils (mkTagNameValue, mkTagNameValueExpiry)
import Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.NammaTag as DNT
import Kernel.Types.Id

yudhishthiraDecide ::
  ( MonadFlow m,
    Metrics.CoreMetrics m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasYudhishthiraTablesSchema
    -- HasCacConfig r
  ) =>
  YudhishthiraDecideReq ->
  m YudhishthiraDecideResp
yudhishthiraDecide req = do
  nammaTags <-
    case req.source of
      Application event -> do
        nammaTagsTrigger <- SQNTT.findAllByEvent event
        let tagNames = nammaTagsTrigger <&> (.tagName)
        when (null tagNames) $
          logWarning $ "No triggers found for event: " <> show event
        nammaTags <- SQNT.findAllByPrimaryKeys tagNames
        let missedTags = filter (`notElem` (nammaTags <&> (.name))) tagNames
        unless (null missedTags) $
          logError $ "Some tags missing for event: " <> show event <> "; tags: " <> show missedTags
        pure nammaTags
      KaalChakra chakra -> SQNT.findAllByChakra chakra
  logDebug $ "NammaTags for source <> " <> show req.source <> ": " <> show nammaTags
  logDebug $ "SourceData: " <> show req.sourceData
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
      let tagValidity = tag.validity
      mbRespValue <-
        case tag.rule of
          LLM _ -> return Nothing
          RuleEngine rule -> do
            let resp = jsonLogicEither rule req.sourceData
            case resp of
              Left err -> do
                logError $ "Invalid tag rule: " <> show err
                return Nothing
              Right val -> return $ Just val
      logDebug $ "Tag: " <> show tag <> " jsonResp: " <> show mbRespValue
      mbTagValue <- case mbRespValue of
        Just (A.String text) -> return $ Just (TextValue text)
        Just (A.Number number) -> do
          let doubleValue = toRealFloat number -- :: Maybe Int = toBoundedInteger number
          return $ Just (NumberValue doubleValue)
        Just (A.Array arr') -> return (ArrayValue <$> mapM extractText (toList arr'))
        value -> do
          logError $ "Invalid value for tag: " <> show value
          return Nothing
      logDebug $ "Tag: " <> show tag <> " Value: " <> show mbTagValue
      return $
        mbTagValue
          <&> \tagValue ->
            NammaTagResponse
              { tagName = tag.name,
                tagValue,
                tagCategory = tag.category,
                tagValidity
              }

    extractText :: A.Value -> Maybe Text
    extractText (A.String txt) = Just txt
    extractText _ = Nothing

computeNammaTags ::
  ( MonadFlow m,
    Metrics.CoreMetrics m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasYudhishthiraTablesSchema,
    ToJSON a
  ) =>
  Id MerchantOperatingCity ->
  ApplicationEvent ->
  a ->
  m [TagNameValue]
computeNammaTags merchantOpCityId event sourceData_ = do
  let sourceData = A.toJSON sourceData_
  let req = YudhishthiraDecideReq {merchantOperatingCityId = merchantOpCityId, source = Application event, sourceData}
  resp <- yudhishthiraDecide req
  pure $ resp.tags <&> (\tag -> mkTagNameValue (TagName tag.tagName) tag.tagValue)

computeNammaTagsWithExpiry ::
  ( MonadFlow m,
    Metrics.CoreMetrics m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasYudhishthiraTablesSchema,
    ToJSON a
  ) =>
  Id MerchantOperatingCity ->
  ApplicationEvent ->
  a ->
  m [TagNameValueExpiry]
computeNammaTagsWithExpiry merchantOpCityId event sourceData_ = do
  let sourceData = A.toJSON sourceData_
  let req = YudhishthiraDecideReq {merchantOperatingCityId = merchantOpCityId, source = Application event, sourceData}
  resp <- yudhishthiraDecide req
  now <- getCurrentTime
  pure $ resp.tags <&> (\tag -> mkTagNameValueExpiry (TagName tag.tagName) tag.tagValue tag.tagValidity now)
