module Lib.Yudhishthira.Tools.Utils where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as DBL
import Data.Either.Extra (mapLeft)
import qualified Data.String.Conversions as CS
import qualified Data.Text as T
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.Lazy as DTE
import qualified Data.Text.Lazy.Encoding as DTLE
import qualified Data.Time.Format as Time
import qualified Data.Vector as V
import qualified Data.Vector as Vector
import JsonLogic
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Storage.Queries.NammaTag as QNammaTag
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types.NammaTag as DNT

mandatoryChakraFields :: [Text]
mandatoryChakraFields = [userIdField]

userIdField :: Text
userIdField = "userId"

getUserIdsQueryName :: Text
getUserIdsQueryName = "getUserIds"

baseElementPatch :: A.Value
baseElementPatch =
  A.object
    [ "cat"
        A..= A.Array
          ( V.fromList
              [ A.object
                  [ "config"
                      A..= A.object
                        [ "var" A..= A.String "config"
                        ]
                  ],
                A.object
                  [ "identifier"
                      A..= A.object
                        [ "var" A..= A.String "identifier"
                        ]
                  ]
              ]
          )
    ]

decodeTextToValue :: Text -> Either String Value
decodeTextToValue text =
  let byteString = DTLE.encodeUtf8 $ DTE.fromStrict text
   in A.eitherDecode byteString

runJsonLogic :: (MonadFlow m) => Value -> Text -> m A.Value
runJsonLogic data' ruleText = do
  let eitherRule = decodeTextToValue ruleText
  rule <-
    case eitherRule of
      Right rule -> return rule
      Left err -> throwError $ InternalError ("Unable to decode rule:" <> show err)
  jsonLogic rule data'

runLogics :: (MonadFlow m, ToJSON a) => [A.Value] -> a -> m LYT.RunLogicResp
runLogics logics data_ = do
  let logicData = A.toJSON data_
  logDebug $ "logics- " <> show logics
  logDebug $ "logicData- " <> CS.cs (A.encode logicData)
  let startingPoint = LYT.RunLogicResp logicData []
  foldlM
    ( \acc logic -> do
        let result = jsonLogicEither logic acc.result
        res <-
          case result of
            Left err -> do
              logError $ "Got error: " <> show err <> " while running logic: " <> CS.cs (A.encode logics)
              pure $ LYT.RunLogicResp acc.result (acc.errors <> [show err])
            Right res -> pure $ LYT.RunLogicResp res acc.errors
        logDebug $ "logic- " <> (CS.cs . A.encode $ logic)
        logDebug $ "json logic result - " <> (CS.cs . A.encode $ res)
        return res
    )
    startingPoint
    logics

decodeText :: Text -> Maybe A.Value
decodeText txt = A.decode (DBL.fromStrict . DTE.encodeUtf8 $ txt)

-- Function to convert Text to Maybe Value
textToMaybeValue :: Text -> Maybe A.Value
textToMaybeValue txt =
  case decodeText txt of
    Just value -> Just value
    Nothing -> decodeText (T.concat ["\"", txt, "\""])

mkTagNameValueExpiry ::
  LYT.TagName ->
  LYT.TagValue ->
  Maybe Hours ->
  UTCTime ->
  LYT.TagNameValueExpiry
mkTagNameValueExpiry (LYT.TagName tagName) tagValue mbValidity now = do
  let mbExpiredAt = mbValidity <&> (\validity -> addUTCTime (3600 * fromIntegral validity) now)
  let showTagValue = case tagValue of
        LYT.TextValue tagValueText -> tagValueText
        LYT.NumberValue tagValueDouble -> show tagValueDouble
        LYT.ArrayValue tagValueArray -> T.intercalate "&" tagValueArray
  LYT.TagNameValueExpiry $ tagName <> "#" <> showTagValue <> maybe "" (\expiredAt -> "#" <> showNammaTagExpiry expiredAt) mbExpiredAt

mkTagNameValue ::
  LYT.TagName ->
  LYT.TagValue ->
  LYT.TagNameValue
mkTagNameValue (LYT.TagName tagName) tagValue = do
  let showTagValue = case tagValue of
        LYT.TextValue tagValueText -> tagValueText
        LYT.NumberValue tagValueDouble -> show tagValueDouble
        LYT.ArrayValue tagValueArray -> T.intercalate "&" tagValueArray
  LYT.TagNameValue $ tagName <> "#" <> showTagValue

addTagExpiry ::
  LYT.TagNameValue ->
  Maybe Hours ->
  UTCTime ->
  LYT.TagNameValueExpiry
addTagExpiry (LYT.TagNameValue txt) (Just validity) now = do
  let expiredAt = addUTCTime (3600 * fromIntegral validity) now
  LYT.TagNameValueExpiry $ case T.splitOn "#" txt of
    (tagName : tagValue : _oldExpiredAt : xs) -> T.intercalate "#" (tagName : tagValue : showNammaTagExpiry expiredAt : xs)
    [tagName, tagValue] -> T.intercalate "#" [tagName, tagValue, showNammaTagExpiry expiredAt]
    [tagName] -> T.intercalate "#" [tagName, "", showNammaTagExpiry expiredAt]
    [] -> T.intercalate "#" ["", "", showNammaTagExpiry expiredAt] -- should never happen
addTagExpiry (LYT.TagNameValue txt) Nothing _now = LYT.TagNameValueExpiry txt

parseTagExpiry :: LYT.TagNameValueExpiry -> Maybe UTCTime
parseTagExpiry (LYT.TagNameValueExpiry txt) = case T.splitOn "#" txt of
  _tagName : _tagValue : expiredAt : _xs -> parseTagExpiryTxt expiredAt
  _xs -> Nothing

parseTagExpiryTxt :: Text -> Maybe UTCTime
parseTagExpiryTxt expiredAt = Time.parseTimeM @Maybe True Time.defaultTimeLocale nammaTagExpiryFormat (T.unpack expiredAt)

-- ISO 8601
nammaTagExpiryFormat :: String
nammaTagExpiryFormat = "%Y-%m-%dT%H:%M:%S"

showNammaTagExpiry :: UTCTime -> Text
showNammaTagExpiry = T.pack . Time.formatTime Time.defaultTimeLocale nammaTagExpiryFormat

-- inverse conversion for mkTagNameValue
parseTagValueFromText :: HasTagNameValue tag => DNT.NammaTag -> tag -> Either Text LYT.TagValue
parseTagValueFromText tag txt = case T.splitOn "#" . (.getTagNameValue) $ convertToTagNameValue txt of
  _tagName : tagValue : _xs -> do
    case tag.possibleValues of
      LYT.Range _d1 _d2 -> do
        tagDouble <- mapLeft ("Couldn't parse double value: " <>) $ readEither @Text @Double tagValue
        pure $ LYT.NumberValue tagDouble
      _ -> case T.splitOn "&" tagValue of
        [tagValueText] -> pure $ LYT.TextValue tagValueText -- we can't separate text value from array containting one item
        [] -> Left "Tag value should not be empty"
        tagValueArray -> pure $ LYT.ArrayValue tagValueArray
  _ -> Left "Tag should have format tagName#tagValue of tagName#tagValue#expiredAt"

removeTagExpiry ::
  LYT.TagNameValueExpiry ->
  LYT.TagNameValue
removeTagExpiry (LYT.TagNameValueExpiry txt) = do
  LYT.TagNameValue $ case T.splitOn "#" txt of
    [tagName, tagValue, _oldExpiredAt] -> T.intercalate "#" [tagName, tagValue]
    (tagName : tagValue : _oldExpiredAt : xs) -> T.intercalate "#" (tagName : tagValue : "" : xs)
    _ -> txt

-- helper class for reduce boilerplate
class HasTagNameValue tag where
  convertToTagNameValue :: tag -> LYT.TagNameValue
  convertToRawText :: tag -> Text

instance HasTagNameValue LYT.TagNameValue where
  convertToTagNameValue = identity
  convertToRawText = LYT.getTagNameValue

instance HasTagNameValue LYT.TagNameValueExpiry where
  convertToTagNameValue = removeTagExpiry
  convertToRawText = LYT.getTagNameValueExpiry

compareTagNameValue :: (HasTagNameValue tag1, HasTagNameValue tag2) => tag1 -> tag2 -> Bool
compareTagNameValue tag1 tag2 = convertToTagNameValue tag1 == convertToTagNameValue tag2

showRawTags :: [LYT.TagNameValueExpiry] -> String
showRawTags = show

elemTagNameValue :: (HasTagNameValue tag1, HasTagNameValue tag2) => tag1 -> [tag2] -> Bool
elemTagNameValue tag tags = convertToTagNameValue tag `elem` (convertToTagNameValue <$> tags)

-- this way older value will be replaced to new one with upated expiry
replaceTagNameValue :: HasTagNameValue tag => Maybe [tag] -> tag -> [tag]
replaceTagNameValue Nothing tag = [tag]
replaceTagNameValue (Just tags) tag = removeTagNameValue (Just tags) tag ++ [tag]

removeTagNameValue :: (HasTagNameValue tag1, HasTagNameValue tag2) => Maybe [tag1] -> tag2 -> [tag1]
removeTagNameValue Nothing _ = []
removeTagNameValue (Just tags) tag = do
  let tag' = convertToTagNameValue tag
  filter ((/= tag') . convertToTagNameValue) tags

parseTagName :: (HasTagNameValue tag) => tag -> Maybe LYT.TagName
parseTagName tag = case T.splitOn "#" . (.getTagNameValue) $ convertToTagNameValue tag of
  (tagName : _) -> Just (LYT.TagName tagName)
  _ -> Nothing

-- used if we don't want fetch the same tag multiple times
fetchNammaTagValidity :: BeamFlow m r => LYT.TagName -> m (Maybe Hours)
fetchNammaTagValidity (LYT.TagName tagName) = runMaybeT $ do
  nammaTag <- MaybeT $ QNammaTag.findByPrimaryKey tagName
  MaybeT $ pure nammaTag.validity

fetchNammaTagExpiry :: BeamFlow m r => LYT.TagNameValue -> m LYT.TagNameValueExpiry
fetchNammaTagExpiry tagValue = do
  now <- getCurrentTime
  let defaulTagValueExpiry = addTagExpiry tagValue Nothing now
  mbTagValueExpiry <- runMaybeT $ do
    LYT.TagName tagName <- MaybeT $ pure (parseTagName tagValue)
    nammaTag <- MaybeT $ QNammaTag.findByPrimaryKey tagName
    validity <- MaybeT $ pure nammaTag.validity
    pure $ addTagExpiry tagValue (Just validity) now
  pure $ fromMaybe defaulTagValueExpiry mbTagValueExpiry

filterExpiredTags ::
  MonadTime m =>
  [LYT.TagNameValueExpiry] ->
  m [LYT.TagNameValueExpiry]
filterExpiredTags tags = (`filterExpiredTags'` tags) <$> getCurrentTime

filterExpiredTags' ::
  UTCTime ->
  [LYT.TagNameValueExpiry] ->
  [LYT.TagNameValueExpiry]
filterExpiredTags' now = filter $ \tagValue -> do
  case parseTagExpiry tagValue of
    Nothing -> True
    Just expiredAt -> expiredAt >= now

-- DSL transformers

tagsNameValueExpiryToTType :: Maybe [LYT.TagNameValueExpiry] -> Maybe [Text]
tagsNameValueExpiryToTType = (fmap (.getTagNameValueExpiry) <$>)

tagsNameValueExpiryFromTType :: MonadTime m => Maybe [Text] -> m (Maybe [LYT.TagNameValueExpiry])
tagsNameValueExpiryFromTType tagsText = do
  let tags = fmap LYT.TagNameValueExpiry <$> tagsText
  forM tags filterExpiredTags

tagsNameValueToTType :: Maybe [LYT.TagNameValue] -> Maybe [Text]
tagsNameValueToTType = (fmap (.getTagNameValue) <$>)

tagsNameValueFromTType :: Maybe [Text] -> Maybe [LYT.TagNameValue]
tagsNameValueFromTType = (fmap LYT.TagNameValue <$>)

convertTags :: HasTagNameValue tag => [tag] -> A.Value
convertTags input = A.object $ map toObject pairs
  where
    pairs = map (T.splitOn "#" . LYT.getTagNameValue . convertToTagNameValue) input
    toObject [name, value] = do
      let valueArr = T.splitOn "&" value
      case valueArr of
        [element] -> (A.fromText $ T.strip name :: A.Key) A..= fromMaybe A.Null (textToMaybeValue (T.strip element) :: Maybe A.Value)
        elements -> do
          let jsonValues = map A.String elements
          (A.fromText $ T.strip name :: A.Key) A..= A.Array (Vector.fromList jsonValues)
    toObject [name] = (A.fromText $ T.strip name :: A.Key) A..= A.Null
    toObject xs = do
      let reconstructed = T.intercalate "#" xs
      (A.fromText $ T.strip reconstructed :: A.Key) A..= A.Null

accessTagKey :: LYT.TagName -> A.Value -> Maybe A.Value
accessTagKey (LYT.TagName keyValue) (A.Object obj) = KM.lookup (A.fromText keyValue) obj
accessTagKey _ _ = Nothing

convertToTagObject :: HasTagNameValue tag => tag -> LYT.TagObject
convertToTagObject tag = case splitOnNonEmpty "#" $ convertToRawText tag of
  nameTxt :| [] -> mkTagObject nameTxt Nothing Nothing
  nameTxt :| [valueTxt] -> mkTagObject nameTxt (Just valueTxt) Nothing
  nameTxt :| (valueTxt : expiryTxt : _xs) -> mkTagObject nameTxt (Just valueTxt) (Just expiryTxt)
  where
    mkTagObject nameTxt mbValueTxt mbExpiryTxt = do
      let tagName = LYT.TagName nameTxt
      let tagValue = convertToTagValue <$> mbValueTxt
      let tagExpiry = parseTagExpiryTxt =<< mbExpiryTxt
      LYT.TagObject {..}

    convertToTagValue :: Text -> LYT.TagValue
    convertToTagValue txt = case splitOnNonEmpty "&" txt of
      element :| [] -> case readMaybe @Double (T.unpack element) of
        Just d -> LYT.NumberValue d
        Nothing -> LYT.TextValue element
      elements -> LYT.ArrayValue $ toList elements

splitOnNonEmpty :: Text -> Text -> NonEmpty Text
splitOnNonEmpty sep txt = case T.splitOn sep txt of
  (x : xs) -> x :| xs
  [] -> "" :| [] -- impossible case
