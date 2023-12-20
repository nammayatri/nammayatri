{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Common.Tags
  ( module Beckn.Types.Core.Taxi.Common.Tags,
  )
where

import Beckn.Types.Core.Taxi.Common.Descriptor -- 2.x
import Data.Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AKM
import Data.Char (isDigit)
import Data.List (nub)
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import EulerHS.Prelude hiding (State)
import qualified Kernel.Prelude as P hiding (show)

data TagGroupV2 = TagGroupV2
  { display :: Bool,
    descriptor :: DescriptorV2, -- 2.x
    -- code :: Text, -- To be moved in descriptor in 2.x
    -- name :: Text, -- To be moved in descriptor in 2.x
    list :: [TagV2]
  }
  deriving (Generic, Show, ToSchema, FromJSON, ToJSON)

-- instance FromJSON TagGroup where
--   parseJSON = genericParseJSON removeNullFields

-- instance ToJSON TagGroup where
--   toJSON = genericToJSON removeNullFields

data TagV2 = TagV2
  { display :: Maybe Bool,
    descriptor :: Maybe DescriptorV2, -- 2.x
    -- code :: Maybe Text, -- To be moved in descriptor in 2.x
    -- name :: Maybe Text, -- To be moved in descriptor in 2.x
    value :: Maybe Text
  }
  deriving (Generic, Show, ToSchema, FromJSON, ToJSON)

-- instance FromJSON Tag where
--   parseJSON = genericParseJSON removeNullFields

-- instance ToJSON Tag where
--   toJSON = genericToJSON removeNullFields

---------------------------------------- TagGroups not required in 2.x ---------------------------------

data TagGroup = TagGroup
  { display :: Bool,
    code :: Text,
    name :: Text,
    list :: [Tag]
  }
  deriving (Generic, Show, ToSchema, FromJSON, ToJSON)

data Tag = Tag
  { display :: Maybe Bool,
    code :: Maybe Text,
    name :: Maybe Text,
    value :: Maybe Text
  }
  deriving (Generic, Show, ToSchema, FromJSON, ToJSON)

newtype TagGroups = TG [TagGroup] deriving (Show, Generic, ToSchema)

instance ToJSON TagGroups where
  toJSON (TG abc) = Object $ AKM.fromList $ map convertKey (convertToList abc)
    where
      convertKey :: (String, Value) -> (AesonKey.Key, Value)
      convertKey (key, value) = (AesonKey.fromText $ T.pack key, value)

instance FromJSON TagGroups where
  parseJSON (Object obj) = return $ extractTagGroups obj
  parseJSON _ = fail "Object TagGroups"

convertToList :: [TagGroup] -> [(String, Value)]
convertToList tagGroups = P.concatMap processTagGroup (zip [1 ..] tagGroups)
  where
    processTagGroup :: (Int, TagGroup) -> [(String, Value)]
    processTagGroup (groupIndex, tagGroup) =
      [ ("groups/" ++ show groupIndex ++ "/display", parseDisplay tagGroup.display),
        ("groups/" ++ show groupIndex ++ "/code", toJSON tagGroup.code),
        ("groups/" ++ show groupIndex ++ "/name", toJSON tagGroup.name)
      ]
        ++ processTags groupIndex (filter (\lst -> not $ isNothing lst.display && isNothing lst.code && isNothing lst.name && isNothing lst.value) tagGroup.list)

    processTags :: Int -> [Tag] -> [(String, Value)]
    processTags groupIndex tags = P.concatMap (processTag groupIndex) (zip [1 ..] tags)

    parseMaybeDisplay :: Maybe Bool -> Value
    parseMaybeDisplay Nothing = String ""
    parseMaybeDisplay (Just val) = parseDisplay val

    parseDisplay :: Bool -> Value
    parseDisplay True = String "true"
    parseDisplay False = String "false"

    processTag :: Int -> (Int, Tag) -> [(String, Value)]
    processTag groupIndex (listIndex, tag) =
      [ ("groups/" ++ show groupIndex ++ "/list/" ++ show listIndex ++ "/display", parseMaybeDisplay tag.display),
        ("groups/" ++ show groupIndex ++ "/list/" ++ show listIndex ++ "/code", toJSON tag.code),
        ("groups/" ++ show groupIndex ++ "/list/" ++ show listIndex ++ "/name", toJSON tag.name),
        ("groups/" ++ show groupIndex ++ "/list/" ++ show listIndex ++ "/value", toJSON tag.value)
      ]

extractTagGroups :: Object -> TagGroups
extractTagGroups obj =
  let groupIndices = sort $ getGroupIndices obj
   in TG $ map (extractTagGroup obj) groupIndices

getGroupIndices :: Object -> [Int]
getGroupIndices = nub . mapMaybe (extractInteger . AesonKey.toText) . AKM.keys
  where
    extractInteger :: T.Text -> Maybe Int
    extractInteger key =
      case T.stripPrefix "groups/" key of
        Just rest -> readMaybe (T.unpack $ T.takeWhile isDigit rest)
        Nothing -> Nothing

extractTagGroup :: Object -> Int -> TagGroup
extractTagGroup obj groupIndex =
  let groupKey = "groups/" ++ show groupIndex ++ "/"
      displayValue = extractStringValue obj (groupKey ++ "display")
      codeValue = extractStringValue obj (groupKey ++ "code")
      nameValue = extractStringValue obj (groupKey ++ "name")
      listValues = filter (\lst -> not $ isNothing lst.display && isNothing lst.code && isNothing lst.name && isNothing lst.value) (extractTagList obj groupIndex)
   in TagGroup
        { display = T.toLower (T.pack displayValue) == "true",
          code = T.pack codeValue,
          name = T.pack nameValue,
          list = listValues
        }

extractTagList :: Object -> Int -> [Tag]
extractTagList obj groupIndex =
  let listKey = "groups/" ++ show groupIndex ++ "/list"
      listIndices = sort $ nub $ getTagListIndices obj groupIndex
   in map (extractTag obj (T.pack listKey)) listIndices

getTagListIndices :: Object -> Int -> [Int]
getTagListIndices obj groupIndex =
  let listKey = "groups/" ++ show groupIndex ++ "/list"
      indices = (mapMaybe (extractInteger . AesonKey.toText) . AKM.keys) obj
   in filter (isTagListIndex obj (T.pack listKey)) indices
  where
    extractInteger :: T.Text -> Maybe Int
    extractInteger key = do
      let listKey = T.pack $ "groups/" ++ show groupIndex ++ "/list"
      case T.stripPrefix listKey key >>= T.stripPrefix "/" of
        Just rest -> readMaybe (T.unpack $ T.takeWhile isDigit rest)
        Nothing -> Nothing

isTagListIndex :: Object -> T.Text -> Int -> Bool
isTagListIndex obj listKey idx =
  let listIdxKey = listKey <> "/" <> T.pack (show idx)
      hasDisplay = AKM.member (AesonKey.fromText (listIdxKey <> "/display")) obj
      hasCode = AKM.member (AesonKey.fromText (listIdxKey <> "/code")) obj
      hasName = AKM.member (AesonKey.fromText (listIdxKey <> "/name")) obj
      hasValue = AKM.member (AesonKey.fromText (listIdxKey <> "/value")) obj
   in hasDisplay && hasCode && hasName && hasValue

extractTag :: Object -> T.Text -> Int -> Tag
extractTag obj listKey listIndex =
  let tagIdxKey = listKey <> "/" <> T.pack (show listIndex)
      nameValue = extractStringValue obj (T.unpack $ tagIdxKey <> "/name")
      codeValue = extractStringValue obj (T.unpack $ tagIdxKey <> "/code")
      valueValue = extractStringValue obj (T.unpack $ tagIdxKey <> "/value")
      displayValue = extractStringValue obj (T.unpack $ tagIdxKey <> "/display")
   in Tag
        { name = evalString nameValue,
          code = evalString codeValue,
          value = evalString valueValue,
          display = readBool displayValue
        }

extractStringValue :: Object -> String -> String
extractStringValue obj key =
  case AKM.lookup (AesonKey.fromText $ T.pack key) obj of
    Just (String value) -> T.unpack value
    _ -> ""

evalString :: String -> Maybe Text
evalString str = do
  let strVal = T.toLower . T.strip $ T.pack str
  if strVal == "" || strVal == "null" then Nothing else Just (T.pack str)

readBool :: String -> Maybe Bool
readBool "" = Nothing
readBool "true" = Just True
readBool _ = Just False
