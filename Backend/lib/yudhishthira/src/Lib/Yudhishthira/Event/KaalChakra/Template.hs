module Lib.Yudhishthira.Event.KaalChakra.Template
  ( RawQuery (..),
    Template (..),
    replaceTemplateUnits,
  )
where

import Data.Char (toLower)
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types as Yudhishthira

--- Chakra Query Templates ---

data TemplateField = Limit | Offset | UsersSetCondition
  deriving (Show)

showTemplateField :: TemplateField -> Text
showTemplateField = (\f -> "{" <> f <> "}") . headToLower . show
  where
    headToLower :: Text -> Text
    headToLower txt = case T.uncons txt of
      Just (x, xs) -> T.cons (toLower x) xs
      Nothing -> ""

data IsMandatory = Mandatory | Optional

data TemplateUnit = TemplateUnit
  { templateField :: TemplateField,
    templateValue :: Text,
    isMandatory :: IsMandatory
  }

data Template = Template
  { limit :: Yudhishthira.QLimit,
    offset :: Yudhishthira.QOffset,
    usersSet :: Yudhishthira.UsersSet
  }

newtype RawQuery = RawQuery {getRawQuery :: Text}

replaceTemplateUnits :: RawQuery -> Template -> Either Text RawQuery
replaceTemplateUnits rawQuery Template {..} =
  foldlM
    replaceTemplateUnit
    rawQuery
    [ mkLimitUnit limit,
      mkOffsetUnit offset,
      mkUsersSetConditionUnit usersSet
    ]

mkLimitUnit :: Yudhishthira.QLimit -> TemplateUnit
mkLimitUnit limit = TemplateUnit Limit (show limit.getQLimit) Mandatory

mkOffsetUnit :: Yudhishthira.QOffset -> TemplateUnit
mkOffsetUnit offset = TemplateUnit Offset (show offset.getQOffset) Mandatory

mkUsersSetConditionUnit :: Yudhishthira.UsersSet -> TemplateUnit
mkUsersSetConditionUnit (Yudhishthira.SINGLE_USER (Id userId)) = TemplateUnit UsersSetCondition ("userId = '" <> userId <> "'") Mandatory
mkUsersSetConditionUnit (Yudhishthira.LIST_USERS userIds) = do
  let userList = T.intercalate ", " $ userIds <&> \(Id userId) -> "'" <> userId <> "'"
  TemplateUnit UsersSetCondition ("userId IN (" <> userList <> ")") Mandatory
mkUsersSetConditionUnit Yudhishthira.ALL_USERS = TemplateUnit UsersSetCondition "True" Mandatory

replaceTemplateUnit :: RawQuery -> TemplateUnit -> Either Text RawQuery
replaceTemplateUnit query templateUnit = do
  let templateField = showTemplateField templateUnit.templateField
  case templateUnit.isMandatory of
    Mandatory ->
      if T.isInfixOf templateField query.getRawQuery
        then Right $ RawQuery $ T.replace templateField templateUnit.templateValue query.getRawQuery
        else Left $ "Template field " <> templateField <> " is mandatory for query: '" <> query.getRawQuery <> "'; example: " <> templateExample
    Optional -> Right $ RawQuery $ T.replace templateField templateUnit.templateValue query.getRawQuery

templateExample :: Text
templateExample = "'SELECT driver_id as userId, count(id) :: int as cancellationsCount FROM atlas_driver_offer_bpp.ride WHERE status = 'CANCELLED' AND created_at >= date_sub (day, 1, toStartOfDay (now ())) AND created_at < toStartOfDay (now ()) AND {usersSetCondition} GROUP BY driver_id ORDER BY driver_id ASC LIMIT {limit} OFFSET {offset}'"
