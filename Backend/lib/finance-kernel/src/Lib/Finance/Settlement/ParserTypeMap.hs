module Lib.Finance.Settlement.ParserTypeMap
  ( allParserBasenames,
    parserMapAcceptsRemoteName,
    lookupParserTypeForRemoteName,
    resolveSplitCustomerType,
  )
where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.Text as T
import Kernel.External.Settlement.Types (SettlementParserTypeMap (..), SplitSettlementCustomerType (MERCHANT))
import Kernel.Prelude

allParserBasenames :: Maybe SettlementParserTypeMap -> [Text]
allParserBasenames = \case
  Nothing -> []
  Just (SettlementParserTypeMap m) -> concat (Map.elems m)

parserMapAcceptsRemoteName :: Maybe SettlementParserTypeMap -> Text -> Bool
parserMapAcceptsRemoteName mbMap fname
  | null (allParserBasenames mbMap) = True
  | otherwise = isJust (lookupParserTypeForRemoteName mbMap fname)

lookupParserTypeForRemoteName :: Maybe SettlementParserTypeMap -> Text -> Maybe SplitSettlementCustomerType
lookupParserTypeForRemoteName mbMap fname = case mbMap of
  Nothing -> Nothing
  Just (SettlementParserTypeMap m) ->
    listToMaybe $
      mapMaybe
        ( \(ty, needles) ->
            if any (\needle -> needle `T.isInfixOf` fname) needles
              then Just ty
              else Nothing
        )
        (Map.toList m)

resolveSplitCustomerType :: Maybe SettlementParserTypeMap -> Text -> SplitSettlementCustomerType
resolveSplitCustomerType mbMap fname =
  fromMaybe MERCHANT (lookupParserTypeForRemoteName mbMap fname)
