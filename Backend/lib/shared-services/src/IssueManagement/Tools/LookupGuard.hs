{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Guards for the in-house chat lookup engine ("ApiCall" flow steps).
--
-- A lookup is an internal dashboard call made with the engine's own service
-- token, so two things must hold for every call:
--
-- 1. The request stays inside the merchant/city dashboard tree. The author's
--    template is parsed into path segments and query pairs, never used as a
--    raw string: segments are restricted to a safe charset (no "..", "//",
--    "%", "#", "\\"), placeholders may only be the engine's built-ins and
--    fill a whole segment or query value, and query values are URL-encoded
--    when rendered so they cannot add keys or change the path.
--
-- 2. The endpoint actually reached is whitelisted for the city. That is
--    decided on the receiving side, where the router has already resolved
--    the path to an endpoint id: 'checkLookupAllowed' compares that id
--    against issue_config.allowed_lookup_endpoints.
module IssueManagement.Tools.LookupGuard
  ( lookupBuiltinKeys,
    validateLookupTemplate,
    renderLookupTemplate,
    checkLookupAllowed,
  )
where

import qualified Data.Char as C
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import EulerHS.Prelude
import qualified Network.HTTP.Types as HttpTypes

-- | The only placeholders a lookup template may use. They are filled by the
-- engine from the issue being reported, never from earlier lookup responses
-- or user input, so a lookup is always about the reporter's own ride/profile.
lookupBuiltinKeys :: [Text]
lookupBuiltinKeys = ["personId", "categoryId", "rideId", "merchantShortId", "city", "merchantOperatingCityId"]

data Piece = Lit Text | Var Text

data ParsedTemplate = ParsedTemplate
  { pathPieces :: [Piece],
    queryPieces :: [(Text, Piece)]
  }

-- | Letters, digits, '-' and '_' only. Rules out "..", "/", "%", "?", "#"
-- and every other character that could change where a request goes.
isSafeToken :: Text -> Bool
isSafeToken s = not (T.null s) && T.all (\c -> C.isAsciiUpper c || C.isAsciiLower c || C.isDigit c || c == '-' || c == '_') s

parsePiece :: Text -> Either Text Piece
parsePiece t
  | Just inner <- T.stripPrefix "{{" t >>= T.stripSuffix "}}" =
    if inner `elem` lookupBuiltinKeys
      then Right (Var inner)
      else Left $ "Unknown placeholder {{" <> inner <> "}}; lookups may use only " <> T.intercalate ", " lookupBuiltinKeys
  | "{{" `T.isInfixOf` t || "}}" `T.isInfixOf` t =
    Left $ "A placeholder must fill a whole path segment or query value: " <> t
  | otherwise = Right (Lit t)

parseTemplate :: Text -> Either Text ParsedTemplate
parseTemplate template = do
  when (T.null template) $ Left "Lookup path cannot be empty"
  when (T.any (\c -> c == '#' || c == '\\' || C.isSpace c || C.isControl c) template) $
    Left "Lookup path cannot contain '#', '\\', spaces or control characters"
  let (pathPart, rest) = T.breakOn "?" template
      queryPart = T.drop 1 rest
  when ("/" `T.isPrefixOf` pathPart || "http" `T.isPrefixOf` T.toLower pathPart) $
    Left "Give only the endpoint path after /direct-dashboard/{merchant}/{city}/, e.g. ride/rideinfo/{{rideId}}"
  pathPieces <- forM (T.splitOn "/" pathPart) $ \segment -> do
    piece <- parsePiece segment
    case piece of
      Lit lit | not (isSafeToken lit) -> Left $ "Path segment not allowed: '" <> lit <> "' (letters, digits, '-' and '_' only; no empty segments)"
      _ -> Right piece
  queryPieces <-
    if T.null queryPart
      then Right []
      else forM (T.splitOn "&" queryPart) $ \pair -> do
        let (key, value) = T.breakOn "=" pair
        unless (isSafeToken key) $ Left $ "Query key not allowed: '" <> key <> "'"
        piece <- parsePiece (T.drop 1 value)
        Right (key, piece)
  Right ParsedTemplate {..}

-- | Upsert-time check: the template is well-formed and uses only built-ins.
validateLookupTemplate :: Text -> Either Text ()
validateLookupTemplate = void . parseTemplate

-- | Render the endpoint tail (path plus encoded query) from the template and
-- the engine's built-in values. Path values must themselves be safe tokens;
-- query values are percent-encoded, so "a&limit=1" stays one value.
renderLookupTemplate :: Map.Map Text Text -> Text -> Either Text Text
renderLookupTemplate builtins template = do
  ParsedTemplate {..} <- parseTemplate template
  let resolve = \case
        Lit lit -> Right lit
        Var var -> maybe (Left $ "{{" <> var <> "}} is not available for this issue") Right (Map.lookup var builtins)
  pathSegments <- forM pathPieces $ \piece -> do
    value <- resolve piece
    unless (isSafeToken value) $ Left $ "Value not allowed in a path segment: '" <> value <> "'"
    Right value
  queryPairs <- forM queryPieces $ \(key, piece) -> (key,) <$> resolve piece
  let query = TE.decodeUtf8 $ HttpTypes.renderQuery True [(TE.encodeUtf8 k, Just (TE.encodeUtf8 v)) | (k, v) <- queryPairs]
  Right (T.intercalate "/" pathSegments <> query)

-- | Receiving-side check for a lookup call: the endpoint the router resolved
-- must be listed in issue_config.allowed_lookup_endpoints for the city.
checkLookupAllowed :: Maybe [Text] -> Text -> Either Text ()
checkLookupAllowed allowed endpointId
  | endpointId `elem` fromMaybe [] allowed = Right ()
  | otherwise = Left $ "Endpoint " <> endpointId <> " is not allowed for chat lookups in this city"
