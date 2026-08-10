module Tools.Csv where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Csv
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Utils.Common
import Tools.Error

readCsv ::
  forall csv domain m.
  (FromNamedRecord csv, MonadThrow m, Log m, L.MonadFlow m) =>
  FilePath ->
  (Int -> csv -> m domain) ->
  m [domain]
readCsv csvFile parseFunc = do
  csvData <- L.runIO $ BS.readFile csvFile
  case (decodeByName $ LBS.fromStrict csvData :: Either String (Header, V.Vector csv)) of
    Left err -> throwError (InvalidRequest $ show err)
    Right (_, v) -> V.imapM parseFunc v >>= (pure . V.toList)

readCSVField :: forall a m. (Read a, MonadThrow m, Log m) => Int -> Text -> Text -> m a
readCSVField idx fieldValue fieldName =
  cleanField fieldValue >>= readMaybe . T.unpack & fromMaybeM (InvalidRequest $ "Invalid " <> fieldName <> ": " <> show fieldValue <> " at row: " <> show idx)

cleanCSVField :: forall m. (MonadThrow m, Log m) => Int -> Text -> Text -> m Text
cleanCSVField idx fieldValue fieldName =
  cleanField fieldValue & fromMaybeM (InvalidRequest $ "Invalid " <> fieldName <> ": " <> show fieldValue <> " at row: " <> show idx)

cleanMaybeCSVField :: Int -> Text -> Text -> Maybe Text
cleanMaybeCSVField _ fieldValue _ = cleanField fieldValue

readMaybeCSVField :: forall a. Read a => Int -> Text -> Text -> Maybe a
readMaybeCSVField _ fieldValue _ = cleanField fieldValue >>= readMaybe . T.unpack

-- | Leading characters that make a spreadsheet treat a cell as a formula rather than as text.
-- Leading whitespace (including tab and CR) is stripped before this check, because a spreadsheet
-- ignores it too: "\t=cmd()" evaluates exactly as "=cmd()" does.
csvFormulaTriggers :: [Char]
csvFormulaTriggers = ['=', '+', '-', '@']

-- | True when a spreadsheet would evaluate this value instead of displaying it. Use at input
-- validation time to reject values that have no business reason to look like formulas.
hasCsvFormulaPrefix :: Text -> Bool
hasCsvFormulaPrefix value =
  case T.uncons (T.stripStart value) of
    Just (c, _) -> c `elem` csvFormulaTriggers
    Nothing -> False

-- | Neutralize a value for inclusion in a CSV export by prefixing a single quote, which makes
-- spreadsheets treat the cell as literal text. Apply to every user-controlled field on the way
-- out; validating on the way in is not sufficient on its own, because rows can predate the
-- validation or arrive through other write paths.
--
-- IMPORTANT: any CSV that is a round-trip format (exported, edited, re-uploaded) must apply
-- 'desanitizeCsvField' on the import side, or values that were quoted on the way out will fail
-- to parse on the way back in. Note that '-' is a trigger, so this affects every negative number.
sanitizeCsvField :: Text -> Text
sanitizeCsvField value
  | hasCsvFormulaPrefix value = T.cons '\'' value
  | otherwise = value

-- | Undo 'sanitizeCsvField' on the import side of a round-trip CSV: drop a leading apostrophe,
-- but only where sanitize could have added one — that is, only when what follows would itself
-- have been treated as a formula. Stripping unconditionally corrupts values that legitimately
-- begin with an apostrophe: @'24x7' service@ is never quoted on export, yet would come back as
-- @24x7' service@, losing a character on every round trip.
--
-- Safe against both shapes of round trip. A spreadsheet treats the apostrophe as a text-format
-- marker and omits it when re-saving to CSV, in which case there is nothing to strip and this is
-- the identity. A file re-uploaded verbatim still carries it, and this removes it. Without this,
-- "'-5" reaches readMaybe, yields Nothing, and the field is silently dropped.
--
-- Not a true inverse, and cannot be: 'sanitizeCsvField' is not injective, since @"=x"@ and
-- @"'=x"@ both encode to @"'=x"@. A value that literally starts with an apostrophe followed by
-- @=@, @+@, @-@ or @\@@ therefore loses that apostrophe on round trip. Escaping the apostrophe
-- by doubling it would restore injectivity but breaks the spreadsheet path, which silently eats
-- one apostrophe of its own. The ambiguity is accepted; the realistic cases round-trip exactly.
desanitizeCsvField :: Text -> Text
desanitizeCsvField value =
  case T.stripPrefix "'" value of
    Just rest | hasCsvFormulaPrefix rest -> rest
    _ -> value

cleanField :: Text -> Maybe Text
cleanField = replaceEmpty . T.strip

replaceEmpty :: Text -> Maybe Text
replaceEmpty = \case
  "" -> Nothing
  "no constraint" -> Nothing
  "no_constraint" -> Nothing
  x -> Just x
