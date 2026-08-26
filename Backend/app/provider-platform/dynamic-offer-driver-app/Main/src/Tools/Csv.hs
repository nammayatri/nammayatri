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

-- | Values that must carry a quote in a CSV export: those a spreadsheet would evaluate, and —
-- recursively — those that already look like the quoted form of such a value.
--
-- The recursion is what makes the encoding reversible. Quoting only formula-prefixed values makes
-- @"=x"@ and @"'=x"@ both encode to @"'=x"@, so the import side cannot tell a value the exporter
-- quoted from one the user actually typed. Counting the leading apostrophes distinguishes them:
-- @"=x"@ -> @"'=x"@ and @"'=x"@ -> @"''=x"@.
--
-- Note this is deliberately narrow. A value beginning with an apostrophe that is NOT followed by
-- a trigger — @'24x7' service@, @'Best' plan@ — is left completely alone, because there is
-- nothing to disambiguate. Only genuinely ambiguous values pay the extra character.
needsCsvQuoting :: Text -> Bool
needsCsvQuoting value =
  hasCsvFormulaPrefix value
    || maybe False needsCsvQuoting (T.stripPrefix "'" value)

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
  | needsCsvQuoting value = T.cons '\'' value
  | otherwise = value

-- | Exact inverse of 'sanitizeCsvField': drops a leading apostrophe only when sanitization is
-- what put it there. @desanitizeCsvField . sanitizeCsvField == id@ for every input, which the
-- unit tests pin.
--
-- Stripping unconditionally would corrupt values that legitimately begin with an apostrophe:
-- @'24x7' service@ is never quoted on export, yet would come back as @24x7' service@, losing a
-- character on every round trip. Matching only on a formula-like remainder — the previous
-- behaviour — fixed that but still corrupted @'-5@ into @-5@ and @'=cmd()@ into @=cmd()@, which
-- silently rewrote stored values and, worse, turned an inert quoted string back into a live
-- formula. Deferring to 'needsCsvQuoting' removes both failure modes.
--
-- The spreadsheet leg of the trip remains outside our control: Excel and Sheets treat a leading
-- apostrophe as a text-format marker and drop it when re-saving, so a value that made the round
-- trip through a spreadsheet may arrive already unquoted. That is handled — with nothing to
-- strip, this is the identity — but a value the user typed as @'=x@ cannot survive a spreadsheet
-- edit, because the spreadsheet itself discards the apostrophe before we ever see the file.
desanitizeCsvField :: Text -> Text
desanitizeCsvField value =
  case T.stripPrefix "'" value of
    Just rest | needsCsvQuoting rest -> rest
    _ -> value

cleanField :: Text -> Maybe Text
cleanField = replaceEmpty . T.strip

replaceEmpty :: Text -> Maybe Text
replaceEmpty = \case
  "" -> Nothing
  "no constraint" -> Nothing
  "no_constraint" -> Nothing
  x -> Just x
