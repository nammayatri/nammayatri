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
-- Tab and carriage return are included because Excel strips them before deciding, so "\t=cmd()"
-- evaluates just as "=cmd()" does.
csvFormulaTriggers :: [Char]
csvFormulaTriggers = ['=', '+', '-', '@', '\t', '\r']

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
sanitizeCsvField :: Text -> Text
sanitizeCsvField value
  | hasCsvFormulaPrefix value = T.cons '\'' value
  | otherwise = value

cleanField :: Text -> Maybe Text
cleanField = replaceEmpty . T.strip

replaceEmpty :: Text -> Maybe Text
replaceEmpty = \case
  "" -> Nothing
  "no constraint" -> Nothing
  "no_constraint" -> Nothing
  x -> Just x
