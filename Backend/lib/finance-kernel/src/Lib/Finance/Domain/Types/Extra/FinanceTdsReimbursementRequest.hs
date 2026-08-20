module Lib.Finance.Domain.Types.Extra.FinanceTdsReimbursementRequest where

import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (Log)
import Kernel.Utils.Error (throwError)
import Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest (AssessmentYear (..))

mkAssessmentYear :: (MonadThrow m, Log m) => Text -> m AssessmentYear
mkAssessmentYear = fmap fst . mkAssessmentYearWithFyStartYear

-- | Like 'mkAssessmentYear', but also returns the start year of the financial year the
-- assessment year covers: AY "2024-25" assesses FY 2023-24, so its FY start year is 2023.
mkAssessmentYearWithFyStartYear :: (MonadThrow m, Log m) => Text -> m (AssessmentYear, Integer)
mkAssessmentYearWithFyStartYear raw = case T.splitOn "-" raw of
  [startText, endText]
    | T.length startText == 4,
      T.length endText == 2,
      Just start <- readMaybe (T.unpack startText) :: Maybe Integer,
      Just end <- readMaybe (T.unpack endText) :: Maybe Integer,
      end == (start + 1) `mod` 100 ->
      pure (AssessmentYear raw, start - 1)
  _ -> throwError $ InvalidRequest $ "Invalid assessment year \"" <> raw <> "\", expected format YYYY-YY, e.g. 2024-25"
