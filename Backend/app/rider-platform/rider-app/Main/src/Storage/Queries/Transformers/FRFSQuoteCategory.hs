module Storage.Queries.Transformers.FRFSQuoteCategory where

import Domain.Types.FRFSQuoteCategory
import Kernel.Prelude

mkQuoteCategoryMetadata :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Domain.Types.FRFSQuoteCategory.QuoteCategoryMetadata
mkQuoteCategoryMetadata mcode mtitle mdescription mtnc = do
  code <- mcode
  title <- mtitle
  description <- mdescription
  tnc <- mtnc
  Just Domain.Types.FRFSQuoteCategory.QuoteCategoryMetadata {code, title, description, tnc}
