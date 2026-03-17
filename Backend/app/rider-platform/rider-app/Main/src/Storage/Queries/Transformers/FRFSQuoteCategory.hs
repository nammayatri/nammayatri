module Storage.Queries.Transformers.FRFSQuoteCategory where

import Domain.Types.FRFSQuoteCategory
import Kernel.Prelude

-- Used by FRFSTicketBookingPaymentCategory (no category_order column)
mkQuoteCategoryMetadata :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Domain.Types.FRFSQuoteCategory.QuoteCategoryMetadata
mkQuoteCategoryMetadata mcode mtitle mdescription mtnc = do
  code <- mcode
  title <- mtitle
  description <- mdescription
  tnc <- mtnc
  Just Domain.Types.FRFSQuoteCategory.QuoteCategoryMetadata {code, title, description, tnc, categoryOrder = Nothing}

-- Used by FRFSQuoteCategory which stores and sorts by category_order
mkQuoteCategoryMetadataWithOrder :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Domain.Types.FRFSQuoteCategory.QuoteCategoryMetadata
mkQuoteCategoryMetadataWithOrder mcode mtitle mdescription mtnc mcategoryOrder = do
  code <- mcode
  title <- mtitle
  description <- mdescription
  tnc <- mtnc
  Just Domain.Types.FRFSQuoteCategory.QuoteCategoryMetadata {code, title, description, tnc, categoryOrder = mcategoryOrder}
