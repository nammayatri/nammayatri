module Lib.Finance.Utils.SensitiveData
  ( maskCardNumber,
    maskTaxNo,
  )
where

import Data.Char (isDigit)
import qualified Data.Text as T
import Kernel.Prelude

-- | Mask a card number for audit logs: keep last 4 digits (PCI-friendly).
--   Passes through values already masked by the PG (contains X/x/*/•).
maskCardNumber :: Text -> Text
maskCardNumber raw
  | T.any isMaskChar raw = raw
  | otherwise =
    let digits = T.filter isDigit raw
        len = T.length digits
     in if len >= 4
          then "XXXX-XXXX-XXXX-" <> T.drop (len - 4) digits
          else "****"
  where
    isMaskChar c = c `elem` ("Xx*•" :: String)

-- | Mask a tax identifier (GSTIN, VAT number, PAN, etc.) for audit logs.
--   Hides at least 4 characters. Short values keep suffix only (PCI-style);
--   longer values add a growing prefix (up to 4 characters) for correlation
maskTaxNo :: Text -> Text
maskTaxNo taxNo
  | len <= 4 = "****"
  | len <= 8 = "****" <> takeSuffix (len - 4) taxNo
  | otherwise =
    let prefixLen = min 4 (len - 8)
     in T.take prefixLen taxNo <> "****" <> takeSuffix 4 taxNo
  where
    len = T.length taxNo
    takeSuffix n txt = T.drop (max 0 (T.length txt - n)) txt
