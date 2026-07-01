{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Light-weight HTML to plain text conversion for chat bodies coming from
-- third-party desk integrations (e.g. Xyne Spaces @DESK_REPLY@ webhooks).
-- Preserves line breaks for @<br>@ and block-level boundaries, drops every
-- other tag, and decodes the common named entities. Not a full HTML parser —
-- intentionally — because chat bodies are short and well-formed in practice.
module IssueManagement.Utils.Html
  ( stripHtml,
  )
where

import qualified Data.Text as T
import Kernel.Prelude

stripHtml :: Text -> Text
stripHtml = decodeEntities . collapseBlanks . dropTags . normalizeBreaks
  where
    -- Convert line-breaking tags to literal newlines before dropping all tags
    -- so the visible structure of the reply survives.
    normalizeBreaks =
      T.replace "<br>" "\n"
        . T.replace "<br/>" "\n"
        . T.replace "<br />" "\n"
        . T.replace "</p>" "\n"
        . T.replace "</div>" "\n"
        . T.replace "</li>" "\n"

    dropTags = go
      where
        go t = case T.breakOn "<" t of
          (before, rest)
            | T.null rest -> before
            | otherwise -> case T.breakOn ">" rest of
              (_, after)
                | T.null after -> before
                | otherwise -> before <> go (T.drop 1 after)

    collapseBlanks =
      T.intercalate "\n"
        . dropTrailingEmpties
        . dropLeadingEmpties
        . map T.strip
        . T.splitOn "\n"
      where
        dropLeadingEmpties = dropWhile T.null
        dropTrailingEmpties = reverse . dropWhile T.null . reverse

    decodeEntities =
      T.replace "&nbsp;" " "
        . T.replace "&amp;" "&"
        . T.replace "&lt;" "<"
        . T.replace "&gt;" ">"
        . T.replace "&quot;" "\""
        . T.replace "&#39;" "'"
        . T.replace "&apos;" "'"
