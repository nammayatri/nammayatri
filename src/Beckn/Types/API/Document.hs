module Beckn.Types.API.Document where

import EulerHS.Prelude

data UpdateDocumentRes =
  UpdateDocumentRes
    { docIds :: [Text]
    } deriving (Generic, ToJSON)
