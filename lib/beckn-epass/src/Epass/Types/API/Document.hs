module Epass.Types.API.Document where

import Epass.Types.Storage.Document
import EulerHS.Prelude

data DocumentRes = DocumentRes
  { docIds :: [Text]
  }
  deriving (Generic, ToJSON)

data ListDocumentRes = ListDocumentRes
  { docId :: Text,
    fileName :: Text
  }
  deriving (Generic, ToJSON)
