module Epass.Types.API.Document where

import Epass.Types.Storage.Document
import EulerHS.Prelude

newtype DocumentRes = DocumentRes
  { docIds :: [Text]
  }
  deriving (Generic, FromJSON, ToJSON)

data ListDocumentRes = ListDocumentRes
  { docId :: Text,
    fileName :: Text
  }
  deriving (Generic, FromJSON, ToJSON)
