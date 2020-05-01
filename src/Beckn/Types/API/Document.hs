module Beckn.Types.API.Document where

import EulerHS.Prelude
import Beckn.Types.Storage.Document

data DocumentRes =
  DocumentRes
    { docIds :: [Text]
    } deriving (Generic, ToJSON)

data ListDocumentRes =
  ListDocumentRes
    { documents :: [Document]
    } deriving (Generic, ToJSON)
