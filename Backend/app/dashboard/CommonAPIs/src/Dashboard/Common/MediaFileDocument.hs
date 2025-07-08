module Dashboard.Common.MediaFileDocument
  ( MediaFileDocument,
    MediaFileDocumentStatus (..),
  )
where

import Kernel.Prelude

data MediaFileDocument

data MediaFileDocumentStatus
  = PENDING
  | DELETED
  | FAILED
  | CONFIRMED
  | COMPLETED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
