{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Permit (module Reexport) where

import qualified Data.Text as T
import Lib.Permit.Storage.Beam.PermitApplicant as Reexport
import Lib.Permit.Storage.Beam.PermitApplication as Reexport
import Lib.Permit.Storage.Beam.PermitApplicationDocument as Reexport
import Lib.Permit.Storage.Beam.PermitApplicationEvent as Reexport
import Tools.Beam.UtilsTH (HasSchemaName (..), currentSchemaName)

instance HasSchemaName PermitApplicantT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName PermitApplicationT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName PermitApplicationDocumentT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName PermitApplicationEventT where
  schemaName _ = T.pack currentSchemaName
