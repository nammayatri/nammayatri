{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Payment where

import qualified Data.Text as T
import qualified Lib.Payment.Storage.Beam.PaymentOrder as BeamPO
import qualified Lib.Payment.Storage.Beam.PaymentTransaction as BeamPT
import Tools.Beam.UtilsTH (HasSchemaName (..), currentSchemaName)

instance HasSchemaName BeamPO.PaymentOrderT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamPT.PaymentTransactionT where
  schemaName _ = T.pack currentSchemaName
