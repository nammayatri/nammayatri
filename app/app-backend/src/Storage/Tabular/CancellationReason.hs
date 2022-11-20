{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.CancellationReason where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import qualified Domain.Types.CancellationReason as Domain

derivePersistField "Domain.CancellationStage"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    CancellationReasonT sql=cancellation_reason
      reasonCode Text
      description Text
      enabled Bool
      onSearch Bool
      onConfirm Bool
      onAssign Bool
      priority Int
      Primary reasonCode
      deriving Generic
    |]

instance TEntityKey CancellationReasonT where
  type DomainKey CancellationReasonT = Domain.CancellationReasonCode
  fromKey (CancellationReasonTKey _id) = Domain.CancellationReasonCode _id
  toKey (Domain.CancellationReasonCode id) = CancellationReasonTKey id

instance TType CancellationReasonT Domain.CancellationReason where
  fromTType CancellationReasonT {..} = do
    return $
      Domain.CancellationReason
        { reasonCode = Domain.CancellationReasonCode reasonCode,
          ..
        }
  toTType Domain.CancellationReason {..} =
    CancellationReasonT
      { reasonCode = let (Domain.CancellationReasonCode rc) = reasonCode in rc,
        ..
      }
