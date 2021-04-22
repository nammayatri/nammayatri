module Types.Error
  ( module Types.Error,
    module Beckn.Types.Error,
  )
where

import Beckn.Types.Error
import EulerHS.Prelude

data ErrorCode
  = CORE001
  | CORE002
  | CORE003
  | CORE004
  | CORE005
  | CORE006
  | CORE007
  | CORE008
  | CORE009
  | CORE010
  | FMD001
  | FMD002
  | FMD003
  | FMD004
  | FMD005
  | FMD006
  | FMD007
  | FMD008
  | FMD009
  | FMD010
  | FMD011
  | FMD012
  | FMD013
  | FMD014
  | FMD015
  | FMD016
  deriving (Generic, Show, FromJSON, ToJSON)
