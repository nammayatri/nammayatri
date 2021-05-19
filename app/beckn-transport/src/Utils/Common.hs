{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Utils.Common
  ( module Utils.Common,
    module CoreCommon,
  )
where

import App.Types
import Beckn.Utils.Common as CoreCommon
import qualified Crypto.Number.Generate as Cryptonite
import Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude

generateOTPCode :: Flow Text
generateOTPCode =
  L.runIO $ padNumber 4 <$> Cryptonite.generateBetween 1 9999
