{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.KnowlarityTest where

import Data.Aeson
import Kernel.Prelude
import qualified Tools.Beam.UtilsTH

data KnowlarityTest = KnowlarityTest {callFrom :: Kernel.Prelude.Text, callTo :: Kernel.Prelude.Text, description :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
