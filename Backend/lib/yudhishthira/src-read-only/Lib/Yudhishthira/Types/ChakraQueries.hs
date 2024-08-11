{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Types.ChakraQueries where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Yudhishthira.Types
import qualified Tools.Beam.UtilsTH

data ChakraQueries = ChakraQueries
  { chakra :: Lib.Yudhishthira.Types.Chakra,
    id :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries,
    queryResults :: [Kernel.Prelude.Text],
    queryText :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
