{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Extra.LocationInstance where

import qualified Domain.Types.Location (Location (..), LocationAddress (..))
import qualified Domain.Types.YudhishthiraTH as T
import Kernel.Prelude

$(T.generateAllDefault ''Domain.Types.Location.LocationAddress [])
$(T.generateAllDefault ''Domain.Types.Location.Location [])
