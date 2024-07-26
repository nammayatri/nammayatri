{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.NammaTag where

import qualified Domain.Types.NammaTag
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Types

getChakra :: (Domain.Types.NammaTag.TagInfo -> Kernel.Prelude.Maybe Lib.Yudhishthira.Types.Chakra)
getChakra tag =
  case tag of
    Domain.Types.NammaTag.Application (Domain.Types.NammaTag.ApplicationTagInfo _) -> Kernel.Prelude.Nothing
    Domain.Types.NammaTag.KaalChakra (Domain.Types.NammaTag.KaalChakraTagInfo chakra _) -> Just chakra

getEvent :: (Domain.Types.NammaTag.TagInfo -> Kernel.Prelude.Maybe Lib.Yudhishthira.Types.ApplicationEvent)
getEvent tag =
  case tag of
    Domain.Types.NammaTag.Application (Domain.Types.NammaTag.ApplicationTagInfo event) -> Just event
    Domain.Types.NammaTag.KaalChakra (Domain.Types.NammaTag.KaalChakraTagInfo _ _) -> Kernel.Prelude.Nothing

getTag :: (Domain.Types.NammaTag.TagInfo -> Domain.Types.NammaTag.TagType)
getTag tag =
  case tag of
    Domain.Types.NammaTag.Application _ -> Domain.Types.NammaTag.ApplicationTag
    Domain.Types.NammaTag.KaalChakra _ -> Domain.Types.NammaTag.KaalChakraTag

getValidity :: (Domain.Types.NammaTag.TagInfo -> Kernel.Prelude.Maybe Kernel.Types.Common.Hours)
getValidity tag =
  case tag of
    Domain.Types.NammaTag.Application (Domain.Types.NammaTag.ApplicationTagInfo _) -> Kernel.Prelude.Nothing
    Domain.Types.NammaTag.KaalChakra (Domain.Types.NammaTag.KaalChakraTagInfo _ validity) -> validity

mkTagInfo :: (Kernel.Prelude.Maybe Lib.Yudhishthira.Types.Chakra -> Kernel.Prelude.Maybe Lib.Yudhishthira.Types.ApplicationEvent -> Domain.Types.NammaTag.TagType -> Kernel.Prelude.Maybe Kernel.Types.Common.Hours -> Domain.Types.NammaTag.TagInfo)
mkTagInfo chakra event tagType validity =
  case tagType of
    Domain.Types.NammaTag.ApplicationTag -> Domain.Types.NammaTag.Application (Domain.Types.NammaTag.ApplicationTagInfo (fromMaybe Lib.Yudhishthira.Types.RideEnd event))
    Domain.Types.NammaTag.KaalChakraTag -> Domain.Types.NammaTag.KaalChakra (Domain.Types.NammaTag.KaalChakraTagInfo (fromMaybe Lib.Yudhishthira.Types.Monthly chakra) validity)
