module BecknV2.FRFS.Utils where

import qualified BecknV2.FRFS.Types as Spec
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.Common

tfDescriptor :: Maybe Text -> Maybe Text -> Maybe Spec.Descriptor
tfDescriptor mCode mName = do
  name <- mCode
  code <- mName
  return
    Spec.Descriptor
      { descriptorCode = Just $ code,
        descriptorImages = Nothing,
        descriptorName = Just $ name
      }

parseMoney :: Spec.Price -> Maybe HighPrecMoney
parseMoney price =
  price.priceValue >>= (readMaybe . T.unpack)
    >>= return . HighPrecMoney
