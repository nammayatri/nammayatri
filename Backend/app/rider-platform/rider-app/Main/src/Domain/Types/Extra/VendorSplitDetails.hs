{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.Extra.VendorSplitDetails where

import Data.Aeson
import qualified Data.List as List
import qualified Data.Text as T
import Kernel.Prelude
import qualified Text.Show
import qualified Tools.Beam.UtilsTH as UtilsTH

data SplitShare = Percentage Kernel.Prelude.Double | FixedValue Kernel.Prelude.Int deriving (Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

instance Show SplitShare where
  show (Percentage percentage) = "PERCENTAGE_" <> T.unpack (show percentage)
  show (FixedValue fixedValue) = "FIXEDVALUE_" <> T.unpack (show fixedValue)

instance Read SplitShare where
  readsPrec d' =
    readParen
      (d' > app_prec)
      ( \r ->
          [ (Percentage (read r1 :: Kernel.Prelude.Double), "")
            | r1 <- stripPrefix "PERCENTAGE_" r
          ]
            ++ [ (FixedValue (read r1 :: Kernel.Prelude.Int), "")
                 | r1 <- stripPrefix "FIXEDVALUE_" r
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [List.drop (length pref) r] $ List.isPrefixOf pref r

$(UtilsTH.mkBeamInstancesForEnumAndList ''SplitShare)
