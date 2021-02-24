module Product.CallsTrack where

import App.Types
import qualified Beckn.Types.Storage.Organization as Org
import Control.Lens (Getter)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import GHC.IO.Unsafe (unsafePerformIO)

-- Solely for testing purposes
data CallsTrack = CallsTrack
  { orderConfirms :: Map Text OrderInfo,
    lastOrderId :: Maybe Text
  }
  deriving (Generic)

newtype OrderInfo = OrderInfo
  { bppOrg :: Org.Organization
  }
  deriving (Generic)

callsTrack :: TVar CallsTrack
callsTrack = unsafePerformIO . newTVarIO $ CallsTrack mempty Nothing
{-# NOINLINE callsTrack #-}

readingCallsTrack :: Getter CallsTrack a -> Flow a
readingCallsTrack getter =
  fmap (view getter) $ L.runIO $ readTVarIO callsTrack

updateCallsTrack :: State CallsTrack () -> Flow ()
updateCallsTrack st =
  L.runIO . atomically $ modifyTVar callsTrack (execState st)
