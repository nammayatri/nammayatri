-- | The owner-token lock's Redis commands, run against the local Redis (localhost:6379).
module SharedCabLockTests (tests) where

import qualified Database.Redis as RawRedis
import Kernel.Prelude
import SharedLogic.SharedCab.Lock (acquireOwned, releaseOwned)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "SharedCabLock"
    [ testCase "a holder whose lock expired and was taken over does not release the new holder's lock" $ do
        conn <- RawRedis.checkedConnect RawRedis.defaultConnectInfo
        let key = "sharedcab:test:ownedLock"
            run :: RawRedis.Redis (Either RawRedis.Reply a) -> IO (Either RawRedis.Reply a)
            run = RawRedis.runRedis conn
            acquired owner = (== Right RawRedis.Ok) <$> run (acquireOwned key 10 owner)
        void $ run (RawRedis.del [key])
        acquired "A" >>= (@?= True)
        acquired "B" >>= (@?= False)
        void $ run (RawRedis.pexpire key 1)
        threadDelay 20000
        acquired "B" >>= (@?= True)
        run (releaseOwned key "A") >>= (@?= Right 0)
        acquired "C" >>= (@?= False)
        run (releaseOwned key "B") >>= (@?= Right 1)
        run (RawRedis.exists key) >>= (@?= Right False)
    ]
