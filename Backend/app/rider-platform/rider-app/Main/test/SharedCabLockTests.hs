-- | The owner-token lock's Redis commands, run against the local Redis (localhost:6379).
module SharedCabLockTests (tests) where

import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import qualified Database.Redis as RawRedis
import Kernel.Prelude
import SharedLogic.SharedCab.Lock (acquireOwned, releaseOwned, waitUntil)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "SharedCabLock"
    [ testCase "a lock wait gives up once its deadline passes; a lock freed before it is taken" $ do
        let t0 = UTCTime (fromGregorian 2026 10 28) 0
            -- each pause moves the clock 1 s; the lock frees up at try `freeAt`
            run freeAt = do
              clock <- newIORef t0
              tries <- newIORef (0 :: Int)
              got <-
                waitUntil
                  (readIORef clock)
                  (addUTCTime 5 t0)
                  (modifyIORef tries (+ 1) >> (== freeAt) <$> readIORef tries)
                  (modifyIORef clock (addUTCTime 1))
              (got,) <$> readIORef tries
        held <- run 100
        freed <- run 3
        (held, freed) @?= ((False, 6), (True, 3)),
      testCase "a holder whose lock expired and was taken over does not release the new holder's lock" $ do
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
