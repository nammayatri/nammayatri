{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Control.Concurrent
import Foreign
import Foreign.C
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)

foreign import ccall "init_cac_clients" init_cac_clients :: CString -> CULong -> Bool -> Ptr CString -> CInt -> IO (Ptr CULong)

foreign import ccall "eval_ctx" eval_ctx :: CString -> CString -> IO (Ptr CChar)

foreign import ccall "&free_json_data" free_json_data :: FunPtr (Ptr CChar -> IO ())

foreign import ccall "init_superposition_clients" init_superposition_clients :: CString -> CULong -> Ptr CString -> CInt -> IO (Ptr CULong)

foreign import ccall "eval_experiment" eval_experiment :: CString -> CString -> CInt -> IO (Ptr CChar)

foreign import ccall "run_polling_updates" run_polling_updates :: IO ()

initCacClients :: CString -> CULong -> Bool -> Ptr CString -> CInt -> IO (ForeignPtr CULong)
initCacClients hostname polling_interval_secs update_cac_periodically tenants tenants_count = do
  resPtr <- init_cac_clients hostname polling_interval_secs update_cac_periodically tenants tenants_count
  newForeignPtr_ resPtr

initSuperPositionClients :: CString -> CULong -> Ptr CString -> CInt -> IO (ForeignPtr CULong)
initSuperPositionClients hostname polling_interval tenants tenants_count = do
  resPtr <- init_superposition_clients hostname polling_interval tenants tenants_count
  newForeignPtr_ resPtr

evalCtx :: CString -> CString -> IO (ForeignPtr CChar)
evalCtx tenant context = do
  resPtr <- eval_ctx tenant context
  freeJsonData resPtr

evalExperiment :: CString -> CString -> CInt -> IO (ForeignPtr CChar)
evalExperiment tenant context toss = do
  resPtr <- eval_experiment tenant context toss
  freeJsonData resPtr

freeJsonData :: Ptr CChar -> IO (ForeignPtr CChar)
freeJsonData ptr = do
  putStrLn "freeing json data"
  newForeignPtr free_json_data ptr

stringToCString :: String -> IO CString
stringToCString str = newCString str

printForeignPtr :: ForeignPtr CULong -> IO ()
printForeignPtr fptr = do
  let ptr = unsafeForeignPtrToPtr fptr
  culongValue <- peek ptr
  putStrLn $ "CULong value: " ++ show culongValue

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  arr1 <- mapM stringToCString ["mjos"]
  arr2 <- newArray arr1
  host <- stringToCString "http://localhost:8080"
  _ <- initCacClients host 10 True arr2 (1 :: CInt)
  _ <- initSuperPositionClients host 1 arr2 (1 :: CInt)
  _ <- forkIO run_polling_updates
  tenant <- stringToCString "mjos"
  putStrLn "evaluated context"
