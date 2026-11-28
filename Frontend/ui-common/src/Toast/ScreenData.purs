module Toast.ScreenData where

initData :: ToastState
initData = {
    data : {
        title : ""
    ,   message : ""
    }
}

type ToastState
  = { data :: ToastData
    }

type ToastData
  = { title :: String
    , message :: String
    }