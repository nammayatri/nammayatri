var loaderFiber = undefined;

export const storeLoaderFiber = function (fiber) {
    loaderFiber = fiber;
  }
  
  export const getLoaderFiber = function (just) {
    return function(nothing) {
        if (loaderFiber != undefined) {
            return just(loaderFiber);
        } else {
            return nothing;
        }
    }
  }