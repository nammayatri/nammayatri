export const swapElements = function (i, j, arr) {
  if (i >= 0 && i < arr.length && j >= 0 && j < arr.length) {
    const temp = arr[i];
    arr[i] = arr[j];
    arr[j] = temp;
  }
  return arr;
}
