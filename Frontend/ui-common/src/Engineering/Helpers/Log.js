exports["log"] = function (a) {
  return function (b) {
    console.log(a, " : ", b);
  };
};
