import http from "k6/http";
import { sleep, check } from "k6";

const data = JSON.parse(open("./reqForLoadTest.json"))

export default function () {
  var url =
    "http://127.0.0.1:8014/v1/7f7896dd-787e-4a0b-8675-e9e6fe93bb8f/search";
  data.forEach((elem) => {
    var params = {
      headers: {
        "Content-Type": "application/json",
        Authorization: elem.signature,
        "Proxy-Authorization": elem.signature,
      },
    };
    let res = http.post(url, elem.rawRequest, params);
    check(res, { "status was 200": (r) => r.status == 200 });
  });
}
