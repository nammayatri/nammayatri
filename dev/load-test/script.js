import http from "k6/http";
import { check } from "k6";

const data = JSON.parse(open("./reqForLoadTest.json"));
const url =
  "http://127.0.0.1:8014/v1/7f7896dd-787e-4a0b-8675-e9e6fe93bb8f/search";

function toBatchRequest(input) {
  return input.map((elem) => {
    var params = {
      headers: {
        "Content-Type": "application/json",
        Authorization: elem.signature,
        "Proxy-Authorization": elem.signature,
      },
    };
    return ["POST", url, elem.rawRequest, params];
  });
}

export default function () {
  let responses = http.batch(toBatchRequest(data));
  responses.forEach((res) => {
    check(res, { "status was 200": (r) => r.status == 200 });
  });
}
