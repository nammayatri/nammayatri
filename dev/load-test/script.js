import http from "k6/http";
import { check } from "k6";

const data = JSON.parse(open(__ENV.FILE_PATH));
const url = __ENV.LOAD_TEST_URL + "/search";

function toBatchRequest(input, url) {
  return input.map((elem) => {
    const params = {
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
  const responses = http.batch(toBatchRequest(data, url));
  responses.forEach((res) => {
    check(res, { "status was 200": (r) => r.status == 200 });
  });
}
