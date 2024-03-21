import xml2js from '../xml2js.js';

function xml2Json(xml) {
  let json;
  xml2js.parseString(xml, { async: false }, (err, result) => {
    if (err) {
      throw err;
    }
    json = result;
  });
  return json;
}

global.xml2Json = xml2Json;
