
export function getStringValue(key){
    if (key in mlStrings){
        return mlStrings[key];
    }
    console.error("string not found in mlStrings");
    return "";
}

const mlStrings = {
    YOUR_VEHICLE : "നിങ്ങളുടെ വാഹനം",
    BOOKING_OPTIONS : "ബുക്കിംഗ് ഓപ്ഷനുകൾ"
}