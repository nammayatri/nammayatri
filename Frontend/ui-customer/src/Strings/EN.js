export function getStringValue(key){
    if (key in englishStrings){
        return englishStrings[key];
    }
    return "error in getCommonEN";
}

const englishStrings = {
    WELCOME_TEXT : "Welcome to the app",
    ABOUT_TEXT : "This App is an open platform to connect drivers with riders. The app makes it convenient for drivers to find riders with proposed desired rates. No ride based commission, just pay small amount in the form of monthly subscription"
}