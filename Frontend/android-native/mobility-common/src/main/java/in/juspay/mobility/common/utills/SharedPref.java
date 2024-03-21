package in.juspay.mobility.common.utills;

import static android.content.Context.MODE_PRIVATE;

import android.content.SharedPreferences;
import android.webkit.JavascriptInterface;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.mobility.common.R;
import in.juspay.mobility.common.Utils;

public class SharedPref {

    BridgeComponents bridgeComponents;

    public SharedPref(BridgeComponents bridgeComponents){
        this.bridgeComponents = bridgeComponents;
    }

    public void setKeysInSharedPrefs(String key, String value) {
        SharedPreferences sharedPref = bridgeComponents.getContext().getSharedPreferences(bridgeComponents.getSdkName(), MODE_PRIVATE);
        sharedPref.edit().putString(key, value).apply();
        if (key.equals(bridgeComponents.getContext().getString(R.string.LANGUAGE_KEY))) {
            Utils.updateLocaleResource(value, bridgeComponents.getContext());
        }
    }

    public String getKeysInSharedPref(String key) {
        SharedPreferences sharedPref = bridgeComponents.getContext().getSharedPreferences(bridgeComponents.getSdkName(), MODE_PRIVATE);
        return sharedPref.getString(key, "__failed");
    }

    public String getKeyInNativeSharedPrefKeys(String key) {
        return getKeysInSharedPref(key);
    }

    public void setEnvInNativeSharedPrefKeys(String key, String value) {
        setKeysInSharedPrefs(key, value);
    }
}
