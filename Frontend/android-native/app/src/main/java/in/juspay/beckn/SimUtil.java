package in.juspay.beckn;


import android.annotation.TargetApi;
import android.app.PendingIntent;
import android.content.Context;
import android.os.Build;
import android.os.IBinder;
import android.telephony.SmsManager;
import android.telephony.SubscriptionInfo;
import android.telephony.SubscriptionManager;
import android.telephony.TelephonyManager;
import android.util.Log;

import org.json.JSONArray;
import org.json.JSONObject;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by Arun on 6/21/2016.
 */
public class SimUtil {
    private static final String TAG = "SimUtil";

    @TargetApi(Build.VERSION_CODES.LOLLIPOP_MR1)
    public static boolean sendSMS(Context ctx, int simID, String toNum, String centerNum, final String smsText, final PendingIntent sentIntent, final PendingIntent deliveryIntent) {
        String name;
        try {
            if (simID == 0) {
                name = Build.MODEL.equals("Philips T939") ? "isms0" : "isms";
            } else if (simID == 1) {
                name = "isms2";
            } else {
                throw new Exception("can not get service which for sim '" + simID + "', only 0,1 accepted as values");
            }
            Method method = Class.forName("android.os.ServiceManager").getDeclaredMethod("getService", String.class);
            method.setAccessible(true);
            Object param = method.invoke(null, name);

            method = Class.forName("com.android.internal.telephony.ISms$Stub").getDeclaredMethod("asInterface", IBinder.class);
            method.setAccessible(true);
            Object stubObj = method.invoke(null, param);
            Log.d(TAG, "send msg - " + smsText);
            if (Build.VERSION.SDK_INT < 18) {
                method = stubObj.getClass().getMethod("sendText", String.class, String.class, String.class, PendingIntent.class, PendingIntent.class);
                method.invoke(stubObj, toNum, centerNum, smsText, sentIntent, deliveryIntent);
            } else {
                if (isDualSim(ctx)) {
                    ArrayList<Integer> subIds = new ArrayList<>();
                    SubscriptionManager subscriptionManager = SubscriptionManager.from(ctx);
                    List<SubscriptionInfo> subscriptionInfoList = subscriptionManager.getActiveSubscriptionInfoList();
                    for (SubscriptionInfo subscriptionInfo : subscriptionInfoList) {
                        int subscriptionId = subscriptionInfo.getSubscriptionId();
                        subIds.add(subscriptionId);
                        Log.d(TAG, "SmsManager - subscriptionId: " + subscriptionId);
                    }
                    SmsManager.getSmsManagerForSubscriptionId(subIds.get(simID)).sendTextMessage(toNum, null, smsText, sentIntent, deliveryIntent);
                } else {
                    SmsManager.getDefault().sendTextMessage(toNum, null, smsText, sentIntent, deliveryIntent);
                }
            }

            return true;
        } catch (Exception e) {
            Log.e(TAG, "Excepting in sending SMS ", e);
        }
        return false;
    }

    public static boolean isDualSim(Context context) {
        if (Build.VERSION.SDK_INT >= 22) {
            return SubscriptionManager.from(context).getActiveSubscriptionInfoCount() > 1;
        } else {
            return false;
        }
    }

    public static boolean isSimSupport(Context context) {
        TelephonyManager tm = (TelephonyManager) context.getSystemService(Context.TELEPHONY_SERVICE);  //gets the current TelephonyManager
        return !(tm.getSimState() == TelephonyManager.SIM_STATE_ABSENT);

    }

    public static String getSIMOperators(Context context) {
        try {
            List<SubscriptionInfo> subInfoList;
            JSONArray simDetails = new JSONArray();
            if (Build.VERSION.SDK_INT >= 22) {
                SubscriptionManager mSubscriptionManager = SubscriptionManager.from(context);
                subInfoList = mSubscriptionManager.getActiveSubscriptionInfoList();
                for (SubscriptionInfo subscriptionInfo : subInfoList) {
                    try {
                        JSONObject simDetail = new JSONObject();
                        simDetail.put("slotId", subscriptionInfo.getSimSlotIndex());
                        simDetail.put("subscriptionId", subscriptionInfo.getSubscriptionId());
                        simDetail.put("displayName", subscriptionInfo.getDisplayName());
                        simDetail.put("carrierName", subscriptionInfo.getCarrierName());
                        simDetail.put("phoneNumber", subscriptionInfo.getNumber());
                        simDetail.put("simId",subscriptionInfo.getIccId());
                        simDetails.put(simDetail);
                    } catch (Exception e) {
                        Log.e(TAG, "Exception getting sim details for SDK >= 22", e);
                    }
                }
            } else {
                TelephonyManager telephonyManager = ((TelephonyManager) context.getSystemService(Context.TELEPHONY_SERVICE));
                if (telephonyManager != null) {
                    try {
                        JSONObject simDetail = new JSONObject();
                        simDetail.put("slotId", telephonyManager.getSimState());
                        simDetail.put("subscriptionId", telephonyManager.getSubscriberId());
                        simDetail.put("displayName", telephonyManager.getNetworkOperator());
                        simDetail.put("carrierName", telephonyManager.getNetworkOperatorName());
                        simDetail.put("phoneNumber", telephonyManager.getLine1Number());
                        simDetail.put("simId",telephonyManager.getSimSerialNumber());
                        simDetails.put(simDetail);
                    } catch (Exception e) {
                        Log.e(TAG, "Exception getting sim details for SDK < 22", e);
                    }
                }
            }
            return simDetails.toString();
        } catch (Exception e) {
            Log.e(TAG, "Not able to fetch Sim Cards");
        }
        return null;
    }
}