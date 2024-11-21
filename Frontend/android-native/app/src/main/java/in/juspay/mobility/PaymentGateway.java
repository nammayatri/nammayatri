package in.juspay.mobility;
//package in.juspay.bbps;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;

import org.json.JSONArray;
import org.json.JSONObject;

import java.security.SecureRandom;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;

import in.org.npci.bbps.BBPSService;

public class PaymentGateway extends AppCompatActivity {

    public static BBPSService bbpsService;
    
    public static MainActivity mainActivityContext = MainActivity.appContext;

    private final BroadcastReceiver closeReceiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            if ("CLOSE_PAYMENT_ACTIVITY".equals(intent.getAction())) {
                finish(); // Close the current activity
            }
        }
    };

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        // setContentView(in.juspay.mobility.R.layout.bbps_payment_layout);
        setContentView(R.layout.bbps_payment_layout);

        IntentFilter filter = new IntentFilter("CLOSE_PAYMENT_ACTIVITY");
        registerReceiver(closeReceiver, filter);
    }

    public static void setBBPSService(BBPSService bbps){
        bbpsService = bbps;
    }

    private static String getJulianDate() {
        Date date = new Date();
        GregorianCalendar calendar = new GregorianCalendar(TimeZone.getTimeZone("UTC"));
        calendar.setTime(date);
        int year = calendar.get(GregorianCalendar.YEAR) % 10; // Last digit of the year
        int dayOfYear = calendar.get(GregorianCalendar.DAY_OF_YEAR);
        return String.format("%d%03d", year, dayOfYear);  // YDDD format
    }
    public static String generateRandomId() {
        SecureRandom random1 = new SecureRandom();
        StringBuilder sb = new StringBuilder("BL01");
        sb.append(getJulianDate());

        for (int i = 0; i < 12; i++) {
            sb.append(random1.nextInt(10));
        }

        return sb.toString();
    }

    public void startMicroForSuccessTxn(View view) {
        try {
            JSONObject payload = new JSONObject();
            JSONObject data = new JSONObject();
            mainActivityContext.lastTxnRefId = generateRandomId();

            data.put("response", "Success");
            data.put("note", "Sending Money");
            data.put("bbpsTxnId", mainActivityContext.bbpsBillDetail.getString("bbpsTxnid"));

            JSONObject paymentDetails = new JSONObject();
            paymentDetails.put("payeeName", "Neha Jain");
            paymentDetails.put("txnAmount", mainActivityContext.bbpsBillDetail.getJSONObject("billdetails").getString("txnAmount"));
            paymentDetails.put("txnRefId", mainActivityContext.lastTxnRefId);
            paymentDetails.put("paymentMode", "Debit_Card");
            paymentDetails.put("couCustConvFee", "10");

            JSONArray paymentParams = new JSONArray();
            JSONObject paymentParam = new JSONObject();
            paymentParam.put("name", "CardNum|AuthCode");
            paymentParam.put("value", "4336620020624963|123456");
            paymentParams.put(paymentParam);
            paymentDetails.put("paymentParams", paymentParams);

            data.put("paymentDetails", paymentDetails);
            payload.put("mobileNumber", mainActivityContext.bbpsBillDetail.getString("mobileNumber"));
            payload.put("action", "TRANSACTION_STATUS");
            payload.put("data", data.toString());
            Log.e("Testing101", payload.toString());

            bbpsService.process(this, payload);
        } catch (Exception e){
            Toast.makeText(this, e.toString(), Toast.LENGTH_LONG).show();
        }
    }
}
