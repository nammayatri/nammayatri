package in.juspay.mobility.common;
import static androidx.core.content.PackageManagerCompat.LOG_TAG;

import android.util.Log;

import org.json.JSONObject;

import in.org.npci.bbps.BBPSAgentInterface;

public class BBPSAgent implements in.org.npci.bbps.BBPSAgentInterface {
        /*
         *  BBPSAgent functions needs to populated by
         *  the client. These functions will be invoked by
         *  the SDK during processing.
         *
         * */
        private final String LOG_TAG = this.getClass().getSimpleName();

        @Override
        public void doPayment(JSONObject billDetails) {
//            bbo
//            Intent intent = new Intent(MainActivity.this, PaymentGateway.class);
//            MainActivity.this.startActivity(intent);
            Log.d(LOG_TAG,"Coming here with: "+billDetails);
//            initiatePP("");
        }

        @Override
        public void setTxnStatus(JSONObject txnStatus) {
//            Intent intent = new Intent("CLOSE_PAYMENT_ACTIVITY");
//            sendBroadcast(intent);
            // callback from SDK to Application
            // to store the transaction status
        }


    }