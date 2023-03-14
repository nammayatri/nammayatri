package in.juspay.mobility;

import android.content.Intent;
import android.util.Log;

import androidx.annotation.Nullable;

import com.google.zxing.integration.android.IntentIntegrator;
import com.google.zxing.integration.android.IntentResult;

import in.juspay.hypersdk.HyperFragment;
import in.juspay.hypersdk.core.DuiInterface;

public class Fragment extends HyperFragment {

    @Override
    public void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        IntentResult result = IntentIntegrator.parseActivityResult(requestCode, resultCode, data);
        System.out.println("HyperFragment Request Code " + requestCode + " ResultCode " + resultCode + " IntentData " + data);
        if (result != null && result.getContents() != null) {
            if(this.getDuiInterface() != null)
                this.getDuiInterface().onActivityResult(requestCode, resultCode, data);
            else Log.e("Fragment", "DuiInterface is null, can't execute onActivityResult");
        } else {
            super.onActivityResult(requestCode, resultCode, data);
            if(this.getDuiInterface() != null)
                this.getDuiInterface().onActivityResult(requestCode, resultCode, data);
        }
    }
    
    protected DuiInterface createDuiInterface(){
        return new JsInterface(this.getActivity(), this.getJuspayServices(), this);
    }
}
