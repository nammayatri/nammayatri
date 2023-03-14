package in.juspay.beckn;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import android.view.ViewGroup;


import in.juspay.hypersdk.HyperFragment;
import in.juspay.services.HyperServices;

class BecknServices extends HyperServices {
    BecknServices(@NonNull MainActivity fragmentActivity, @NonNull ViewGroup viewGroup) {
        super(fragmentActivity, viewGroup);
    }

    @Override
    protected HyperFragment createFragment() {return new Fragment(); }

    @Nullable
    @Override
    protected HyperFragment getFragment() {
        return super.getFragment();
    }
}
