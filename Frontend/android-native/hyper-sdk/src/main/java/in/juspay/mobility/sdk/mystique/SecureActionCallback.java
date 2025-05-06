package in.juspay.mobility.sdk.mystique;

import android.os.Build;
import android.view.ActionMode;
import android.view.Menu;
import android.view.MenuItem;


public class SecureActionCallback implements ActionMode.Callback {
    private boolean disableCopy = false;
    private boolean disableCut = false;
    private boolean disableShare = false;
    private boolean disablePaste = false;

    public SecureActionCallback(boolean disableCopy, boolean disableCut, boolean disableShare, boolean disablePaste) {
        this.disableCopy = disableCopy;
        this.disableCut = disableCut;
        this.disableShare = disableShare;
        this.disablePaste = disablePaste;
    }

    @Override
    public boolean onCreateActionMode(ActionMode mode, Menu menu) {
        if(disableCopy)
            menu.removeItem(android.R.id.copy);
        if(disableCut)
            menu.removeItem(android.R.id.cut);
        if(disableShare)
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
                menu.removeItem(android.R.id.shareText);
            }
        if (disablePaste)
            menu.removeItem(android.R.id.paste);
        return true;
    }

    @Override
    public boolean onPrepareActionMode(ActionMode mode, Menu menu) {
        if(disableCopy)
            menu.removeItem(android.R.id.copy);
        if(disableCut)
            menu.removeItem(android.R.id.cut);
        if(disableShare)
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
                menu.removeItem(android.R.id.shareText);
            }
        if (disablePaste)
            menu.removeItem(android.R.id.paste);
        return true;
    }

    @Override
    public boolean onActionItemClicked(ActionMode mode, MenuItem item) {
        return true;
    }

    @Override
    public void onDestroyActionMode(ActionMode mode) {

    }
}
