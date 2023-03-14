package in.juspay.beckn.utils;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;
import androidx.viewpager2.widget.ViewPager2;

import java.util.ArrayList;
import java.util.List;

import in.juspay.beckn.R;

public class SheetAdapter extends RecyclerView.Adapter<SheetAdapter.SheetViewHolder> {
    private ArrayList<SheetModel> sheetList;
    private final OnItemClickListener listener;
    private ViewPager2 viewPager;

    public void setViewPager(ViewPager2 viewPager) {
        this.viewPager = viewPager;
    }

    public SheetAdapter(ArrayList<SheetModel> sheetList, ViewPager2 viewPager, OnItemClickListener listener) {
        this.sheetList = sheetList;
        this.viewPager = viewPager;
        this.listener = listener;
    }

    public void updateSheetList (ArrayList<SheetModel> sheetList) {
        this.sheetList = sheetList;
    }

    @NonNull
    @Override
    public SheetViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.sheet_view, parent, false);
        return new SheetViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull SheetViewHolder holder, int position) {
        listener.onViewHolderBind(holder,position, viewPager, null);
    }

    @Override
    public void onBindViewHolder(@NonNull SheetViewHolder holder, int position, @NonNull List<Object> payloads) {
        listener.onViewHolderBind(holder,position, viewPager, payloads);
    }

    @Override
    public int getItemCount() {
        return sheetList.size();
    }

    public class SheetViewHolder extends RecyclerView.ViewHolder{
        TextView pickUpDistance;
        TextView basePrice;
        TextView sourceAddress;
        TextView destinationArea;
        TextView destinationAddress;
        TextView distanceToBeCovered;
        TextView acceptRejTimer;
        Button reqButton;
        Button rejButton;
        View buttonDecreasePrice;
        View buttonIncreasePrice;
        View progressBar;
        String amount;
        public SheetViewHolder(@NonNull View itemView) {
            super(itemView);
            pickUpDistance = itemView.findViewById(R.id.distancePickUp);
            basePrice = itemView.findViewById(R.id.basePrice);
            sourceAddress = itemView.findViewById(R.id.journeySource);
            destinationArea = itemView.findViewById(R.id.destinationArea);
            destinationAddress = itemView.findViewById(R.id.journeyDestination);
            distanceToBeCovered = itemView.findViewById(R.id.distanceToBeCovered);
            acceptRejTimer = itemView.findViewById(R.id.acceptRejTimer);
            reqButton = itemView.findViewById(R.id.reqButton);
            rejButton = itemView.findViewById(R.id.rejButton);
            buttonDecreasePrice = itemView.findViewById(R.id.buttonDecreasePrice);
            buttonIncreasePrice = itemView.findViewById(R.id.buttonIncreasePrice);
            progressBar = itemView.findViewById(R.id.progressBar);
            amount = (String) basePrice.getText();
        }
    }



    public interface OnItemClickListener{
        void onViewHolderBind(SheetViewHolder holder, int position, ViewPager2 viewPager, List<Object> objects);
    }

}
