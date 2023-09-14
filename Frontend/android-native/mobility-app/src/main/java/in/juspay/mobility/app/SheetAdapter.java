/*
 * Copyright 2022-23, Juspay
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 * is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
package in.juspay.mobility.app;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;
import androidx.viewpager2.widget.ViewPager2;

import com.google.android.material.card.MaterialCardView;
import java.util.ArrayList;
import java.util.List;

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

    public void updateSheetList(ArrayList<SheetModel> sheetList) {
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
        listener.onViewHolderBind(holder, position, viewPager, null);
    }

    @Override
    public void onBindViewHolder(@NonNull SheetViewHolder holder, int position, @NonNull List<Object> payloads) {
        listener.onViewHolderBind(holder, position, viewPager, payloads);
    }

    @Override
    public int getItemCount() {
        return sheetList.size();
    }

    public static class SheetViewHolder extends RecyclerView.ViewHolder{
        TextView pickUpDistance, acceptRejTimer, baseFare, sourceArea, currency, sourceAddress, destinationArea, destinationAddress, distanceToBeCovered, textIncPrice, textDecPrice, zoneText, fixedFareText, customerTipText, textIncludesCharges, sourcePinCode , destinationPinCode, accessibilityTagText;
        Button reqButton, rejectButton;
        View buttonDecreasePrice, buttonIncreasePrice, progressBar;
        ImageView assetZonePickup, assetZoneDrop;
        MaterialCardView specialLocationTag, zoneDot;
        LinearLayout customerTipBlock;
        LinearLayout accessibilityTag;
        LinearLayout customerTipTag;

        public SheetViewHolder(@NonNull View itemView) {
            super(itemView);
            pickUpDistance = itemView.findViewById(R.id.distancePickUp);
            baseFare = itemView.findViewById(R.id.basePrice);
            currency = itemView.findViewById(R.id.currency);
            sourceArea = itemView.findViewById(R.id.sourceArea);
            sourceAddress = itemView.findViewById(R.id.journeySource);
            destinationArea = itemView.findViewById(R.id.destinationArea);
            destinationAddress = itemView.findViewById(R.id.journeyDestination);
            distanceToBeCovered = itemView.findViewById(R.id.distanceToBeCovered);
            acceptRejTimer = itemView.findViewById(R.id.acceptRejTimer);
            reqButton = itemView.findViewById(R.id.reqButton);
            rejectButton = itemView.findViewById(R.id.rejButton);
            buttonDecreasePrice = itemView.findViewById(R.id.buttonDecreasePrice);
            buttonIncreasePrice = itemView.findViewById(R.id.buttonIncreasePrice);
            progressBar = itemView.findViewById(R.id.progressBar);
            textDecPrice = itemView.findViewById(R.id.textDecPrice);
            textIncPrice = itemView.findViewById(R.id.textIncPrice);
            customerTipBlock = itemView.findViewById(R.id.customer_tip_block);
            customerTipText = itemView.findViewById(R.id.tip_text);
            accessibilityTagText = itemView.findViewById(R.id.accessibilityTagText);
            textIncludesCharges = itemView.findViewById(R.id.textIncludesCharges);
            specialLocationTag = itemView.findViewById(R.id.specialLocationTag);
            zoneText = itemView.findViewById(R.id.zoneText);
            fixedFareText = itemView.findViewById(R.id.fixedFareText);
            zoneDot = itemView.findViewById(R.id.zoneDot);
            assetZoneDrop = itemView.findViewById(R.id.assetZoneDrop);
            assetZonePickup = itemView.findViewById(R.id.assetZonePickup);
            sourcePinCode = itemView.findViewById(R.id.sourcePinCode);
            destinationPinCode = itemView.findViewById(R.id.destinationPinCode);
            accessibilityTag = itemView.findViewById(R.id.accessibilityTag);
            customerTipTag = itemView.findViewById(R.id.customerTipTag);
        }
    }


    public interface OnItemClickListener {
        void onViewHolderBind(SheetViewHolder holder, int position, ViewPager2 viewPager, List<Object> objects);
    }

}
