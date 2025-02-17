/*
 * Copyright 2022-23, Juspay
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 * is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
package in.juspay.mobility.app;

import android.content.Context;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.GradientDrawable;
import android.view.ContextThemeWrapper;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.ImageView;
import android.widget.TextView;
import androidx.cardview.widget.CardView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;
import androidx.viewpager2.widget.ViewPager2;

import com.google.android.material.card.MaterialCardView;

import org.json.JSONObject;

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
        holder.setTheme(sheetList.get(position).getRideProductType(),holder.context);
        listener.onViewHolderBind(holder, position, viewPager, null);
    }

    @Override
    public void onBindViewHolder(@NonNull SheetViewHolder holder, int position, @NonNull List<Object> payloads) {
        holder.setTheme(sheetList.get(position).getRideProductType(),holder.context);
        listener.onViewHolderBind(holder, position, viewPager, payloads);
    }

    @Override
    public int getItemCount() {
        return sheetList.size();
    }

    public static class SheetViewHolder extends RecyclerView.ViewHolder{
        Context context;
        TextView pickUpDistance, durationToPickup, acceptRejTimer, baseFare, sourceArea, currency, durationToPickupImage, sourceAddress, destinationArea, destinationAddress, distanceToBeCovered, textIncPrice, textDecPrice, customerTipText, textIncludesCharges, sourcePinCode , destinationPinCode, accessibilityTagText, rideTypeText, specialLocExtraTip, rateText, vehicleServiceTier, rideStartTime, rideStartDate, rideDuration, rideDistance, tollTag, thirdPartyTagText, stopsInfo, stopsTagText;
        Button reqButton, rejectButton;
        View buttonDecreasePrice, buttonIncreasePrice, progressBar, rateViewDot, acView, ventilator, nonAcView;
        MaterialCardView vcTierAndACView;
        ImageView assetZonePickup, assetZoneDrop, rideTypeImage, locationDashedLine;
        LinearLayout tagsBlock, accessibilityTag, customerTipTag, gotoTag, rideTypeTag, testRequestTag, specialLocTag, acNonAcView, rentalRideTypeTag, intercityRideTypeTag, rentalDurationDistanceTag, rideStartDateTimeTag, thirdPartyTag, isFavouriteTag, stopsTag , roundTripRideTypeTag;
        CardView locationDestinationPinTag;
        
        public SheetViewHolder(@NonNull View itemView) {
            super(itemView);
            context = itemView.getContext();
            pickUpDistance = itemView.findViewById(R.id.distancePickUp);
            baseFare = itemView.findViewById(R.id.basePrice);
            currency = itemView.findViewById(R.id.currency);
            sourceArea = itemView.findViewById(R.id.sourceArea);
            sourceAddress = itemView.findViewById(R.id.journeySource);
            destinationArea = itemView.findViewById(R.id.destinationArea);
            destinationAddress = itemView.findViewById(R.id.journeyDestination);
            distanceToBeCovered = itemView.findViewById(R.id.distanceToBeCovered);
            tollTag = itemView.findViewById(R.id.tollTag);
            durationToPickup = itemView.findViewById(R.id.durationToPickup);
            acceptRejTimer = itemView.findViewById(R.id.acceptRejTimer);
            reqButton = itemView.findViewById(R.id.reqButton);
            rejectButton = itemView.findViewById(R.id.rejButton);
            buttonDecreasePrice = itemView.findViewById(R.id.buttonDecreasePrice);
            buttonIncreasePrice = itemView.findViewById(R.id.buttonIncreasePrice);
            progressBar = itemView.findViewById(R.id.progressBar);
            textDecPrice = itemView.findViewById(R.id.textDecPrice);
            textIncPrice = itemView.findViewById(R.id.textIncPrice);
            tagsBlock = itemView.findViewById(R.id.tags_block);
            customerTipText = itemView.findViewById(R.id.tip_text);
            accessibilityTagText = itemView.findViewById(R.id.accessibilityTagText);
            textIncludesCharges = itemView.findViewById(R.id.textIncludesCharges);
            assetZoneDrop = itemView.findViewById(R.id.assetZoneDrop);
            assetZonePickup = itemView.findViewById(R.id.assetZonePickup);
            sourcePinCode = itemView.findViewById(R.id.sourcePinCode);
            destinationPinCode = itemView.findViewById(R.id.destinationPinCode);
            accessibilityTag = itemView.findViewById(R.id.accessibilityTag);
            customerTipTag = itemView.findViewById(R.id.customerTipTag);
            isFavouriteTag = itemView.findViewById(R.id.isFavouriteTag);
            rideTypeTag = itemView.findViewById(R.id.rideTypeTag);
            rideTypeText = itemView.findViewById(R.id.rideTypeText);
            rideTypeImage = itemView.findViewById(R.id.rideTypeImage);
            durationToPickupImage = itemView.findViewById(R.id.durationToPickupImage);
            gotoTag = itemView.findViewById(R.id.gotoTag);
            testRequestTag = itemView.findViewById(R.id.testRequestTag);
            specialLocTag = itemView.findViewById(R.id.spZoneTag);
            specialLocExtraTip = itemView.findViewById(R.id.spZoneExtraTip);
            rateText = itemView.findViewById(R.id.rateText);
            vehicleServiceTier = itemView.findViewById(R.id.vehicleServiceTier);
            acNonAcView = itemView.findViewById(R.id.acNonAcView);
            vcTierAndACView = itemView.findViewById(R.id.vcTierAndACView);
            rentalDurationDistanceTag = itemView.findViewById(R.id.rentalDurationDistanceTag);
            rideDuration = itemView.findViewById(R.id.rideDuration);
            rideDistance = itemView.findViewById(R.id.rideDistance);
            rideStartDateTimeTag = itemView.findViewById(R.id.rideStartDateTimeTag);
            rideStartTime = itemView.findViewById(R.id.rideStartTime);
            rideStartDate = itemView.findViewById(R.id.rideStartDate);
            rentalRideTypeTag = itemView.findViewById(R.id.rentalRideTypeTag);
            intercityRideTypeTag = itemView.findViewById(R.id.intercityRideTypeTag);
            locationDashedLine = itemView.findViewById(R.id.locationDashedLine);
            locationDestinationPinTag = itemView.findViewById(R.id.locationDestinationPinTag);
            rateViewDot = itemView.findViewById(R.id.rateViewDot);
            thirdPartyTag = itemView.findViewById(R.id.thirdPartyTag);
            thirdPartyTagText = itemView.findViewById(R.id.thirdPartyTagText);
            acView = itemView.findViewById(R.id.acView);
            ventilator = itemView.findViewById(R.id.ventilator);
            nonAcView = itemView.findViewById(R.id.nonAcView);
            stopsInfo = itemView.findViewById(R.id.stopsInfo);
            stopsTag = itemView.findViewById(R.id.stopsTag);
            stopsTagText = itemView.findViewById(R.id.stopsTagText);
            roundTripRideTypeTag = itemView.findViewById(R.id.roundTripRideTypeTag);
        }

        private void setTheme(String type,Context context) {
            SheetTheme theme = SheetTheme.getTheme(type,context);
            View mainView = itemView.findViewById(R.id.card);
            if (mainView != null) {
                updateBackground(theme.getPrimaryBackground(), mainView);
            }
            mainView = itemView.findViewById(R.id.main_sheet_layout);
            if (mainView != null) {
                mainView.setBackgroundColor(theme.getPrimaryBackground());
            }

            mainView = itemView.findViewById(R.id.pickupText);
            if (mainView != null) {
                ((TextView)mainView).setTextColor(theme.getSecondaryText());
            }

            pickUpDistance.setTextColor(theme.getPrimaryText());
            pickUpDistance.setTextColor(theme.getPrimaryText());
            sourceArea.setTextColor(theme.getPrimaryText());
            rideStartTime.setTextColor(theme.getPrimaryText());
            rideStartDate.setTextColor(theme.getPrimaryText());
            destinationArea.setTextColor(theme.getPrimaryText());
            sourcePinCode.setTextColor(theme.getPrimaryText());
            destinationPinCode.setTextColor(theme.getPrimaryText());
            sourceAddress.setTextColor(theme.getSecondaryText());
            destinationAddress.setTextColor(theme.getSecondaryText());
            destinationAddress.setTextColor(theme.getSecondaryText());
            rateText.setTextColor(theme.getSecondaryText());
            textIncludesCharges.setTextColor(theme.getSecondaryText());
            rateViewDot.setBackgroundColor(theme.getSecondaryText());
            tagsBlock.setBackgroundColor(theme.getSecondaryBackground());
        }
    }


    public interface OnItemClickListener {
        void onViewHolderBind(SheetViewHolder holder, int position, ViewPager2 viewPager, List<Object> objects);
    }

    private static void updateBackground(int color, View view) {
        if (view instanceof CardView) {
            CardView cardView = ((CardView)view);
            float radius =  cardView.getRadius();
            cardView.setCardBackgroundColor(color);
            cardView.setRadius(radius);
        }
    }
}
