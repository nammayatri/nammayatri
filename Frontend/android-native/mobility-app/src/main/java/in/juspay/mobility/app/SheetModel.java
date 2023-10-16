/*
 * Copyright 2022-23, Juspay
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 * is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
package in.juspay.mobility.app;

public class SheetModel {
    private final String pickUpDistance, distanceToBeCovered, durationToPickup, sourceArea, currency, sourceAddress, destinationArea, destinationAddress, searchRequestId, specialLocationTag, sourcePinCode, destinationPinCode, requestedVehicleVariant , notificationType;
    private String requestId;
    private int startTime;
    private double updatedAmount;
    private double offeredPrice;
    private int customerExtraFee;
    private final int baseFare;
    private final int reqExpiryTime;
    private final int driverMinExtraFee;
    private final int driverMaxExtraFee;
    private final int rideRequestPopupDelayDuration;
    private final int negotiationUnit;

    private final Boolean disabilityTag;
    private float buttonIncreasePriceAlpha , buttonDecreasePriceAlpha;
    private boolean buttonIncreasePriceClickable , buttonDecreasePriceClickable;

    public SheetModel(String pickUpDistance,
                      String distanceToBeCovered,
                      String durationToPickup,
                      String sourceAddress,
                      String destinationAddress,
                      int baseFare,
                      int reqExpiryTime,
                      String searchRequestId,
                      String destinationArea,
                      String sourceArea,
                      String currency,
                      int startTime,
                      int driverMinExtraFee,
                      int driverMaxExtraFee,
                      int rideRequestPopupDelayDuration,
                      int negotiationUnit,
                      int customerExtraFee,
                      String specialLocationTag,
                      String sourcePinCode,
                      String destinationPinCode,
                      String requestedVehicleVariant,
                      Boolean disabilityTag,
                      String notificationType){
        this.pickUpDistance = pickUpDistance;
        this.distanceToBeCovered = distanceToBeCovered;
        this.sourceArea = sourceArea;
        this.sourceAddress = sourceAddress;
        this.destinationArea = destinationArea;
        this.destinationAddress = destinationAddress;
        this.updatedAmount = 0;
        this.reqExpiryTime = reqExpiryTime;
        this.searchRequestId = searchRequestId;
        this.offeredPrice = 0;
        this.baseFare = baseFare;
        this.startTime = startTime;
        this.driverMinExtraFee = driverMinExtraFee;
        this.driverMaxExtraFee = driverMaxExtraFee;
        this.rideRequestPopupDelayDuration = rideRequestPopupDelayDuration;
        this.negotiationUnit = negotiationUnit;
        this.buttonIncreasePriceAlpha = 1.0f;
        this.buttonIncreasePriceClickable = true;
        this.buttonDecreasePriceAlpha = 0.5f;
        this.buttonDecreasePriceClickable = false;
        this.currency = currency;
        this.specialLocationTag = specialLocationTag;
        this.customerExtraFee = customerExtraFee;
        this.sourcePinCode = sourcePinCode;
        this.destinationPinCode = destinationPinCode;
        this.requestedVehicleVariant = requestedVehicleVariant;
        this.disabilityTag = disabilityTag;
        this.durationToPickup = durationToPickup;
        this.notificationType = notificationType;
    }

    public String getRequestedVehicleVariant() {
        return requestedVehicleVariant;
    }

    public double getOfferedPrice() {
        return offeredPrice;
    }

    public int getCustomerTip() {return customerExtraFee;}

    public Boolean getDisabilityTag() {return disabilityTag; }

    public void setOfferedPrice(double offeredPrice) {
        this.offeredPrice = offeredPrice;
    }

    public String getSearchRequestId() {
        return searchRequestId;
    }

    public String getPickUpDistance() {
        return pickUpDistance;
    }

    public String getDistanceToBeCovered() {
        return distanceToBeCovered;
    }

    public String getSourceArea() {
        return sourceArea;
    }

    public String getSourceAddress() {
        return sourceAddress;
    }

    public String getDestinationArea() {
        return destinationArea;
    }

    public String getDestinationAddress() {
        return destinationAddress;
    }

    public int getBaseFare() {
        return baseFare;
    }

    public String getCurrency () {
        return currency;
    }

    public String getRequestId() {
        return requestId;
    }

    public void setRequestId(String requestId) {
        this.requestId = requestId;
    }

    public int getReqExpiryTime() {
        return reqExpiryTime;
    }

    public int getStartTime() {
        return startTime;
    }

    public void setStartTime(int startTime) {
        this.startTime = startTime;
    }

    public double getUpdatedAmount() {
        return updatedAmount;
    }

    public void setUpdatedAmount(double updatedAmount) {
        this.updatedAmount = updatedAmount;
    }

    public int getDriverMaxExtraFee() {
        return driverMaxExtraFee;
    }

    public int getRideRequestPopupDelayDuration() {
        return rideRequestPopupDelayDuration;
    }

    public int getNegotiationUnit() {
        return negotiationUnit;
    }

    public float getButtonIncreasePriceAlpha() {
        return buttonIncreasePriceAlpha;
    }

    public float getButtonDecreasePriceAlpha() {
        return buttonDecreasePriceAlpha;
    }

    public boolean isButtonIncreasePriceClickable() {
        return buttonIncreasePriceClickable;
    }

    public boolean isButtonDecreasePriceClickable() {
        return buttonDecreasePriceClickable;
    }

    public void setButtonIncreasePriceAlpha(float buttonIncreasePriceAlpha) {
        this.buttonIncreasePriceAlpha = buttonIncreasePriceAlpha;
    }

    public void setButtonDecreasePriceAlpha(float buttonDecreasePriceAlpha) {
        this.buttonDecreasePriceAlpha = buttonDecreasePriceAlpha;
    }

    public void setButtonIncreasePriceClickable(boolean buttonIncreasePriceClickable) {
        this.buttonIncreasePriceClickable = buttonIncreasePriceClickable;
    }

    public void setButtonDecreasePriceClickable(boolean buttonDecreasePriceClickable) {
        this.buttonDecreasePriceClickable = buttonDecreasePriceClickable;
    }

    public void setCustomerTip(int customerExtraFee) {
        this.customerExtraFee = customerExtraFee;
    }

    public String getspecialLocationTag() {
        return specialLocationTag;
    }

    public String getSourcePinCode() {
        return sourcePinCode;
    }

    public String getDestinationPinCode() {
        return destinationPinCode;
    }

    public String getDurationToPickup(){
        return durationToPickup;
    }

    public String getNotificationType(){
        return notificationType;
    }
}
