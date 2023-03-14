package in.juspay.mobility.utils;

public class SheetModel {
    private String pickUpDistance;
    private String distanceToBeCovered;
    private String sourceAddress;
    private String destinationArea;
    private String destinationAddress;
    private Float basePrice;
    private String requestId;
    private int reqExpiryTime;
    private int startTime;
    private Float updatedAmount;
    private String searchRequestId;
    private int offeredPrice;

    public SheetModel(String pickUpDistance, String distanceToBeCovered, String sourceAddress, String destinationAddress, Float basePrice, int reqExpiryTime, String searchRequestId, int offeredPrice, String destinationArea){
        this.pickUpDistance = pickUpDistance;
        this.distanceToBeCovered = distanceToBeCovered;
        this.sourceAddress =sourceAddress;
        this.destinationArea = destinationArea;
        this.destinationAddress = destinationAddress;
        this.basePrice = basePrice;
        this.updatedAmount = basePrice;
        this.reqExpiryTime = reqExpiryTime;
        this.searchRequestId = searchRequestId;
        this.offeredPrice = offeredPrice;
    }

    public int getOfferedPrice() {
        return offeredPrice;
    }

    public void setOfferedPrice(int offeredPrice) {
        this.offeredPrice = offeredPrice;
    }

    public String getSearchRequestId() {
        return searchRequestId;
    }

    public void setSearchRequestId(String searchRequestId) {
        this.searchRequestId = searchRequestId;
    }

    public String getPickUpDistance() {
        return pickUpDistance;
    }

    public void setPickUpDistance(String pickUpDistance) {
        this.pickUpDistance = pickUpDistance;
    }

    public String getDistanceToBeCovered() {
        return distanceToBeCovered;
    }

    public void setDistanceToBeCovered(String distanceToBeCovered) {
        this.distanceToBeCovered = distanceToBeCovered;
    }

    public String getSourceAddress() {
        return sourceAddress;
    }

    public String getDestinationArea(){
        return destinationArea;
    }

    public void setSourceAddress(String sourceAddress) {
        this.sourceAddress = sourceAddress;
    }

    public String getDestinationAddress() {
        return destinationAddress;
    }

    public void setDestinationAddress(String destinationAddress) {
        this.destinationAddress = destinationAddress;
    }

    public Float getBasePrice() {
        return basePrice;
    }

    public void setBasePrice(Float basePrice) {
        this.basePrice = basePrice;
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

    public void setReqExpiryTime(int reqExpiryTime) {
        this.reqExpiryTime = reqExpiryTime;
    }

    public int getStartTime() {
        return startTime;
    }

    public void setStartTime(int startTime) {
        this.startTime = startTime;
    }

    public Float getUpdatedAmount() {
        return updatedAmount;
    }

    public void setUpdatedAmount(Float updatedAmount) {
        this.updatedAmount = updatedAmount;
    }
}
