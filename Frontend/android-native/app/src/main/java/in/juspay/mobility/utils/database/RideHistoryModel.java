package in.juspay.mobility.utils.database;

public class RideHistoryModel {
    private String
            id,
            status,
            time,
            date,
            shortRideId,
            vehicleNumber,
            driverName,
            rideDistance,
            updatedAt,
            source,
            destination;
    private int
            totalAmount,
            driverSelectedFare;


    public RideHistoryModel(){}
    public RideHistoryModel(String id,
                            String status,
                            String date,
                            String time,
                            String rideDistance,
                            String shortRideId,
                            String vehicleNumber,
                            String driverName,
                            String updatedAt,
                            String source,
                            String destination,
                            int driverSelectedFare,
                            int totalAmount) {
        this.id = id;
        this.status = status;
        this.shortRideId = shortRideId;
        this.vehicleNumber = vehicleNumber;
        this.driverName = driverName;
        this.updatedAt = updatedAt;
        this.driverSelectedFare = driverSelectedFare;
        this.source = source;
        this.destination = destination;
        this.date = date;
        this.time = time;
        this.rideDistance = rideDistance;
        this.totalAmount = totalAmount;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getShortRideId() {
        return shortRideId;
    }

    public void setShortRideId(String shortRideId) {
        this.shortRideId = shortRideId;
    }

    public String getVehicleNumber() {
        return vehicleNumber;
    }

    public void setVehicleNumber(String vehicleNumber) {
        this.vehicleNumber = vehicleNumber;
    }

    public String getDriverName() {
        return driverName;
    }

    public void setDriverName(String driverName) {
        this.driverName = driverName;
    }

    public String getUpdatedAt() {
        return updatedAt;
    }

    public void setUpdatedAt(String updatedAt) {
        this.updatedAt = updatedAt;
    }

    public int getDriverSelectedFare() {
        return driverSelectedFare;
    }

    public void setDriverSelectedFare(int driverSelectedFare) {
        this.driverSelectedFare = driverSelectedFare;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public String getDestination() {
        return destination;
    }

    public void setDestination(String destination) {
        this.destination = destination;
    }

    public String getTime() {
        return time;
    }

    public void setTime(String time) {
        this.time = time;
    }

    public String getDate() {
        return date;
    }

    public void setDate(String date) {
        this.date = date;
    }

    public String getRideDistance() {
        return rideDistance;
    }

    public void setRideDistance(String rideDistance) {
        this.rideDistance = rideDistance;
    }

    public int getTotalAmount() {
        return totalAmount;
    }

    public void setTotalAmount(int totalAmount) {
        this.totalAmount = totalAmount;
    }
}