package in.juspay.mobility.common.dataModel;

public class CircleConfig {
    private String id, stroke, fillColor;
    private double centerLat, centerLon;
    private int radius, strokeWidth;

    public String getId() {
        return id;
    }

    public int getStrokeWidth() {
        return strokeWidth;
    }

    public String getStroke() {
        return stroke;
    }

    public String getFillColor() {
        return fillColor;
    }

    public double getCenterLat() {
        return centerLat;
    }

    public double getCenterLon() {
        return centerLon;
    }

    public int getRadius() {
        return radius;
    }
}
