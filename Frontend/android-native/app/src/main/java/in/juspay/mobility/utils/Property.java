package in.juspay.mobility.utils;

/**
 * Simple class used for the systems properties
 * checked in later places of the application.
 *
 * @author tstrazzere
 */
public class Property {
    public String name;
    public String seek_value;

    public Property(String name, String seek_value) {
        this.name = name;
        this.seek_value = seek_value;
    }
}