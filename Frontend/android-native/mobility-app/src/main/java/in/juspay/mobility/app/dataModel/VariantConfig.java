package in.juspay.mobility.app.dataModel;

public class VariantConfig {
    String text, textColor, background, icon;
    boolean visible = false;

    public String getText() {
        return text;
    }

    public String getTextColor() {
        return textColor;
    }

    public String getBackground() {
        return background;
    }

    public String getIcon() {
        return icon;
    }

    public boolean isVisible() {
        return visible;
    }

    public VariantConfig(String text, String textColor, String background, String icon, boolean visible) {
        this.text = text;
        this.textColor = textColor;
        this.background = background;
        this.icon = icon;
        this.visible = visible;
    }
}