package in.juspay.mobility.app;

public class ImageProperties {
    private String base64Image, imageName, imagePath;

    public ImageProperties(String base64Image, String imageName, String imagePath) {
        this.base64Image = base64Image;
        this.imageName = imageName;
        this.imagePath = imagePath;
    }

    public String getBase64Image() {
        return base64Image;
    }

    public String getImageName() {
        return imageName;
    }

    public String getImagePath() {
        return imagePath;
    }
}
