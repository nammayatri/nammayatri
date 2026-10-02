/*
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
*/
package in.juspay.mobility.app.carousel;

import android.view.View;

import androidx.annotation.NonNull;
import androidx.viewpager2.widget.ViewPager2;

public class VPPageTransformer implements ViewPager2.PageTransformer {
    private final PageTransformer type ;

    public VPPageTransformer(String type) {
        this.type = PageTransformer.valueOf(type);
    }

    @Override
    public void transformPage(@NonNull View page, float position ) {
        switch (type) {
            case ANTI_CLOCK_SPIN : Transformations.AntiClockSpinTransformation(page,position);break;
            case CLOCK_SPIN : Transformations.ClockSpinTransformation(page,position);break;
            case CUBE_IN_DEPTH : Transformations.CubeInDepthTransformation(page, position);break;
            case CUBE_IN_ROTATION : Transformations.CubeInRotationTransformation(page, position);break;
            case CUBE_IN_SCALING : Transformations.CubeInScalingTransformation(page, position);break;
            case CUBE_OUT_DEPTH : Transformations.CubeOutDepthTransformation(page, position);break;
            case CUBE_OUT_ROTATION : Transformations.CubeOutRotationTransformation(page, position);break;
            case CUBE_OUT_SCALING : Transformations.CubeOutScalingTransformation(page, position);break;
            case DEPTH : Transformations.DepthTransformation(page, position);break;
            case FADE_OUT : Transformations.FadeOutTransformation(page, position);break;
            case FAN : Transformations.FanTransformation(page, position);break;
            case FIDGET_SPIN : Transformations.FidgetSpinTransformation(page, position);break;
            case GATE : Transformations.GateTransformation(page, position);break;
            case HINGE : Transformations.HingeTransformation(page, position);break;
            case HORIZONTAL_FLIP : Transformations.HorizontalFlipTransformation(page, position);break;
            case POP : Transformations.PopTransformation(page, position);break;
            case SPINNER : Transformations.SpinnerTransformation(page, position);break;
            case TOSS : Transformations.TossTransformation(page, position);break;
            case VERTICAL_FLIP : Transformations.VerticalFlipTransformation(page, position);break;
            case VERTICAL_SHUT : Transformations.VerticalShutTransformation(page, position);break;
            case ZOOM_OUT : Transformations.ZoomOutTransformation(page, position);break;
            default:break;
        }
    }



    public enum PageTransformer {
        ANTI_CLOCK_SPIN,
        CLOCK_SPIN,
        CUBE_IN_DEPTH,
        CUBE_IN_ROTATION,
        CUBE_IN_SCALING,
        CUBE_OUT_DEPTH,
        CUBE_OUT_ROTATION,
        CUBE_OUT_SCALING,
        DEPTH,
        FADE_OUT,
        FAN,
        FIDGET_SPIN,
        GATE,
        HINGE,
        HORIZONTAL_FLIP,
        POP,
        SPINNER,
        TOSS,
        VERTICAL_FLIP,
        VERTICAL_SHUT,
        ZOOM_OUT
    }
}