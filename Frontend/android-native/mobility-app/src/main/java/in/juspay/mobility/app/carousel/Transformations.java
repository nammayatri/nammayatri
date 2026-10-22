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

public class Transformations {
    private static final float MIN_SCALE = 0.65f;
    private static final float MIN_ALPHA = 0.3f;

    public static void AntiClockSpinTransformation(View page, float position) {


        page.setTranslationX(-position * page.getWidth());

        if (Math.abs(position) < 0.5) {
            page.setVisibility(View.VISIBLE);
            page.setScaleX(1 - Math.abs(position));
            page.setScaleY(1 - Math.abs(position));
        } else if (Math.abs(position) > 0.5) {
            page.setVisibility(View.GONE);
        }

        if (position < -1) {  // [-Infinity,-1)
            // This page is way off-screen to the left.
            page.setAlpha(0);

        } else if (position <= 0) {    // [-1,0]
            page.setAlpha(1);
            page.setRotation(360 * (1 - Math.abs(position)));

        } else if (position <= 1) {    // (0,1]
            page.setAlpha(1);
            page.setRotation(-360 * (1 - Math.abs(position)));

        } else {  // (1,+Infinity]
            // This page is way off-screen to the right.
            page.setAlpha(0);

        }
    }

    public static void ClockSpinTransformation(View page, float position) {

        page.setTranslationX(-position * page.getWidth());

        if (Math.abs(position) <= 0.5) {
            page.setVisibility(View.VISIBLE);
            page.setScaleX(1 - Math.abs(position));
            page.setScaleY(1 - Math.abs(position));
        } else if (Math.abs(position) > 0.5) {
            page.setVisibility(View.GONE);
        }


        if (position < -1) {  // [-Infinity,-1)
            // This page is way off-screen to the left.
            page.setAlpha(0);

        } else if (position <= 0) {   // [-1,0]
            page.setAlpha(1);
            page.setRotation(360 * Math.abs(position));

        } else if (position <= 1) {   // (0,1]
            page.setAlpha(1);
            page.setRotation(-360 * Math.abs(position));

        } else {  // (1,+Infinity]
            // This page is way off-screen to the right.
            page.setAlpha(0);

        }
    }

    public static void CubeInDepthTransformation(View page, float position) {

        page.setCameraDistance(20000);


        if (position < -1) {
            page.setAlpha(0);
        } else if (position <= 0) {
            page.setAlpha(1);
            page.setPivotX(page.getWidth());
            page.setRotationY(90 * Math.abs(position));
        } else if (position <= 1) {
            page.setAlpha(1);
            page.setPivotX(0);
            page.setRotationY(-90 * Math.abs(position));
        } else {
            page.setAlpha(0);
        }


        float max = Math.max(.4f, 1 - Math.abs(position));
        if (Math.abs(position) <= 0.5) {
            page.setScaleY(max);
        } else if (Math.abs(position) <= 1) {
            page.setScaleY(max);

        }
    }

    public static void CubeInRotationTransformation(View page, float position) {

        page.setCameraDistance(20000);


        if (position < -1) {     // [-Infinity,-1)
            // This page is way off-screen to the left.
            page.setAlpha(0);

        } else if (position <= 0) {    // [-1,0]
            page.setAlpha(1);
            page.setPivotX(page.getWidth());
            page.setRotationY(90 * Math.abs(position));

        } else if (position <= 1) {    // (0,1]
            page.setAlpha(1);
            page.setPivotX(0);
            page.setRotationY(-90 * Math.abs(position));

        } else {    // (1,+Infinity]
            // This page is way off-screen to the right.
            page.setAlpha(0);

        }
    }

    public static void CubeInScalingTransformation(View page, float position) {
        page.setCameraDistance(20000);


        if (position < -1) {     // [-Infinity,-1)
            // This page is way off-screen to the left.
            page.setAlpha(0);

        } else if (position <= 0) {    // [-1,0]
            page.setAlpha(1);
            page.setPivotX(page.getWidth());
            page.setRotationY(90 * Math.abs(position));

        } else if (position <= 1) {    // (0,1]
            page.setAlpha(1);
            page.setPivotX(0);
            page.setRotationY(-90 * Math.abs(position));

        } else {    // (1,+Infinity]
            // This page is way off-screen to the right.
            page.setAlpha(0);

        }

        if (Math.abs(position) <= 0.5) {
            page.setScaleY(Math.max(.4f, 1 - Math.abs(position)));
        } else if (Math.abs(position) <= 1) {
            page.setScaleY(Math.max(.4f, Math.abs(position)));

        }
    }

    public static void CubeOutDepthTransformation(View page, float position) {
        if (position < -1) {    // [-Infinity,-1)
            // This page is way off-screen to the left.
            page.setAlpha(0);

        } else if (position <= 0) {    // [-1,0]
            page.setAlpha(1);
            page.setPivotX(page.getWidth());
            page.setRotationY(-90 * Math.abs(position));

        } else if (position <= 1) {    // (0,1]
            page.setAlpha(1);
            page.setPivotX(0);
            page.setRotationY(90 * Math.abs(position));

        } else {    // (1,+Infinity]
            // This page is way off-screen to the right.
            page.setAlpha(0);

        }


        float max = Math.max(0.4f, 1 - Math.abs(position));
        if (Math.abs(position) <= 0.5) {
            page.setScaleY(max);
        } else if (Math.abs(position) <= 1) {
            page.setScaleY(max);
        }
    }

    public static void CubeOutRotationTransformation(View page, float position) {
        if (position < -1) {    // [-Infinity,-1)
            // This page is way off-screen to the left.
            page.setAlpha(0);

        } else if (position <= 0) {    // [-1,0]
            page.setAlpha(1);
            page.setPivotX(page.getWidth());
            page.setRotationY(-90 * Math.abs(position));

        } else if (position <= 1) {    // (0,1]
            page.setAlpha(1);
            page.setPivotX(0);
            page.setRotationY(90 * Math.abs(position));

        } else {    // (1,+Infinity]
            // This page is way off-screen to the right.
            page.setAlpha(0);

        }
    }

    public static void CubeOutScalingTransformation(View page, float position) {
        {

            if (position < -1) {    // [-Infinity,-1)
                // This page is way off-screen to the left.
                page.setAlpha(0);

            } else if (position <= 0) {    // [-1,0]
                page.setAlpha(1);
                page.setPivotX(page.getWidth());
                page.setRotationY(-90 * Math.abs(position));

            } else if (position <= 1) {    // (0,1]
                page.setAlpha(1);
                page.setPivotX(0);
                page.setRotationY(90 * Math.abs(position));

            } else {    // (1,+Infinity]
                // This page is way off-screen to the right.
                page.setAlpha(0);

            }


            if (Math.abs(position) <= 0.5) {
                page.setScaleY(Math.max(0.4f, 1 - Math.abs(position)));
            } else if (Math.abs(position) <= 1) {
                page.setScaleY(Math.max(0.4f, Math.abs(position)));
            }

        }
    }

    public static void DepthTransformation(View page, float position) {

        if (position < -1) {    // [-Infinity,-1)
            // This page is way off-screen to the left.
            page.setAlpha(0);

        } else if (position <= 0) {    // [-1,0]
            page.setAlpha(1);
            page.setTranslationX(0);
            page.setScaleX(1);
            page.setScaleY(1);

        } else if (position <= 1) {    // (0,1]
            page.setTranslationX(-position * page.getWidth());
            page.setAlpha(1 - Math.abs(position));
            page.setScaleX(1 - Math.abs(position));
            page.setScaleY(1 - Math.abs(position));

        } else {    // (1,+Infinity]
            // This page is way off-screen to the right.
            page.setAlpha(0);

        }
    }

    public static void FadeOutTransformation(View page, float position) {
        page.setTranslationX(-position * page.getWidth());

        page.setAlpha(1 - Math.abs(position));

    }

    public static void FanTransformation(View page, float position) {
        page.setTranslationX(-position * page.getWidth());
        page.setPivotX(0);
        page.setPivotY(page.getHeight() / 2.0f);
        page.setCameraDistance(20000);

        if (position < -1) {     // [-Infinity,-1)
            // This page is way off-screen to the left.
            page.setAlpha(0);

        } else if (position <= 0) {    // [-1,0]
            page.setAlpha(1);
            page.setRotationY(-120 * Math.abs(position));
        } else if (position <= 1) {    // (0,1]
            page.setAlpha(1);
            page.setRotationY(120 * Math.abs(position));

        } else {    // (1,+Infinity]
            // This page is way off-screen to the right.
            page.setAlpha(0);

        }
    }

    public static void FidgetSpinTransformation(View page, float position) {
        page.setTranslationX(-position * page.getWidth());

        if (Math.abs(position) < 0.5) {
            page.setVisibility(View.VISIBLE);
            page.setScaleX(1 - Math.abs(position));
            page.setScaleY(1 - Math.abs(position));
        } else if (Math.abs(position) > 0.5) {
            page.setVisibility(View.GONE);
        }

        float v = Math.abs(position) * Math.abs(position) * Math.abs(position) * Math.abs(position) * Math.abs(position) * Math.abs(position) * Math.abs(position);
        if (position < -1) {     // [-Infinity,-1)
            // This page is way off-screen to the left.
            page.setAlpha(0);

        } else if (position <= 0) {    // [-1,0]
            page.setAlpha(1);
            page.setRotation(36000 * v);

        } else if (position <= 1) {    // (0,1]
            page.setAlpha(1);
            page.setRotation(-36000 * (v));

        } else {    // (1,+Infinity]
            // This page is way off-screen to the right.
            page.setAlpha(0);

        }
    }

    public static void GateTransformation(View page, float position) {
        page.setTranslationX(-position * page.getWidth());


        if (position < -1) {    // [-Infinity,-1)
            // This page is way off-screen to the left.
            page.setAlpha(0);

        } else if (position <= 0) {    // [-1,0]
            page.setAlpha(1);
            page.setPivotX(0);
            page.setRotationY(90 * Math.abs(position));

        } else if (position <= 1) {    // (0,1]
            page.setAlpha(1);
            page.setPivotX(page.getWidth());
            page.setRotationY(-90 * Math.abs(position));

        } else {    // (1,+Infinity]
            // This page is way off-screen to the right.
            page.setAlpha(0);

        }

    }

    public static void HingeTransformation(View page, float position) {
        page.setTranslationX(-position * page.getWidth());
        page.setPivotX(0);
        page.setPivotY(0);


        if (position < -1) {    // [-Infinity,-1)
            // This page is way off-screen to the left.
            page.setAlpha(0);

        } else if (position <= 0) {    // [-1,0]
            page.setRotation(90 * Math.abs(position));
            page.setAlpha(1 - Math.abs(position));

        } else if (position <= 1) {    // (0,1]
            page.setRotation(0);
            page.setAlpha(1);

        } else {    // (1,+Infinity]
            // This page is way off-screen to the right.
            page.setAlpha(0);

        }
    }

    public static void HorizontalFlipTransformation(View page, float position) {
        page.setTranslationX(-position * page.getWidth());
        page.setCameraDistance(20000);

        if (position < 0.5 && position > -0.5) {
            page.setVisibility(View.VISIBLE);
        } else {
            page.setVisibility(View.INVISIBLE);
        }


        if (position < -1) {     // [-Infinity,-1)
            // This page is way off-screen to the left.
            page.setAlpha(0);

        } else if (position <= 0) {    // [-1,0]
            page.setAlpha(1);
            page.setRotationX(180 * (1 - Math.abs(position) + 1));
        } else if (position <= 1) {    // (0,1]
            page.setAlpha(1);
            page.setRotationX(-180 * (1 - Math.abs(position) + 1));
        } else {    // (1,+Infinity]
            // This page is way off-screen to the right.
            page.setAlpha(0);

        }

    }

    public static void PopTransformation(View page, float position) {
        page.setTranslationX(-position * page.getWidth());
        page.setTranslationX(-position * page.getWidth());

        if (Math.abs(position) < 0.5) {
            page.setVisibility(View.VISIBLE);
            page.setScaleX(1 - Math.abs(position));
            page.setScaleY(1 - Math.abs(position));
        } else if (Math.abs(position) > 0.5) {
            page.setVisibility(View.GONE);
        }

    }

    public static void SpinnerTransformation(View page, float position) {
        page.setTranslationX(-position * page.getWidth());
        page.setTranslationX(-position * page.getWidth());
        page.setCameraDistance(12000);

        if (position < 0.5 && position > -0.5) {
            page.setVisibility(View.VISIBLE);
        } else {
            page.setVisibility(View.INVISIBLE);
        }


        if (position < -1) {     // [-Infinity,-1)
            // This page is way off-screen to the left.
            page.setAlpha(0);

        } else if (position <= 0) {    // [-1,0]
            page.setAlpha(1);
            page.setRotationY(900 * (1 - Math.abs(position) + 1));

        } else if (position <= 1) {    // (0,1]
            page.setAlpha(1);
            page.setRotationY(-900 * (1 - Math.abs(position) + 1));

        } else {    // (1,+Infinity]
            // This page is way off-screen to the right.
            page.setAlpha(0);

        }
    }

    public static void TossTransformation(View page, float position) {
        page.setTranslationX(-position * page.getWidth());
        page.setCameraDistance(20000);


        if (position < 0.5 && position > -0.5) {
            page.setVisibility(View.VISIBLE);

        } else {
            page.setVisibility(View.INVISIBLE);

        }

        float max = Math.max(0.4f, (1 - Math.abs(position)));
        if (position < -1) {     // [-Infinity,-1)
            // This page is way off-screen to the left.
            page.setAlpha(0);

        } else if (position <= 0) {    // [-1,0]
            page.setAlpha(1);
            page.setScaleX(max);
            page.setScaleY(max);
            page.setRotationX(1080 * (1 - Math.abs(position) + 1));
            page.setTranslationY(-1000 * Math.abs(position));

        } else if (position <= 1) {    // (0,1]
            page.setAlpha(1);
            page.setScaleX(max);
            page.setScaleY(max);
            page.setRotationX(-1080 * (1 - Math.abs(position) + 1));
            page.setTranslationY(-1000 * Math.abs(position));

        } else {    // (1,+Infinity]
            // This page is way off-screen to the right.
            page.setAlpha(0);

        }
    }

    public static void VerticalFlipTransformation(View page, float position) {
        page.setTranslationX(-position * page.getWidth());
        page.setCameraDistance(12000);

        if (position < 0.5 && position > -0.5) {
            page.setVisibility(View.VISIBLE);
        } else {
            page.setVisibility(View.INVISIBLE);
        }


        if (position < -1) {     // [-Infinity,-1)
            // This page is way off-screen to the left.
            page.setAlpha(0);

        } else if (position <= 0) {    // [-1,0]
            page.setAlpha(1);
            page.setRotationY(180 * (1 - Math.abs(position) + 1));

        } else if (position <= 1) {    // (0,1]
            page.setAlpha(1);
            page.setRotationY(-180 * (1 - Math.abs(position) + 1));

        } else {    // (1,+Infinity]
            // This page is way off-screen to the right.
            page.setAlpha(0);

        }
    }

    public static void VerticalShutTransformation(View page, float position) {
        page.setTranslationX(-position * page.getWidth());
        page.setCameraDistance(999999999);

        if (position < 0.5 && position > -0.5) {
            page.setVisibility(View.VISIBLE);
        } else {
            page.setVisibility(View.INVISIBLE);
        }


        if (position < -1) {     // [-Infinity,-1)
            // This page is way off-screen to the left.
            page.setAlpha(0);

        } else if (position <= 0) {    // [-1,0]
            page.setAlpha(1);
            page.setRotationX(180 * (1 - Math.abs(position) + 1));

        } else if (position <= 1) {    // (0,1]
            page.setAlpha(1);
            page.setRotationX(-180 * (1 - Math.abs(position) + 1));

        } else {    // (1,+Infinity]
            // This page is way off-screen to the right.
            page.setAlpha(0);

        }
    }

    public static void ZoomOutTransformation(View page, float position) {
        if (position < -1) {  // [-Infinity,-1)
            // This page is way off-screen to the left.
            page.setAlpha(0);

        } else if (position <= 1) { // [-1,1]

            float max = Math.max(MIN_SCALE, 1 - Math.abs(position));
            page.setScaleX(max);
            page.setScaleY(max);
            page.setAlpha(Math.max(MIN_ALPHA, 1 - Math.abs(position)));

        } else {  // (1,+Infinity]
            // This page is way off-screen to the right.
            page.setAlpha(0);

        }
    }
}
