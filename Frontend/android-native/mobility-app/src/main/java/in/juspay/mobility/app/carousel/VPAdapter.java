package in.juspay.mobility.app.carousel;

import android.content.res.ColorStateList;
import android.content.res.Resources;
import android.graphics.Color;
import android.graphics.drawable.GradientDrawable;
import android.text.Html;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import org.json.JSONObject;
import java.util.ArrayList;

import in.juspay.mobility.app.R;

public class VPAdapter extends RecyclerView.Adapter<VPAdapter.ViewHolder> {
    ArrayList<ViewPagerItem> viewPagerItemArrayList;
    public VPAdapter(ArrayList<ViewPagerItem> viewPagerItemArrayList) {
        this.viewPagerItemArrayList = viewPagerItemArrayList;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.viewpager_item,parent,false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        try{
            ViewPagerItem viewPagerItem = viewPagerItemArrayList.get(position);
            JSONObject imageConfig = viewPagerItem.imageConfig;
            JSONObject descriptionConfig = viewPagerItem.descriptionConfig ;
            JSONObject titleConfig = viewPagerItem.titleConfig;
            JSONObject margin = viewPagerItem.descriptionConfig.getJSONObject("margin");
            JSONObject titleMargin = viewPagerItem.titleConfig.getJSONObject("margin");
            String titleGravity = titleConfig.getString("gravity");
            String descriptionGravity = descriptionConfig.getString("gravity");

            float density = (Resources.getSystem().getDisplayMetrics().density);

            // imageView Config ------------------------------------------
            holder.imageView.setImageResource(viewPagerItem.imageID);
            holder.imageView.getLayoutParams().height = (int) (imageConfig.getInt("height") * density);
            GradientDrawable gradientDrawable = new GradientDrawable();
            gradientDrawable.setShape(GradientDrawable.RECTANGLE);
            gradientDrawable.setCornerRadii(new float[] {20, 20, 20, 20, 0,0,0,0});
            gradientDrawable.setColor(Color.parseColor(viewPagerItem.imageConfig.getString("bgColor")));
            holder.imageView.setBackground(gradientDrawable);

            // Heading text Config ------------------------------------------
            holder.tvHeading.setTextSize(titleConfig.getInt("textSize"));
            holder.tvHeading.setTextColor(Color.parseColor(titleConfig.getString("textColor")));
            holder.tvHeading.setText(titleConfig.getString("text"));
            ViewGroup.MarginLayoutParams titleLayoutParams = (ViewGroup.MarginLayoutParams) holder.tvHeading.getLayoutParams();
            titleLayoutParams.setMargins(titleMargin.getInt("left"), titleMargin.getInt("top"), titleMargin.getInt("right"), titleMargin.getInt("bottom"));
            holder.tvHeading.setLayoutParams(titleLayoutParams);
            holder.tvHeading.setGravity(getGravity(titleGravity));

            // Description text Config ---------------------------------------
            holder.tvDesc.setText(Html.fromHtml(descriptionConfig.getString("text")));
            holder.tvDesc.setTextSize(descriptionConfig.getInt("textSize"));
            holder.tvDesc.setTextColor(Color.parseColor(descriptionConfig.getString("textColor")));
            ViewGroup.MarginLayoutParams descLayoutParams = (ViewGroup.MarginLayoutParams) holder.tvDesc.getLayoutParams();
            descLayoutParams.setMargins(margin.getInt("left"), margin.getInt("top"), margin.getInt("right"), margin.getInt("bottom"));
            holder.tvDesc.setLayoutParams(descLayoutParams);
            holder.tvDesc.setGravity(getGravity(descriptionGravity));

          }
            catch (Exception e){

        }
    }
    private int getGravity(String gravity){
        switch (gravity){
            case "LEFT": return Gravity.LEFT;
            case "RIGHT" : return Gravity.RIGHT;
            case "TOP" :  return Gravity.TOP;
            case "BOTTOM" : return Gravity.BOTTOM;
            default: return Gravity.CENTER;}
    }
    @Override
    public int getItemCount() {
        return viewPagerItemArrayList.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{
        ImageView imageView;
        TextView tvHeading, tvDesc;
        public ViewHolder(@NonNull View itemView) {
            super(itemView);
            imageView = itemView.findViewById(R.id.ivimage);
            tvHeading = itemView.findViewById(R.id.tvHeading);
            tvDesc = itemView.findViewById(R.id.tvDesc);
        }
    }

}