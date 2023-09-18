package in.juspay.mobility.app.carousel;

import android.content.res.ColorStateList;
import android.content.res.Resources;
import android.graphics.Color;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

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
        ViewPagerItem viewPagerItem = viewPagerItemArrayList.get(position);
        holder.imageView.setImageResource(viewPagerItem.imageID);
        float density = (Resources.getSystem().getDisplayMetrics().density);
        System.out.println("Inside imageHeight :: " + viewPagerItem.imageHeight);
        holder.imageView.getLayoutParams().height = (int) (viewPagerItem.imageHeight * density);
        holder.tvHeading.setTextSize(viewPagerItem.titleTextSize);
        holder.tvHeading.setTextColor(Color.parseColor(viewPagerItem.titleColor));
        holder.tvHeading.setText(viewPagerItem.heading);
        holder.tvDesc.setText(viewPagerItem.description);
        holder.tvDesc.setTextSize(viewPagerItem.descriptionTextSize);
        holder.tvDesc.setTextColor(Color.parseColor(viewPagerItem.descriptionColor));
        LinearLayout.LayoutParams layoutParams = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.WRAP_CONTENT);
        ViewGroup.MarginLayoutParams descLayoutParams = (ViewGroup.MarginLayoutParams) holder.tvDesc.getLayoutParams();
//        descLayoutParams.setMargins(0, 8, 0, 16);
        holder.tvDesc.setLayoutParams(descLayoutParams);
        holder.tvDesc.setGravity(Gravity.LEFT);
        ViewGroup.MarginLayoutParams titleLayoutParams = (ViewGroup.MarginLayoutParams) holder.tvDesc.getLayoutParams();
        titleLayoutParams.setMargins(0, 16, 0, 0);
        holder.tvHeading.setLayoutParams(titleLayoutParams);
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