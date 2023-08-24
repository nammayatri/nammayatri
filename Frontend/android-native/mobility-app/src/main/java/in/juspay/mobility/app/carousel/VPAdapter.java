package in.juspay.mobility.app.carousel;

import android.content.Context;
import android.content.res.Resources;
import android.graphics.Color;
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
    Context context;
    ArrayList<ViewPagerItem> viewPagerItemArrayList;
    public VPAdapter(ArrayList<ViewPagerItem> viewPagerItemArrayList) {
        this.viewPagerItemArrayList = viewPagerItemArrayList;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        context = parent.getContext();
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.viewpager_item,parent,false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        ViewPagerItem viewPagerItem = viewPagerItemArrayList.get(position);
        holder.imageView.setImageResource(viewPagerItem.imageID);
        holder.imageView.getLayoutParams().height = (Resources.getSystem().getDisplayMetrics().heightPixels)/3;
        holder.tvHeading.setText(viewPagerItem.heading);
        holder.tvDesc.setText(viewPagerItem.description);
//        LinearLayout linearLayout = (LinearLayout) holder.view;
//        linearLayout.addView();
        LinearLayout layout = ((LinearLayout) holder.view);
//        LinearLayout viewToAdd = viewPagerItem.view;
//        viewToAdd.setBackgroundColor(Color.GREEN);

//        holder.view = layout;


//        layout.removeAllViews();
//        if (layout !=null && viewToAdd != null){
//            layout.addView(viewToAdd);
//        }

////        if (holder.view != null){
//        LinearLayout.LayoutParams linearParams = new LinearLayout.LayoutParams(
//                LinearLayout.LayoutParams.MATCH_PARENT,
//                LinearLayout.LayoutParams.WRAP_CONTENT);
//        LinearLayout linearLayout = new LinearLayout(context);
//        linearLayout.setLayoutParams(linearParams);
//
//        linearLayout.addView(viewPagerItem.view);
//        layout.addView(linearLayout);

//        LinearLayout.LayoutParams linearParamsChild = new LinearLayout.LayoutParams(
//                LinearLayout.LayoutParams.MATCH_PARENT,
//                LinearLayout.LayoutParams.WRAP_CONTENT);
//        LinearLayout linearLayoutChild = new LinearLayout(holder.view.getContext());
//        linearLayoutChild.setLayoutParams(linearParamsChild);
//        linearParamsChild.weight = 1.0f;
//
//            holder.view.setVisibility(View.VISIBLE);
//        }else {
//            holder.view.setVisibility(View.GONE);
//        }
    }

    @Override
    public int getItemCount() {
        return viewPagerItemArrayList.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{
        ImageView imageView;
        TextView tvHeading, tvDesc;
        LinearLayout view;
        public ViewHolder(@NonNull View itemView) {
            super(itemView);
            imageView = itemView.findViewById(R.id.ivimage);
            tvHeading = itemView.findViewById(R.id.tvHeading);
            tvDesc = itemView.findViewById(R.id.tvDesc);
//            view = itemView.findViewById(R.id.tvview);
        }
    }

}