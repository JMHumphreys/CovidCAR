plot_WIS_lines <- function(scores_df, by = c("matrix", "date", "location")){



    ggplot() +
      geom_line(data=plot_evals,
                aes(forecast_date, value, group = Model, col = Model), linewidth = 1, linetype = plot_evals$bold) +
      scale_x_date(date_breaks = "1 month",
                   labels=scales::date_format("%Y-%m"),
                   limits = as.Date(c(min(plot_evals$forecast_date),max(plot_evals$forecast_date)))) +
      xlab(" ") +
      ggtitle("Relative Overall Performance (28-day average)") +
      viridis::scale_color_viridis("Model",
                                   discrete=T,
                                   option = "turbo",
                                   direction = 1,
                                   na.value = "white") +
      ylab("Pairwise WIS Score") +
      annotate("text", as_date("2022-10-20"), y=2.5, label= "CFA-CAR (dashed line)", size=6, fontface="bold") +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.background = element_blank(),
            plot.background = element_blank(),
            panel.border = element_blank(),
            legend.title = element_text(size = 16, face = "bold", hjust=0.5),
            legend.text = element_text(size=10, face="bold"),
            strip.text = element_text(size=16, face="bold"),
            strip.background = element_blank(),
            legend.position="bottom", #c(0.7, 0.7),
            legend.direction = "horizontal",
            legend.box = "horizontal",
            legend.key.size = unit(0,"line"),
            legend.key.width = unit(2,"line"),
            axis.text.y = element_text(face="bold", size=19),
            axis.text.x = element_text(face="bold", size=16, vjust=0.5,
                                       hjust=1, angle=90),
            axis.title.x = element_text(size=12, face="bold"),
            axis.title.y = element_text(size=18, face="bold"),
            plot.title = element_text(size=18, face="bold", hjust=0.5)) +
      guides(color = guide_legend(title.position = "top", label.position = "bottom"))

}
