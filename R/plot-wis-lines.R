plot_WIS_lines <- function(scores_df, by = c("tile", "date"), range = c("abs","scaled"), scale_model = NULL){

  if(range == "scaled" & is.character(scale_model) == TRUE){

    scaler_df <- scores_df %>%
      filter(model == paste0(scale_model)) %>%
      mutate(scale_val = WIS) %>%
      select(date, location_name, forecast_date, scale_val)

    scores_df <- left_join(scores_df, scaler_df, by =c("date", "location_name", "forecast_date"))

    scores_df <- scores_df %>%
      mutate(s.WIS = WIS/scale_val) %>%
      select(-WIS) %>%
      mutate(WIS = s.WIS) %>%
      select(-s.WIS)
  }


  if(by == "date"){

    scores_grp <- scores_df %>%
      group_by(date, model) %>%
      summarise(WIS = mean(WIS))


    scores_grp$x <- scores_grp$date
    scores_grp$y <- scores_grp$WIS

    plot_x_axis <- scale_x_date(date_breaks = "1 week",
                 limits = as.Date(c(min(scores_grp$date),max(scores_grp$date))))

    base_line = ggplot() +
      geom_line(data=scores_grp,
                aes(x, y, group = model, col = model), linewidth = 0.75, linetype = "solid") +
      plot_x_axis +
      xlab(" ") +
      viridis::scale_color_viridis("Model",
                                   discrete=T,
                                   option = "turbo",
                                   direction = 1,
                                   na.value = "white") +
      ylab("WIS Score") +
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

  if(by == "tile"){

    scores_df$Abrv = with(States@data,
                          stusab[match(
                            scores_df$location_name,
                                      state)])

    base_line = ggplot() +
      geom_tile(data=scores_df,
                aes(date, Abrv, fill = WIS)) +
      xlab(" ") +
      viridis::scale_fill_viridis(paste0("Scaled WIS (", paste0(scale_model),")"),
                                   discrete=F,
                                   option = "turbo",
                                   direction = -1,
                                   na.value = "white") +
      ylab("Location") +
      facet_wrap(~model, ncol = 4) +
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
            #legend.key.size = unit(0,"line"),
            #legend.key.width = unit(2,"line"),
            axis.text.y = element_text(face="bold", size=10),
            axis.text.x = element_text(face="bold", size=16, vjust=0.5,
                                       hjust=1, angle=90),
            axis.title.x = element_text(size=12, face="bold"),
            axis.title.y = element_text(size=18, face="bold"),
            plot.title = element_text(size=18, face="bold", hjust=0.5)) +
      guides(color = guide_legend(title.position = "top", label.position = "bottom"))

  }

  plot(base_line)
}


