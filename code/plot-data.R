# PLOT DATA FUSION ####


# Multi-Sensor Chainage --------------------------------------------------------

# specify distances
dist_to_center <- 10 # meters
distance <- 6.6 * 2 # 6.6 ft = 2 m (tunnel lining width)

# get chainage list
chainage_list <- NULL
for (chainage in seq(from = 19500, to = 28800, by = distance)) {
  
  num_insar <- insar_data %>% get_mp_num(., chainage, distance, dist_to_center)
  num_arsp <- arsp_data %>% get_mp_num(., chainage, distance, dist_to_center)
  num_mpbx <- mpbx_data %>% get_mp_num(., chainage, distance, dist_to_center)

  if ((num_insar != 0) & (num_arsp != 0) & (num_mpbx != 0)) {
    chainage_list <- c(chainage_list, chainage)
  }
}


# Plot Comparison --------------------------------------------------------------

# set InSAR zero date as benchmark date
benchmark_date <- "2016-05-18 PDT"

for (chainage in chainage_list) {

  # Get df for all monitoring points ----
  
  insar_plot <- insar_data %>% 
    filter(., CenterDistance <= dist_to_center) %>%
    get_mp_region(., chainage, distance) %>%
    get_mp_plot(., benchmark_date, max(tbm_pos$Date))

  arsp_plot <- arsp_data %>% 
    filter(., CenterDistance <= dist_to_center) %>%
    get_mp_region(., chainage, distance) %>%
    get_mp_plot(., benchmark_date, max(tbm_pos$Date))
  
  nssp_plot <- nssp_data %>% 
    filter(., CenterDistance <= dist_to_center) %>%
    get_mp_region(., chainage, distance) %>%
    get_mp_plot(., benchmark_date, max(tbm_pos$Date))
  
  mpbxsr_plot <- mpbx_data %>% 
    filter(., CenterDistance <= dist_to_center) %>%
    filter(., str_sub(ID, -2, -1) == "SR") %>%
    get_mp_region(., chainage, distance) %>%
    get_mp_plot(., benchmark_date, max(tbm_pos$Date))
  
  mpbx_plot <- mpbx_data %>% 
    filter(., CenterDistance <= dist_to_center) %>%
    filter(., str_sub(ID, -2, -1) != "SR") %>%
    get_mp_region(., chainage, distance) %>%
    get_mp_plot(., benchmark_date, max(tbm_pos$Date))
  

  # Get TBM df ----
  
  tbm_plot <- arsp_df %>% 
    .[.$ID == unique(arsp_plot$ID)[1], ] %>%
    mutate(Date = as.POSIXct(Date, format = "%Y%m%d")) %>%
    # remove date before benchmark date
    .[.$Date >= benchmark_date, ] %>% 
    # remove date after end of tunneling
    .[.$Date <= max(tbm_pos$Date), ] 
  
  
  # Plot MP data ----
  fig_data_gs <- ggplot() +
    # plot data
    geom_line(data = rbind(arsp_plot, nssp_plot, mpbxsr_plot, insar_plot), 
              aes(x = Date, y = DeltaValue, color = ID),
              size = 0.5) +
    geom_point(data = rbind(arsp_plot, nssp_plot, mpbxsr_plot, insar_plot),
               aes(x = Date, y = DeltaValue, color = ID),
               size = 0.5) +
    # annotate TBM passing line
    geom_vline(xintercept = tbm_plot[abs(tbm_plot$HeadDistance) == 
                                      min(abs(tbm_plot$HeadDistance)), 'Date'],
               color = 'black',
               linetype = 'dashed') +
    annotate(geom = 'text',
             label = "EPBM Head Passing",
             x = tbm_plot[abs(tbm_plot$HeadDistance) == 
                           min(abs(tbm_plot$HeadDistance)), 'Date'],
             y = min(rbind(insar_plot, arsp_plot, nssp_plot, mpbxsr_plot)$Value),
             angle = 90, hjust = 0, vjust = 1.5) +
    # figure setting
    labs(colour = '') +
    ggtitle(paste("Chainage =", chainage)) +
    ylab("Surface Ground Deformation (mm)") +
    theme_bw(base_size = 7) + 
    theme(legend.position = 'top',
          legend.key.size = unit(4, 'mm'),
          axis.title.x = element_blank(),
          axis.text.x = element_blank())
  
  fig_data_ug <- ggplot() +
    # plot data
    geom_line(data = mpbx_plot, 
              aes(x = Date, y = DeltaValue, color = ID),
              size = 0.5) +
    geom_point(data = mpbx_plot,
               aes(x = Date, y = DeltaValue, color = ID),
               size = 0.5) +
    # annotate TBM passing line
    geom_vline(xintercept = tbm_plot[abs(tbm_plot$HeadDistance) == 
                                       min(abs(tbm_plot$HeadDistance)), 'Date'],
               color = 'black',
               linetype = 'dashed') +
    # figure setting
    labs(colour = '') +
    ylab("Subsurface Ground Deformation (mm)") +
    theme_bw(base_size = 7) + theme(legend.position = 'top',
                                    legend.key.size = unit(4, 'mm'))
  
  # Plot MP locations ----
  
  fig_mp1 <- ggplot() +
    # TBM route
    geom_point(data = tbm_pos,  aes(x = Long, y = Lat, color = 'Tunnel'),
               size = 0.1, color = 'black', alpha = 0.5) +
    # MPs
    geom_point(data = rbind(insar_plot, arsp_plot, nssp_plot, mpbxsr_plot),
               aes(x = Long, y = Lat, color = ID),
               size = 0.25) +
    
    xlab('Long') + ylab('Lat') +
    xlim(-122.3475, -122.3325) +
    coord_equal() +
    theme_bw(base_size = 7) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = 'none')
  
  fig_mp2 <- ggplot() +
    # TBM route
    geom_point(data = tbm_pos,  aes(x = Long, y = Lat, color = 'Tunnel'),
               size = 0.5, color = 'black', ) +
    geom_line(data = tbm_pos,  aes(x = Long, y = Lat, color = 'Tunnel'),
               size = 0.5, color = 'black', ) +
    # MPs
    geom_point(data = rbind(insar_plot, arsp_plot, nssp_plot, mpbxsr_plot),
               aes(x = Long, y = Lat, color = ID),
               size = 1.5) +
    
    xlab('Long') + ylab('Lat') +
    xlim(min(rbind(insar_plot, arsp_plot, nssp_plot, mpbxsr_plot)$Long)-0.0001,
         max(rbind(insar_plot, arsp_plot, nssp_plot, mpbxsr_plot)$Long)+0.0001) +
    ylim(min(rbind(insar_plot, arsp_plot, nssp_plot, mpbxsr_plot)$Lat)-0.0001,
         max(rbind(insar_plot, arsp_plot, nssp_plot, mpbxsr_plot)$Lat)+0.0001) +
    theme_bw(base_size = 7) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = 'none')
  
  # Combine figures ----
  
  fig_data <- ggarrange(fig_data_gs, fig_data_ug, nrow = 2)
  fig_mp <- ggarrange(fig_mp1, fig_mp2, nrow = 2)
  
  
  # Save as PDF ----
  pdf(paste0("../figs/",chainage,".pdf"), width = 10, height  = 10/2)
  grid.arrange(fig_data, fig_mp, ncol = 2, widths = c(4, 1))
  dev.off()
}


