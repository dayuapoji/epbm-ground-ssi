library(plotly)
# library(car)
# library(rgl)


mirror <- result_df %>% 
  # remove center line (distance to alignment = 0)
  .[.$DistanceAlignment != 0, ] %>%
  # inverse sign for mirroring
  mutate(DistanceAlignment = -1 * DistanceAlignment)

full_model <- rbind(result_df, mirror)

plot_ly(x = full_model$ChainagePoint, 
        y = full_model$DistanceAlignment, 
        z = full_model$PredRF,
        color = full_model$PredRF,
        type = 'scatter3d',
        size = 0.5,
        colors = colorRamp(c("red", "yellow", "chartreuse3","lightblue", "blue"))) #%>%
  layout(xaxis = list(range = c(26000, 29000)),
         yaxis = list(range = c(-20, 20)),
         zaxis = list(range = c(5, -15)))

?plot_ly

## x coordinates
xSeq = seq(0, 1, 0.01)
## y coordinates
ySeq = seq(0, 1, 0.01)
## list with x, y coordinates
z = matrix(xSeq + ySeq, nrow = length(xSeq), ncol = length(ySeq))

fig = plot_ly(z = ~z) %>% add_surface()
fig

kd <- with(MASS::geyser, MASS::kde2d(duration, waiting, n = 50))
fig <- plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% add_surface()
fig
