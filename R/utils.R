draw_anno_regions = function(region = SPECTRA_REGIONS,
                             gp0 = ggplot(),
                             print = TRUE
){
  rect_data = cbind(region, ymin = -Inf, ymax = Inf)
  h = gp0 +
    geom_rect(data = rect_data,
              aes(xmin = wave_min, xmax = wave_max,
                  ymin = ymin, ymax = ymax))
  if (print) print(h)
  return(h)
}
