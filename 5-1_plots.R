###################
## Do the plots ##
##################

ifelse(!require(gridExtra), install.packages("gridExtra"), require(gridExtra))

# check plots
plt_sal
plt_thema
plt_pos_new
plt_act_new

# extract legend from position plot
g_legend <- function(a_gplot){
  tmp <- ggplot_gtable(ggplot_build(a_gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

the_legend <- g_legend(plt_pos_new)

# build grid plot
p_grid <- grid.arrange(arrangeGrob(plt_sal + theme(legend.position = "none"),
                                   plt_thema + theme(legend.position = "none"),
                                   plt_pos_new + theme(legend.position = "none"),
                                   plt_act_new + theme(legend.position = "none"),
                                   nrow = 2),
                       the_legend, nrow = 2, heights = c(10, 1))

# save grid plot
ggsave(p_grid, file = "plots/grid_final_new.pdf", device = "pdf", width = 11, height = 9)
ggsave(p_grid, file = "plots/grid_final_new.png", device = "png", width = 11, height = 9)
ggsave(p_grid, file = "plots/grid_final_new.tif", device = "tiff", width = 11, height = 9, limitsize = TRUE)
