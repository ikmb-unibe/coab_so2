###################
## Do the plots ##
##################

ifelse(!require(gridExtra), install.packages("gridExtra"), require(gridExtra))

# check plots
plt_sal
plt_thema
plt_pos_share
plt_pos_div
plt_act_share
plt_act_div

# extract legend from position plot
g_legend <- function(a_gplot){
  tmp <- ggplot_gtable(ggplot_build(a_gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# build grid plot continuos spillovers
the_legend <- g_legend(plt_pos_share)
p_grid <- grid.arrange(arrangeGrob(plt_sal + theme(legend.position = "none"),
                                   plt_thema + theme(legend.position = "none"),
                                   plt_pos_share + theme(legend.position = "none"),
                                   plt_pos_div + theme(legend.position = "none"),
                                   plt_act_share + theme(legend.position = "none"),
                                   plt_act_div + theme(legend.position = "none"),
                                   nrow = 3),
                       the_legend, nrow = 2, heights = c(10, 1))

# save grid plot
ggsave(p_grid, file = "plots/grid_final_rev.pdf", device = "pdf", width = 11, height = 13.5)
ggsave(p_grid, file = "plots/grid_final_rev.png", device = "png", width = 11, height = 13.5)
ggsave(p_grid, file = "plots/grid_final_rev.tif", device = "tiff", width = 11, height = 13.5, limitsize = TRUE)
