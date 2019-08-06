################
# Do the plots #
################

ifelse(!require(gridExtra), install.packages("gridExtra"), require(gridExtra))

plt_sal_r
plt_thema
plt_pos
plt_act

# extract legend
g_legend <- function(a_gplot){
  tmp <- ggplot_gtable(ggplot_build(a_gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

the_legend <- g_legend(plt_pos)

# build plot
p_grid <- grid.arrange(arrangeGrob(plt_sal_r + theme(legend.position = "none"),
                                   plt_thema + theme(legend.position = "none"),
                                   plt_pos + theme(legend.position = "none"),
                                   plt_act + theme(legend.position = "none"),
                                   nrow = 2),
                       the_legend, nrow = 2, heights = c(10, 1))

ggsave(p_grid, file = "plots/grid_final.pdf", device = "pdf", width = 11, height = 9)
ggsave(p_grid, file = "plots/grid_final.png", device = "png", width = 11, height = 9)
