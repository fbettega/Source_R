source(file = "source/source_francois.R")
require(tidyverse,stingr)

eval_decoupage_cluster_iris <- function_box_plot_expo_func(donne = iris, # data_frame
                                                      expo = "Species", # ,nom entre quote de la colones qui définis tes groupes 
                                                      out_come = "Sepal.Length") # nom entre quote  de la colones qui définis la variables utilisé pour les boxplot
eval_decoupage_cluster_iris$plot_test


# code générant l'exemple
# pdf("exemples/box_plot_signif_group_adjacent/exemple_box_plot.pdf") 
# eval_decoupage_cluster_iris$plot_test
# dev.off() 
