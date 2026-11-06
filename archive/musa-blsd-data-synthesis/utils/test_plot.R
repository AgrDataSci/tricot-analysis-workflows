plot_tree_2(musa_plt_01, log = T , qve = T)


object =  musa_plt_01
qve = TRUE
log = TRUE 
ref = NULL
add.letters = FALSE
ci.level = 0.95

font.size <- NULL
threshold <- NULL
terms     <- NULL
adjust    <- NULL
nudge.x   <- NULL
nudge.y   <- NULL
letter.size <- NULL

ggparty(musa_plt_01, terminal_space = .9) +
  geom_edge() +
  geom_edge_label() +
  # geom_node_label(line_list = list(
  #   aes(label = splitvar),
  #   aes(label = paste("p =", round(p.value, 3))),
  #   aes(label = ""),
  #   aes(label = paste("Node " , id))),
  #   line_gpar = list(list(size = 12,  fontface = "bold"),
  #                    list(size = 12),
  #                    list(size = 12),
  #                    list(size = 12,
  #                         col = "black",
  #                         fontface = "bold",
  #                         alignment = "center")
  #   ),
  #   ids = "inner") +
  #geom_node_label(aes(label = id), ids = "terminal") +
  geom_node_plot(gglist = list(geom_point(mapping = aes(x = estimate, y = items),
                                          data = coeffs),
                               geom_errorbarh(mapping = aes(x = estimate,
                                                            y = items, 
                                                            xmin = bmin,
                                                            xmax = bmax),
                                              color = "black",
                                              height = 0.25,
                                              data = coeffs),
                               geom_vline(xintercept = xinter, 
                                          colour = "darkgray", size = 0.7)),
                 shared_legend = T)







 
term_mnode_plot <- ggplot2::ggplot(coeffs, 
                                   ggplot2::aes(x = estimate, 
                                                y = items)) +
  ggplot2::geom_vline(xintercept = xinter, 
                      colour = "darkgray", size = 0.7) +
  ggplot2::geom_point(pch = 18, size = 2.5, 
                      fill = "black",colour = "black") +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = bmin,
                                       xmax = bmax),
                          colour="black", height = 0.1) +
  ggplot2::facet_grid(. ~ node) +
  ggplot2::labs(x = "log(worth)", y = "") 



tree <- 
  ggparty::ggparty(object, terminal_space = 0) +
  ggparty::geom_edge() +
  ggparty::geom_edge_label(mapping = aes(label = unlist(rnd_labels))) +
 
  ggparty::geom_node_label(line_list = list(
    aes(label = splitvar),
    aes(label = paste("p =", round(p.value, 3))),
    
    aes(label = paste("Node " , id))),
    line_gpar = list(list(size = 11,  fontface = "bold"),
                     list(size = 11),
                     list(size = 11,
                          col = "black",
                          #fontface = "bold",
                          alignment = "center")
    ),
    ids = "inner") +
  ggplot2::coord_cartesian(ylim = c(0.1, 1.1))

tree / term_mnode_plot + plot_layout(ncol = 1, heights = c(1, 3))












