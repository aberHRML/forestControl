#' @rdname plot_sft
#' @export

plot_sft.randomForest <- function(x)
  {

  alpha_range <- seq(from  = 0.00, to = 0.95, by = 0.1)
  alpha_range[1] <- 0.01

  thresh <- NULL
  for(i in seq_along(alpha_range)){
    thresh[[i]] <- sft(x, alpha = alpha_range[i])$sft
  }

  tmp <- data.frame(thresh, alpha_range = 100 * alpha_range)
  tmp2 <- tmp
  tmp2 <- tmp2[c(1,2,3,10),]
  lab <- paste0("SFT = ", tmp2$thresh," , ", "FPR = ", tmp2$alpha_range, " %")
  tmp2 <- data.frame(tmp2, lab = lab)

  ggplot(data = tmp, aes(x = alpha_range, y = thresh)) + geom_point() +
    geom_label_repel(data = tmp2, aes(x = alpha_range, y = thresh, label = lab, fill = "red", colour = "red"),
                     fontface = 'bold', color = 'white', size = 3,
                     box.padding = unit(0.35, "lines"),
                     point.padding = unit(1, "lines"),
                     segment.color = 'grey50') +  theme_classic() +
    xlab("False Positive Rate (%)") + ylab("Selection Frequency Threshold") +
    theme(axis.text.x= element_text(face = "bold", colour = "black", size = 10)) +
    theme(axis.text.y= element_text(face = "bold", colour = "black", size = 10)) +
    guides(fill = "none")



    }

#' @rdname plot_sft
#' @export
plot_sft.ranger <- plot_sft.randomForest

