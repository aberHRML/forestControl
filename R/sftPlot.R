#' Plot Selection Frequency Threshold
#'
#'
#' @param x a randomForest of ranger forest object
#' @return a plot
#'
#' @author Tom Wilson \email{tpw2@aber.ac.uk}
#' @export
#' @importFrom ggplot2 ggplot aes geom_point theme_classic guides xlab ylab element_text unit theme

sftPlot <- function(x)
  {

  alpha_range <- seq(from  = 0.00, to = 0.95, by = 0.1)
  alpha_range[1] <- 0.01

  thresh <- NULL
  for(i in seq_along(alpha_range)){
    thresh[[i]] <- sft(x, alpha = alpha_range[i])$sft
  }

  alpha_df <- data.frame(thresh, alpha_range = 100 * alpha_range)
  alpha_df2 <- alpha_df
  alpha_df2 <- alpha_df2[c(1,2,3,10),]
  lab <- paste0("SFT = ", alpha_df2$thresh," , ", "FPR = ", alpha_df2$alpha_range, " %")
  alpha_df2 <- data.frame(alpha_df2, lab = lab)

  ggplot(data = alpha_df, aes(x = alpha_range, y = thresh)) + geom_point() +
    ggrepel::geom_label_repel(data = alpha_df2, aes(x = alpha_range, y = thresh, label = lab, fill = "red", colour = "red"),
                     fontface = 'bold', color = 'white', size = 3,
                     box.padding = unit(0.35, "lines"),
                     point.padding = unit(1, "lines"),
                     segment.color = 'grey50') +  theme_classic() +
    xlab("False Positive Rate (%)") + ylab("Selection Frequency Threshold") +
    theme(axis.text.x= element_text(face = "bold", colour = "black", size = 10)) +
    theme(axis.text.y= element_text(face = "bold", colour = "black", size = 10)) +
    guides(fill = "none")



    }
