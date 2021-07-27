# modified from
# https://github.com/tidyverse/ggplot2/blob/master/R/stat-boxplot.r
# now takes qs argument instead of coef to extend the whiskers to a specific 
# percentile

#' A box and whiskers plot that uses percentiles instead of IQR
#' 
#' `stat_boxplot_custom()` modifies [ggplot2::stat_boxplot()] so that it 
#' computes the extents of the whiskers based on specified percentiles, rather 
#' than a multiple of the IQR. 
#' 
#' The upper and lower whiskers extend to the first, and last entries of the 
#' `qs` parameter, respectively. Data beyond the whiskers are "outliers".
#' 
#' @param qs The percentiles that are used to create the lower whisker, lower
#'   hinge, middle bar, upper hinge, and upper whisker, respectively. The lower 
#'   and upper whiskers default to the 5th and 95th percentiles. The hinges 
#'   default to the 25th and 75th percentiles, and the middle bar defaults to 
#'   the median. These  values should be in increasing order, i.e., the whisker 
#'   should be a smaller percentile than the lower hinge, and can only span the 
#'   values from 0 to 1 (inclusive).
#'   
#' @inheritParams ggplot2::stat_boxplot
#' 
#' @examples 
#' 
#' library(ggplot2)
#' p <- ggplot(mpg, aes(class, hwy))
#' 
#' # show whiskers at 5th and 95th percentiles
#' p + stat_boxplot_custom(qs = c(.05, .25, .5, .75, .95))
#' 
#' # show whiskers at 10th and 90th percentiles
#' p + stat_boxplot_custom(qs = c(.1, .25, .5, .75, .9))
#' 
#' @export