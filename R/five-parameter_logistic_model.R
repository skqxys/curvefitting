#' @title Fitting Five-parameter logistic models and Ploting
#' @param stan dataFrame for fitting five-parameter logistic models
#' @param res numeric vector used for prediction
#' @param stan.x the name used for x axis of the fitting plot
#' @param stan.y the name used for y axis of the fitting plot
#' @param predict TRUE or FALSE
#' @param predict.inverse TRUE or FALSE
#' @param toppt TRUE or FALSE
#' @param ppt.fw figure width in ppt
#' @param ppt.fh figure height in ppt
#' @description Fitting data with non-linear five-parameter logistic model.
#' Plotting fitting curve and export to ppt
#' The (inverse) prediction for the new data is provided.
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 theme_set
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 element_text
#' @importFrom minpack.lm nlsLM
#' @importFrom eoffice topptx
#' @return a list containing fit and plot
#' @author Pan Gao
#' @export

fivePL<-function(stan, res, stan.x = NULL, stan.y = NULL, toppt = FALSE,
                 ppt.fw = 5, ppt.fh = 4, predict = FALSE,
                 predict.inverse = TRUE) {
  if (is.null(stan.x)) {
    stan.x <- colnames(stan)[1]
  }
  if (is.null(stan.y)) {
    stan.y <- colnames(stan)[2]
  }
  # qpcr<-read.delim("../COVID19/qPCR/qPCR-IAV standard.txt")
  M.5pl <- function(x, d.5pl, a.5pl, c.5pl, b.5pl, g.5pl){
    f <- d.5pl + ((a.5pl - d.5pl)/
                    (1 + (x / c.5pl)^b.5pl)^g.5pl)
    return(f)
  }
  #small.x.asymp
  d.5pl = min(stan[, stan.y]) / 1.2
  #inf.x.asymp
  a.5pl = max(stan[, stan.y]) * 1.2
  #c.5pl
  c.5pl = max(stan[, stan.x]) / 2
  # hill: Rate of change, slope, “slope factor” or “Hill slope”. In the case of linear regression, the units of b are x-units per y-unit.
  b.5pl = -0.5
  # g.5pl
  g.5pl = 0.5
  start.ocon.5pl <- c(d.5pl = d.5pl, a.5pl = a.5pl, c.5pl = c.5pl,
                      b.5pl = b.5pl, g.5pl = g.5pl)
  xlabel <- colnames(stan)[1]
  colnames(stan)[1] <- "xx"
  fit <- nlsLM(stan[, stan.y] ~ M.5pl(xx,
                                      d.5pl, a.5pl, c.5pl, b.5pl, g.5pl),
               data = stan, start = start.ocon.5pl)
  param.5pl <- summary(fit)$parameters[, 1]
  d.5pl = as.vector(param.5pl[1])
  a.5pl = as.vector(param.5pl[2])
  c.5pl = as.vector(param.5pl[3])
  b.5pl = as.vector(param.5pl[4])
  g.5pl = as.vector(param.5pl[5])
  M.5pl <- function(x, d.5pl, a.5pl, c.5pl, b.5pl, g.5pl){
    f <- d.5pl + ((a.5pl - d.5pl)/
                    (1 + (x / c.5pl)^b.5pl)^g.5pl)
    return(f)
  }
  theme_set(theme_bw())
  p <- data.frame(x = seq(min(stan[, 1]),
                          max(stan[, 1]), by = 0.02),
                  y = M.5pl(seq(min(stan[, 1]),
                                max(stan[, 1]), by = 0.02),
                        d.5pl, a.5pl, c.5pl, b.5pl, g.5pl)) %>%
    ggplot(aes(x = x, y = y)) + geom_line() +
    geom_point(aes(x = stan[, 1], y = stan[, 2]), data = stan) +
    labs(title = "Five-parameter logistic models") +
    xlab(label = xlabel) + ylab(label = stan.y)
  if (isTRUE(toppt)) {
    topptx(figure = p, filename = "Five-parameter logistic fitting.pptx",
           width = ppt.fw, height = ppt.fh)
  }
  print(fit)
  if (isTRUE(predict.inverse)) {
    Inv.5pl <- function(y, d.5pl, a.5pl, c.5pl, b.5pl, g.5pl){
      f <- c.5pl * (((a.5pl - d.5pl) / (y - d.5pl))^(1 / g.5pl) - 1)^(1 / b.5pl)
      return(f)
    }   #####fit7###
    pre.inv <- Inv.5pl(res, d.5pl, a.5pl, c.5pl, b.5pl, g.5pl)
  }
  if (isTRUE(predict)) {
    pre <- M.5pl(res, d.5pl, a.5pl, c.5pl, b.5pl, g.5pl)
  }
  if (predict.inverse == FALSE & predict == TRUE) {
    predict_fit <- cbind.data.frame(res, pre)
    Fit5pl <- list(predict = predict_fit, plot = p)
    return(Fit5pl)
  }
  if (predict.inverse == TRUE & predict == FALSE) {
    predict_fit <- cbind.data.frame(res, pre.inv)
    Fit5pl <- list(predict = predict_fit, plot = p)
    return(Fit5pl)
  }
  if (predict.inverse == TRUE & predict == TRUE) {
    predict_fit <- cbind.data.frame(res, pre, pre.inv)
    Fit5pl <- list(predict = predict_fit, plot = p)
    return(Fit5pl)
  }
  if (predict.inverse == FALSE & predict == FALSE) {
    return(p)
  }
}

