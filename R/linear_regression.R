#' @title Fitting Linear Models and Plotting
#' @param stan dataFrame for fitting linear polynomial regression
#' @param res numeric vector used for prediction
#' @param stan.x the name used for x axis of the fitting plot
#' @param stan.y the name used for y axis of the fitting plot
#' @param predict TRUE or FALSE
#' @param predict.inverse TRUE or FALSE
#' @param toppt TRUE or FALSE
#' @param ppt.fw figure width in ppt
#' @param ppt.fh figure height in ppt
#' @description Fitting data with linear polynomial regression.
#' Plotting fitting curve and export to ppt.
#' The (inverse) prediction for new data
#' @importFrom stats lm
#' @importFrom stats coef
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 theme_set
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 element_text
#' @importFrom eoffice topptx
#' @return a list containing fit and plot
#' @author Pan Gao
#' @export

linear <- function (stan, res, stan.x=NULL, stan.y=NULL, ppt.fw = 5, ppt.fh = 4,
                    toppt = FALSE, predict = TRUE, predict.inverse = FALSE) {
  if (is.null(stan.x)) {
    stan.x <- colnames(stan)[1]
  }
  if (is.null(stan.y)) {
    stan.y <- colnames(stan)[2]
  }
  fit <- lm(formula = stan[,stan.y] ~ stan[, stan.x], data = stan)
  label <- data.frame(formula = sprintf(" italic(y) == %.2f %+.2f * italic(x)",
                                        round(coef(fit)[1], 2),
                                        round(coef(fit)[2], 2)),
                      r2 = sprintf(" italic(R^2) == %.2f",
                                   round(summary(fit)$r.squared, 2)),
                      rse = paste(" Residual standard error:",
                                  round(summary(fit)$sigma, 2)),
                      stringsAsFactors = FALSE)
  cat("Linear regression")
  print(fit)
  theme_set(theme_bw())
  p<-ggplot(data = stan, aes(x = stan[, stan.x], y = stan[, stan.y])) +
    geom_point() + geom_smooth(method = lm, formula = y ~ x,se = F) +
    xlab(label = stan.x) + ylab(label = stan.y) +
    labs(title = "Linear regression") + theme(title = element_text(size = 8)) +
    geom_text(data = label, aes(x = -Inf, y = 8*max(stan[, stan.y])/10,
                                label = formula),
              hjust = 0, vjust = 1, parse = TRUE,
              inherit.aes = FALSE, size = 2.5) +
    geom_text(data = label, aes(x = -Inf, y = 7*max(stan[, stan.y])/10,
                                label = r2),
              hjust = 0, vjust = 1, parse = TRUE,
              inherit.aes = FALSE, size = 2.5)
    # geom_text(data = label, aes(x = -Inf, y = 6*max(stan[, stan.y])/10, label = rse),
    #           hjust = 0, vjust = 1,
    #           inherit.aes = FALSE, size = 2.5)
  print(p)
  if (isTRUE(toppt)) {
    topptx(figure = p, filename = "Linear regression.pptx",
           width = ppt.fw, height = ppt.fh)
  }
  int <- coef(fit)[1]
  beta <- coef(fit)[2]
  if (isTRUE(predict)) {
    fit <- function(x, int, beta){
      f <- int + beta*x
      return(f)
    }
    pre <- fit(res, int, beta)
  }
  if (isTRUE(predict.inverse)) {
    Inv.fit <- function(y, int, beta){
      f <- (y - int)/ beta
      return(f)
    }
    pre.inv <- Inv.fit(res, int, beta)
  }
  if (predict.inverse == FALSE & predict == TRUE) {
    predict_fit <- cbind.data.frame(res, pre)
    linearFit <- list(predict = predict_fit, plot = p)
    return(linearFit)
  }
  if (predict.inverse == TRUE & predict == FALSE) {
    predict_fit <- cbind.data.frame(res, pre.inv)
    linearFit <- list(predict = predict_fit, plot = p)
    return(linearFit)
  }
  if (predict.inverse == TRUE & predict == TRUE) {
    predict_fit <- cbind.data.frame(res, pre, pre.inv)
    linearFit <- list(predict = predict_fit, plot = p)
    return(linearFit)
  }
  if (predict.inverse == FALSE & predict == FALSE) {
    return(p)
  }
}
