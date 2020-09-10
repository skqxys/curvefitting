#' @title Fitting Quadratic polynomial regression and Plotting
#' @param stan dataFrame for fitting quadratic polynomial regression
#' @param res numeric vector used for prediction
#' @param stan.x the name used for x axis of the fitting plot
#' @param stan.y the name used for y axis of the fitting plot
#' @param predict predict TRUE or FALSE
#' @param toppt TRUE or FALSE
#' @param ppt.fw figure width in ppt
#' @param ppt.fh figure height in ppt
#' @description Fitting data with quadratic polynomial regression.
#' Plotting fitting curve and export to ppt.
#' The prediction for new data
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
#quadratic(stan=s,res=t$OD,toppt=TRUE)
quadratic <- function(stan, res, stan.x = NULL, stan.y = NULL, ppt.fw = 5,
                      ppt.fh = 4, toppt = FALSE, predict=TRUE) {
  if (is.null(stan.x)) {
    stan.x <- colnames(stan)[1]
    }
  if (is.null(stan.y)) {
    stan.y <- colnames(stan)[2]
    }
  fit <- lm(formula = stan[, stan.y] ~ stan[, stan.x] + I(stan[, stan.x]^2),
            data = stan)
  cat("Quadratic polynomial regression")
  print(fit)
  label <- data.frame(formula = sprintf(
    "italic(y) == %.2f %+.2f * italic(x) %+.2f * italic(x^2)",
    round(coef(fit)[1], 2), round(coef(fit)[2], 2), round(coef(fit)[3], 2)),
    r2 = sprintf("italic(R^2) == %.2f", round(summary(fit)$r.squared, 2)),
    rse = paste("Residual standard error:", round(summary(fit)$sigma, 2)),
    stringsAsFactors = FALSE)
  theme_set(theme_bw())
  p <- ggplot(data = stan, aes(x = stan[,stan.x], y = stan[,stan.y])) +
    geom_point() + geom_smooth(method = lm, formula = y ~ x + I(x^2) ,se = F) +
    xlab(label = stan.x) + ylab(label = stan.y) +
    labs(title = "Quadratic polynomial regression") +
    theme(title = element_text(size = 8))+
    geom_text(data = label, aes(x = -Inf, y = 8*max(stan[, stan.y])/10,
                                label = formula),
              hjust = 0, vjust = 1, parse = TRUE,
              inherit.aes = FALSE, size = 2.5) +
    geom_text(data = label, aes(x = -Inf, y = 7*max(stan[, stan.y])/10,
                                label = r2),
              hjust = 0, vjust = 1, parse = TRUE,
              inherit.aes = FALSE, size = 2.5)
    # geom_text(data = label, aes(x = max(stan[, stan.x])/10,
    #                             y = 6*max(stan[, stan.y])/10,
    #                             label = label$rse),
    #           inherit.aes = FALSE, size = 2.5)
  print(p)
  if (isTRUE(toppt)) {
    topptx(figure = p, filename = "Quadratic polynomial regression.pptx",
           width = ppt.fw, height = ppt.fh)
  }
  if (isTRUE(predict)) {
    c = coef(fit)[1]
    b = coef(fit)[2]
    a = coef(fit)[3]
    fit <- function(x, c, b, a){
      f <- c + b*x + a*(x^2)
      return(f)
    }
    pre <- fit(res, c, b, a)
  }
  predict_fit <- cbind.data.frame(res, pre)
  quadraticFit <- list(predict = predict_fit, plot = p)
  return(quadraticFit)
}

