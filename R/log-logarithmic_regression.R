#' @title Fitting Log-logarithmic regression and Plotting
#' @param stan dataFrame for fitting log-logarithmic regression
#' @param res numeric vector used for prediction
#' @param stan.x the name used for x axis of the fitting plot
#' @param stan.y the name used for y axis of the fitting plot
#' @param predict TRUE or FALSE
#' @param toppt TRUE or FALSE
#' @param ppt.fw figure width in ppt
#' @param ppt.fh figure height in ppt
#' @description Fitting data with log-logarithmic regression.
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

loglog<-function(stan, res, stan.x = NULL, stan.y = NULL, ppt.fw = 5,
                 ppt.fh = 4, toppt = FALSE, predict = TRUE) {
  if (is.null(stan.x)) {
    stan.x <- colnames(stan)[1]
  }
  if (is.null(stan.y)) {
    stan.y <- colnames(stan)[2]
  }
  fit<-lm(formula = log(stan[, stan.y]) ~ log(stan[, stan.x]),
          data = log(stan))
  cat("Log-logarithmic regression")
  print(fit)
  label <- data.frame(formula = sprintf("italic(y) == %.2f %+.2f * italic(x) ",
                                        round(coef(fit)[1], 2),
                                        round(coef(fit)[2], 2)),
                      r2 = sprintf("italic(R^2) == %.2f",
                                   round(summary(fit)$r.squared, 2)),
                      rse = paste("Residual standard error:",
                                  round(summary(fit)$sigma, 2)),
                      stringsAsFactors = FALSE)
  theme_set(theme_bw())
  p <- ggplot(data = log(stan), aes(x = log(stan[, stan.x]),
                                    y = log(stan[, stan.y]))) +
    geom_point() + geom_smooth(method = lm, formula = y ~ x, se = F)+
    labs(title = "Log-Log regression") +
    xlab(label = paste("log(", stan.x, ")", sep="")) +
    ylab(label = paste("log(", stan.y, ")", sep="")) +
    theme(title = element_text(size = 8)) +
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
    topptx(figure = p, filename = "Log-Log regression.pptx",
           width = ppt.fw, height = ppt.fh)
  }
  if (isTRUE(predict)) {
    b = coef(fit)[1]
    a = coef(fit)[2]
    fit <- function(x, b, a){
      f <- exp(b + a*(log(x)))
      return(f)
    }
    pre <- fit(res, b, a)
  }
  predict_fit <- cbind.data.frame(res, pre)
  loglogFit <- list(predict = predict_fit, plot = p)
  return(loglogFit)
}


