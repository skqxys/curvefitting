#' @title Fitting Four-parameter logistic models and Plotting
#' @param stan dataFrame for fitting four-parameter logistic models
#' @param res numeric vector used for prediction
#' @param stan.x the name used for x axis of the fitting plot
#' @param stan.y the name used for y axis of the fitting plot
#' @param predict TRUE or FALSE
#' @param predict.inverse TRUE or FALSE
#' @param toppt TRUE or FALSE
#' @param ppt.fw figure width in ppt
#' @param ppt.fh figure height in ppt
#' @description Fitting four-parameter logistic models.
#' Plotting the curve and export to ppt
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

fourPL <- function(stan, res, stan.x = NULL, stan.y = NULL, toppt = FALSE,
                   ppt.fw = 5, ppt.fh = 4, predict = FALSE,
                   predict.inverse = TRUE) {
  if (is.null(stan.x)) {
    stan.x <- colnames(stan)[1]  ##Conc
  }
  if (is.null(stan.y)) {
    stan.y <- colnames(stan)[2]   ##OD
  }
  M.4pl <- function(x, d.4pl, a.4pl, c.4pl, b.4pl){
    f <- d.4pl + ((a.4pl - d.4pl)/
                    (1 + (x / c.4pl)^b.4pl))
    return(f)
    }
  #small.x.asymp
  d.4pl = min(stan[, stan.y])/1.2   #OD
  #inf.x.asymp
  a.4pl = max(stan[, stan.y])*1.2     #OD
  #inflec
  c.4pl = max(stan[, stan.x])/2      #Conc
  #hill
  b.4pl = -0.5
  start.ocon.4pl <- c(d.4pl = d.4pl, a.4pl = a.4pl,
                      c.4pl = c.4pl, b.4pl = b.4pl)
  xlabel <- colnames(stan)[1]
  colnames(stan)[1] <- "Conc"
  fit <- nlsLM(stan[, stan.y] ~ M.4pl(Conc,
                                      d.4pl, a.4pl, c.4pl, b.4pl),
               data = stan,start = start.ocon.4pl)
  param.4pl <- summary(fit)$parameters[, 1]
  d.4pl = as.vector(param.4pl[1])
  a.4pl = as.vector(param.4pl[2])
  c.4pl = as.vector(param.4pl[3])
  b.4pl = as.vector(param.4pl[4])
  M.4pl <- function(x, d.4pl, a.4pl, c.4pl, b.4pl){
    f <- d.4pl + ((a.4pl - d.4pl)/
                    (1 + (x / c.4pl)^b.4pl))
    return(f)
    }
  #### OD:x   Con:y
  # label6=data.frame(formula=sprintf("italic(y) == %.2f%+((.2f %-.2f)/(1 %+ (italic(x) / .2f)^.2f)) ",
  #                   round(coef(fit6)[1],2),round(coef(fit6)[2],2),
  #                   round(coef(fit6)[3],2),round(coef(fit6)[4],2)))
  theme_set(theme_bw())
  p <- data.frame(x = seq(min(stan[, 1]),
                          max(stan[, 1]), by = 0.02),
                  y = M.4pl(seq(min(stan[, 1]),
                                max(stan[, 1]), by = 0.02),
                        d.4pl, a.4pl, c.4pl, b.4pl)) %>%
    ggplot(aes(x = x, y = y)) + geom_line() +
    geom_point(aes(x = stan[, 1], y = stan[, 2]), data = stan) +
    labs(title = "Four-parameter logistic models") + xlab(label = xlabel) +
    ylab(label = stan.y) + theme(title = element_text(size = 8))
  print(p)
  if (isTRUE(toppt)) {
    topptx(figure = p, filename = "Four-parameter logistic fitting.pptx",
           width = ppt.fw, height = ppt.fh)
  }
  print(fit)
  if (isTRUE(predict)) {
    pre <- M.4pl(res, d.4pl, a.4pl, c.4pl, b.4pl)
  }
  if (isTRUE(predict.inverse)) {
    Inv.4pl <- function(y, d.4pl, a.4pl, c.4pl, b.4pl){
      f <- c.4pl * ((a.4pl - d.4pl) /
                       (y - d.4pl) - 1)^(1 / b.4pl)
      return(f)
    }
    pre.inv <- Inv.4pl(res, d.4pl, a.4pl, c.4pl, b.4pl)
  }
  if (predict.inverse == FALSE & predict == TRUE) {
    predict_fit <- cbind.data.frame(res, pre)
    Fit4pl <- list(predict = predict_fit,plot = p)
    return(Fit4pl)
  }
  if (predict.inverse == TRUE & predict == FALSE) {
    predict_fit <- cbind.data.frame(res, pre.inv)
    if (TRUE %in% is.na(predict_fit$pre.inv)) {
      predict_fit[rownames(subset(predict_fit,
                                  pre.inv=="NaN" & res<min(s2[, 2]))),
                  "pre.inv"] = 0
      predict_fit[rownames(subset(predict_fit,
                                  pre.inv=="NaN" & res>max(s2[, 2]))),
                  "pre.inv"] = Inf
    }
    Fit4pl <- list(predict = predict_fit,plot = p)
    return(Fit4pl)
  }
  if (predict.inverse == TRUE & predict == TRUE) {
    predict_fit <- cbind.data.frame(res, pre, pre.inv)
    if (TRUE %in% is.na(predict_fit$pre.inv)) {
      predict_fit[rownames(subset(predict_fit,
                                  pre.inv=="NaN" & res<min(s2[, 2]))),
                  "pre.inv"] = 0
      predict_fit[rownames(subset(predict_fit,
                                  pre.inv=="NaN" & res>max(s2[, 2]))),
                  "pre.inv"] = Inf
    }
    Fit4pl <- list(predict = predict_fit,plot = p)
    return(Fit4pl)
  }
  if (predict.inverse == FALSE & predict == FALSE) {
    return(p)
  }
}


