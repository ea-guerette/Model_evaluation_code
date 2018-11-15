#this is the code for conditionalQuantile from openair 
#I want to hack it to make it conditionalHist (keep histograms, remove the quantile stuff)

conditionalQuantile <- function(mydata, obs = "obs", mod = "mod",
                                type = "default",
                                bins = 31,
                                min.bin = c(10, 20),
                                xlab = "predicted value",
                                ylab = "observed value",
                                col = brewer.pal(5, "YlOrRd"),
                                key.columns = 2,
                                key.position = "bottom",
                                auto.text = TRUE, ...) {
  ## partly based on from Wilks (2005) and package verification, with many modifications
  
  # keep R check quite
  data = second = third = NULL
  
  if (length(type) > 2) stop("Only two types can be used with this function")
  
  ## extra.args setup
  extra.args <- list(...)
  
  ## set graphics
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")
  
  ## reset graphic parameters
  on.exit(trellis.par.set(
    
    fontsize = current.font
  ))
  
  # label controls
  # (xlab and ylab handled in formals because unique action)
  extra.args$main <- if ("main" %in% names(extra.args)) {
    quickText(extra.args$main, auto.text)
  } else {
    quickText("", auto.text)
  }
  
  if ("fontsize" %in% names(extra.args)) {
    trellis.par.set(fontsize = list(text = extra.args$fontsize))
  }
  
  
  if (length(col) == 1 && col == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
    # other local colours
    ideal.col <- "black"
    col.1 <- grey(0.75)
    col.2 <- grey(0.5)
    col.5 <- grey(0.25)
  } else {
    ideal.col <- "#0080ff"
    col.1 <- col[1]
    col.2 <- col[2]
    col.5 <- col[5]
  }
  
  vars <- c(mod, obs)
  
  if (any(type %in% dateTypes)) vars <- c("date", vars)
  
  ## check the data
  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE)
  mydata <- na.omit(mydata)
  mydata <- cutData(mydata, type)
  
  
  
  procData <- function(mydata) {
    
    mydata <- select_if(mydata, is.numeric)
    obs <- mydata[[obs]]
    pred <- mydata[[mod]]
    min.d <- min(mydata)
    max.d <- max(mydata)
    bins <- seq(floor(min.d), ceiling(max.d), length = bins)
    
    lo <- min(bins)
    hi <- max(bins)
    b <- bins[-length(bins)]
    labs <- b + 0.5 * diff(bins)
    obs.cut <- cut(
      obs,
      breaks = bins, include.lowest = TRUE,
      labels = labs
    )
    obs.cut[is.na(obs.cut)] <- labs[1]
    obs.cut <- as.numeric(as.character(obs.cut))
    pred.cut <- cut(
      pred,
      breaks = bins, include.lowest = TRUE,
      labels = labs
    )
    pred.cut[is.na(pred.cut)] <- labs[1]
    
    n <- length(labs)
    lng <- tapply(obs, pred.cut, length)
    med <- tapply(obs, pred.cut, median)
    q1 <- tapply(obs, pred.cut, quantile, probs = 0.25)
    q2 <- tapply(obs, pred.cut, quantile, probs = 0.75)
    q1[lng <= min.bin[1]] <- NA
    q2[lng <= min.bin[1]] <- NA
    q3 <- tapply(obs, pred.cut, quantile, probs = 0.1)
    q4 <- tapply(obs, pred.cut, quantile, probs = 0.9)
    q3[lng <= min.bin[2]] <- NA
    q4[lng <= min.bin[2]] <- NA
    
    results <- data.frame(x = as.numeric(levels(pred.cut)), lng, med, q1, q2, q3, q4)
    
    results.cut <- data.frame(pred.cut = as.numeric(as.character(pred.cut)), obs.cut = obs)
    
    ## range taken by observations
    results.obs <- data.frame(min = min(obs), max = max(obs))
    results <- list(results, results.cut, results.obs)
    results
  }
  
  
  lo <- min(mydata[c(mod, obs)])
  hi <- max(mydata[c(mod, obs)])
  
  all.results <- group_by(mydata, UQS(syms(type))) %>% 
    nest() %>% 
    mutate(results = map(data, procData))
  
  
  results <- all.results %>% 
    mutate(first = map(results, 1)) %>% 
    unnest(first)
  
  hist.results <- all.results %>% 
    mutate(second = map(results, 2)) %>% 
    unnest(second)
  
  obs.results <- all.results %>% 
    mutate(third = map(results, 3)) %>% 
    unnest(third)
  
  ## proper names of labelling #################################################
  pol.name <- sapply(levels(results[[type[1]]]), function(x) quickText(x, auto.text))
  strip <- strip.custom(factor.levels = pol.name)
  
  if (length(type) == 1) {
    strip.left <- FALSE
    if (type == "default") strip <- FALSE
  } else { ## two conditioning variables
    
    pol.name <- sapply(levels(results[[type[2]]]), function(x) quickText(x, auto.text))
    strip.left <- strip.custom(factor.levels = pol.name)
  }
  ## ###########################################################################
  
  temp <- paste(type, collapse = "+")
  myform <- formula(paste("x ~ med | ", temp, sep = ""))
  
  xyplot.args <- list(
    x = myform, data = results,
    xlim = c(lo, hi * 1.05),
    ylim = c(lo, hi * 1.05),
    ylab = quickText(ylab, auto.text),
    xlab = quickText(xlab, auto.text),
    as.table = TRUE,
    aspect = 1,
    strip = strip,
    strip.left = strip.left,
    key = list(
      lines = list(
        col = c(col.1, col.2, col.5, ideal.col),
        lwd = c(15, 15, 2, 1)
      ),
      lines.title = 1, title = "", text = list(lab = c(
        "25/75th percentile",
        "10/90th percentile",
        "median",
        "perfect model"
      )),
      space = key.position,
      columns = key.columns
    ), 
    par.strip.text = list(cex = 0.8),
    panel = function(x, subscripts, ...) {
      panel.grid(-1, -1, col = "grey95")
      
      poly.na(
        results$x[subscripts], results$q3[subscripts],
        results$x[subscripts],
        results$q4[subscripts],
        myColors = col.2, alpha = 1
      )
      poly.na(
        results$x[subscripts], results$q1[subscripts],
        results$x[subscripts],
        results$q2[subscripts],
        myColors = col.1, alpha = 1
      )
      
      # draw line of where observations lie
      theSubset <- inner_join(obs.results, results[subscripts[1], ], by = type)
      
      panel.lines(
        c(theSubset$min, theSubset$max), c(
          theSubset$min,
          theSubset$max
        ),
        col = ideal.col, lwd = 1.5
      )
      panel.lines(
        results$x[subscripts], results$med[subscripts],
        col = col.5, lwd = 2
      )
    }
  )
  
  # reset for extra.args
  
  xyplot.args <- listUpdate(xyplot.args, extra.args)
  
  # plot
  scatter <- do.call(xyplot, xyplot.args)
  
  temp <- paste(type, collapse = "+")
  myform <- formula(paste(" ~ pred.cut | ", temp, sep = ""))
  bins <- seq(floor(lo), ceiling(hi), length = bins)
  
  pred.cut <- NULL ## avoid R NOTES
  
  histo <- histogram(
    myform,
    data = hist.results, breaks = bins, type = "count",
    as.table = TRUE,
    strip = strip,
    strip.left = strip.left,
    col = "black", alpha = 0.1, border = NA,
    par.strip.text = list(cex = 0.8),
    ylab = "sample size for histograms",
    panel = function(x = pred.cut, col = "black", border = NA,
                     alpha = 0.2,
                     subscripts, ...) {
      ## histogram of observations
      panel.histogram(
        x = hist.results[["obs.cut"]][subscripts],
        col = NA, alpha = 0.5, lwd = 0.5,
        border = ideal.col, ...
      )
      ## histogram of modelled values
      panel.histogram(x = x, col = "black", border, alpha = 0.15, ...)
    }
  )
  
  ## supress scaling warnings
  thePlot <- latticeExtra::doubleYScale(scatter, histo, add.ylab2 = TRUE)
  thePlot <- update(thePlot, par.settings = simpleTheme(col = c("black", "black")))
  
  if (length(type) == 1) {
    plot(thePlot)
  } else {
    plot(latticeExtra::useOuterStrips(
      thePlot,
      strip = strip,
      strip.left = strip.left
    ))
  }
  
  invisible(trellis.last.object())
  
  output <- list(plot = thePlot, data = results, call = match.call())
  class(output) <- "openair"
  invisible(output)
}