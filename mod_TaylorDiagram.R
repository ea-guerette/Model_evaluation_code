#this is a self-contained TaylorDiagram function 

#modification 1: change colour of "observed" dot and text from purple to black

library(rlang)
library(tidyr)
library(dplyr)
library(lattice)
library(latticeExtra)
library(purrr)

dateTypes <- c(
  "year", "hour", "month", "season", "weekday", "weekend",
  "monthyear", "gmtbst", "bstgmt", "dst", "daylight",
  "seasonyear", "yearseason"
)

#check that obs and mod are numbers 
checkNum <- function(mydata, vars) {
  for (i in seq_along(vars)) {
    if (!is.numeric(mydata[[vars[i]]])) {
      mydata[[vars[i]]] <- as.numeric(as.character(mydata[[vars[i]]]))
      
      warning(
        paste(vars[i], "is not numeric, forcing to numeric..."),
        call. = FALSE
      )
    }
  }
  
  return(mydata)
}


###this is an internal openair function - checks that the data is OK
checkPrep <- function(mydata, Names, type, remove.calm = TRUE, remove.neg = TRUE,
                      strip.white = TRUE, wd = "wd") {
  
  ## deal with conditioning variable if present, if user-defined, must exist in data
  ## pre-defined types
  ## existing conditioning variables that only depend on date (which is checked)
  conds <- c(
    "default", "year", "hour", "month", "season", "weekday",
    "weekend", "monthyear", "gmtbst", "bstgmt", "dst", "daylight",
    "yearseason", "seasonyear"
  )
  all.vars <- unique(c(names(mydata), conds))
  
  varNames <- c(Names, type) ## names we want to be there
  matching <- varNames %in% all.vars
  
  if (any(!matching)) {
    ## not all variables are present
    stop(cat("Can't find the variable(s)", varNames[!matching], "\n"))
  }
  
  ## add type to names if not in pre-defined list
  if (any(type %in% conds == FALSE)) {
    ids <- which(type %in% conds == FALSE)
    Names <- c(Names, type[ids])
  }
  
  ## if type already present in data frame
  if (any(type %in% names(mydata))) {
    ids <- which(type %in% names(mydata))
    Names <- unique(c(Names, type[ids]))
  }
  
  ## just select data needed
  mydata <- mydata[, Names]
  
  ## if site is in the data set, check none are missing
  ## seems to be a problem for some KCL data...
  if ("site" %in% names(mydata)) { ## split by site
    
    ## remove any NA sites
    if (anyNA(mydata$site)) {
      id <- which(is.na(mydata$site))
      mydata <- mydata[-id, ]
    }
  }
  
  
  ## sometimes ratios are considered which can results in infinite values
  ## make sure all infinite values are set to NA
  mydata[] <- lapply(mydata, function(x) {
    replace(x, x == Inf | x == -Inf, NA)
  })
  
  if ("ws" %in% Names) {
    if ("ws" %in% Names & is.numeric(mydata$ws)) {
      
      ## check for negative wind speeds
      if (any(sign(mydata$ws[!is.na(mydata$ws)]) == -1)) {
        if (remove.neg) { ## remove negative ws only if TRUE
          warning("Wind speed <0; removing negative data")
          mydata$ws[mydata$ws < 0] <- NA
        }
      }
    }
  }
  
  ## round wd to make processing obvious
  ## data already rounded to nearest 10 degress will not be affected
  ## data not rounded will be rounded to nearest 10 degrees
  ## assumes 10 is average of 5-15 etc
  if (wd %in% Names) {
    if (wd %in% Names & is.numeric(mydata[, wd])) {
      
      ## check for wd <0 or > 360
      if (any(sign(mydata[[wd]][!is.na(mydata[[wd]])]) == -1 |
              mydata[[wd]][!is.na(mydata[[wd]])] > 360)) {
        warning("Wind direction < 0 or > 360; removing these data")
        mydata[[wd]][mydata[[wd]] < 0] <- NA
        mydata[[wd]][mydata[[wd]] > 360] <- NA
      }
      
      if (remove.calm) {
        if ("ws" %in% names(mydata)) {
          mydata[[wd]][mydata$ws == 0] <- NA ## set wd to NA where there are calms
          mydata$ws[mydata$ws == 0] <- NA ## remove calm ws
        }
        mydata[[wd]][mydata[[wd]] == 0] <- 360 ## set any legitimate wd to 360
        
        ## round wd for use in functions - except windRose/pollutionRose
        mydata[[wd]] <- 10 * ceiling(mydata[[wd]] / 10 - 0.5)
        mydata[[wd]][mydata[[wd]] == 0] <- 360 # angles <5 should be in 360 bin
      }
      mydata[[wd]][mydata[[wd]] == 0] <- 360 ## set any legitimate wd to 360
    }
  }
  
  
  ## make sure date is ordered in time if present
  if ("date" %in% Names) {
    if ("POSIXlt" %in% class(mydata$date)) {
      stop("date should be in POSIXct format not POSIXlt")
    }
    
    ## if date in format dd/mm/yyyy hh:mm (basic check)
    if (length(grep("/", as.character(mydata$date[1]))) > 0) {
      mydata$date <- as.POSIXct(strptime(mydata$date, "%d/%m/%Y %H:%M"), "GMT")
    }
    
    ## try and work with a factor date - but probably a problem in original data
    if (is.factor(mydata$date)) {
      warning("date field is a factor, check date format")
      mydata$date <- as.POSIXct(mydata$date, "GMT")
    }
    
    mydata <- arrange(mydata, date)
    
    ## make sure date is the first field
    if (names(mydata)[1] != "date") {
      mydata <- mydata[c("date", setdiff(names(mydata), "date"))]
    }
    
    ## check to see if there are any missing dates, stop if there are
    ids <- which(is.na(mydata$date))
    if (length(ids) > 0) {
      mydata <- mydata[-ids, ]
      warning(paste(
        "Missing dates detected, removing",
        length(ids), "lines"
      ), call. = FALSE)
    }
    
    ## daylight saving time can cause terrible problems - best avoided!!
    
    if (any(dst(mydata$date))) {
      warning("Detected data with Daylight Saving Time, converting to UTC/GMT")
      mydata$date <- lubridate::force_tz(mydata$date, tzone = "GMT")
    }
  }
  
  if (strip.white) {
    ## set panel strip to white
    suppressWarnings(trellis.par.set(list(strip.background = list(col = "white"))))
  }
  
  ## return data frame
  return(mydata)
}

#this is another internal openair function
listUpdate <- function(a, b, drop.dots = TRUE,
                       subset.a = NULL, subset.b = NULL) {
  if (drop.dots) {
    a <- a[names(a) != "..."]
    b <- b[names(b) != "..."]
  }
  if (!is.null(subset.a)) {
    a <- a[names(a) %in% subset.a]
  }
  if (!is.null(subset.b)) {
    b <- b[names(b) %in% subset.b]
  }
  if (length(names(b) > 0)) {
    a <- modifyList(a, b)
  }
  a
}


mod_TaylorDiagram <- function(mydata, obs = "obs", mod = "mod", group = NULL, type = "default",
                          normalise = FALSE, cols = "brewer1",
                          rms.col = "darkgoldenrod", cor.col = "black", arrow.lwd = 3,
                          annotate = "centred\nRMS error",
                          key = TRUE, key.title = group, key.columns = 1,
                          key.pos = "right", strip = TRUE, auto.text = TRUE, ...) {
  
  ## get rid of R check annoyances
  sd.mod <- R <- NULL
  
  ## greyscale handling
  
  ## set graphics
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")
  
  ## reset graphic parameters
  on.exit(trellis.par.set(
    
    fontsize = current.font
  ))
  
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
    ## other local colours
    method.col <- "greyscale"
  } else {
    method.col <- "default"
  }
  
  
  ## extra.args setup
  extra.args <- list(...)
  
  ## label controls (some local xlab, ylab management in code)
  extra.args$xlab <- if ("xlab" %in% names(extra.args)) {
    quickText(extra.args$xlab, auto.text)
  } else {
    NULL
  }
  
  extra.args$ylab <- if ("ylab" %in% names(extra.args)) {
    quickText(extra.args$ylab, auto.text)
  } else {
    NULL
  }
  
  extra.args$main <- if ("main" %in% names(extra.args)) {
    quickText(extra.args$main, auto.text)
  } else {
    quickText("", auto.text)
  }
  
  if ("fontsize" %in% names(extra.args)) {
    trellis.par.set(fontsize = list(text = extra.args$fontsize))
  }
  
  
  if (!"layout" %in% names(extra.args)) {
    extra.args$layout <- NULL
  }
  
  
  if (!"pch" %in% names(extra.args)) {
    extra.args$pch <- 20
  }
  
  if (!"cex" %in% names(extra.args)) {
    extra.args$cex <- 2
  }
  
  ## #######################################################################################
  
  ## check to see if two data sets are present
  combine <- FALSE
  
  if (length(mod) == 2) combine <- TRUE
  
  if (any(type %in% dateTypes)) {
    vars <- c("date", obs, mod)
  } else {
    vars <- c(obs, mod)
  }
  
  ## assume two groups do not exist
  twoGrp <- FALSE
  
  if (!missing(group)) if (any(group %in% type)) stop("Can't have 'group' also in 'type'.")
  
  mydata <- cutData(mydata, type, ...)
  
  if (missing(group)) {
    if ((!"group" %in% type) & (!"group" %in% c(obs, mod))) {
      mydata$group <- factor("group")
      group <- "group"
      npol <- 1
    }
    ## don't overwrite a
  } else { ## means that group is there
    mydata <- cutData(mydata, group, ...)
  }
  
  ## if group is present, need to add that list of variables unless it is
  ## a pre-defined date-based one
  if (!missing(group)) {
    npol <- length(unique((mydata[[group[1]]])))
    
    ## if group is of length 2
    if (length(group) == 2L) {
      twoGrp <- TRUE
      grp1 <- group[1]
      grp2 <- group[2]
      
      if (missing(key.title)) key.title <- grp1
      vars <- c(vars, grp1, grp2)
      mydata$newgrp <- paste(mydata[[group[1]]], mydata[[group[2]]], sep = "-")
      group <- "newgrp"
    }
    
    if (group %in% dateTypes | any(type %in% dateTypes)) {
      vars <- unique(c(vars, "date", group))
    } else {
      vars <- unique(c(vars, group))
    }
  }
  
  ## data checks, for base and new data if necessary
  
  mydata <- checkPrep(mydata, vars, type)
  
  # check mod and obs are numbers
  mydata <- checkNum(mydata, vars = c(obs, mod))
  
  ## remove missing data
  mydata <- na.omit(mydata)
  
  legend <- NULL
  
  ## function to calculate stats for TD
  calcStats <- function(mydata, obs = obs, mod = mod) {
    R <- cor(mydata[[obs]], mydata[[mod]], use = "pairwise")
    sd.obs <- sd(mydata[[obs]])
    sd.mod <- sd(mydata[[mod]])
    if (normalise) {
      sd.mod <- sd.mod / sd.obs
      sd.obs <- 1
    }
    
    res <- data.frame(R, sd.obs, sd.mod)
    res
  }
  
  vars <- c(group, type)
  
  results <- group_by(mydata, UQS(syms(vars))) %>%
    do(calcStats(., obs = obs, mod = mod[1]))
  
  results.new <- NULL
  
  if (combine) {
    results.new <- group_by(mydata, UQS(syms(vars))) %>%
      do(calcStats(., obs = obs, mod = mod[2]))
  }
  
  
  ## if no group to plot, then add a dummy one to make xyplot work
  if (is.null(group)) {
    results$MyGroupVar <- factor("MyGroupVar")
    group <- "MyGroupVar"
  }
  
  ## set up colours
  myColors <- openColours(cols, npol)
  pch.orig <- extra.args$pch
  
  ## combined colours if two groups
  if (twoGrp) {
    myColors <- rep(
      openColours(cols, length(unique(mydata[[grp1]]))),
      each = length(unique(mydata[[grp2]]))
    )
    
    extra.args$pch <- rep(extra.args$pch, each = length(unique(mydata[[grp2]])))
  }
  
  ## basic function for lattice call + defaults
  temp <- paste(type, collapse = "+")
  
  myform <- formula(paste("R ~ sd.mod", "|", temp, sep = ""))
  
  scales <- list(x = list(rot = 0, alternating = 1), y = list(rot = 0))
  
  pol.name <- sapply(levels(mydata[, group]), function(x) quickText(x, auto.text))
  
  
  if (key & npol > 1 & !combine) {
    thecols <- unique(myColors)
    if (twoGrp) {
      pol.name <- levels(factor(mydata[[grp1]]))
    }
    
    key <- list(
      points = list(col = thecols), pch = pch.orig,
      cex = extra.args$cex, text = list(lab = pol.name, cex = 0.8),
      space = key.pos, columns = key.columns,
      title = quickText(key.title, auto.text),
      cex.title = 0.8, lines.title = 3
    )
  } else if (key & npol > 1 & combine) {
    key <- list(
      lines = list(col = myColors[1:npol]), lwd = arrow.lwd,
      text = list(lab = pol.name, cex = 0.8), space = key.pos,
      columns = key.columns,
      title = quickText(key.title, auto.text),
      cex.title = 0.8, lines.title = 3
    )
  } else {
    key <- NULL
  }
  
  
  ## special wd layout
  if (length(type) == 1 & type[1] == "wd" & is.null(extra.args$layout)) {
    ## re-order to make sensible layout
    wds <- c("NW", "N", "NE", "W", "E", "SW", "S", "SE")
    mydata$wd <- ordered(mydata$wd, levels = wds)
    ## see if wd is actually there or not
    wd.ok <- sapply(wds, function(x) {
      if (x %in% unique(mydata$wd)) FALSE else TRUE
    })
    skip <- c(wd.ok[1:4], TRUE, wd.ok[5:8])
    mydata$wd <- factor(mydata$wd) ## remove empty factor levels
    extra.args$layout <- c(3, 3)
    if (!"skip" %in% names(extra.args)) {
      extra.args$skip <- skip
    }
  }
  if (!"skip" %in% names(extra.args)) {
    extra.args$skip <- FALSE
  }
  
  
  ## proper names of labelling ####################################################
  
  stripName <- sapply(levels(mydata[, type[1]]), function(x) quickText(x, auto.text))
  if (strip) strip <- strip.custom(factor.levels = stripName)
  
  if (length(type) == 1) {
    strip.left <- FALSE
  } else { ## two conditioning variables
    stripName <- sapply(levels(mydata[, type[2]]), function(x) quickText(x, auto.text))
    strip.left <- strip.custom(factor.levels = stripName)
  }
  ## #############################################################################
  
  
  ## no strip needed for single panel
  if (length(type) == 1 & type[1] == "default") strip <- FALSE
  
  ## not sure how to evaluate "group" in xyplot, so change to a fixed name
  id <- which(names(results) == group)
  names(results)[id] <- "MyGroupVar"
  
  maxsd <- 1.2 * max(results$sd.obs, results$sd.mod)
  
  # xlim, ylim handling
  if (!"ylim" %in% names(extra.args)) {
    extra.args$ylim <- 1.12 * c(0, maxsd)
  }
  if (!"xlim" %in% names(extra.args)) {
    extra.args$xlim <- 1.12 * c(0, maxsd)
  }
  
  ## xlab, ylab local management
  if (is.null(extra.args$ylab)) {
    extra.args$ylab <- if (normalise) "standard deviation (normalised)" else "standard deviation"
  }
  if (is.null(extra.args$xlab)) {
    extra.args$xlab <- extra.args$ylab
  }
  
  
  ## plot
  xyplot.args <- list(
    x = myform, data = results, groups = results$MyGroupVar,
    aspect = 1,
    type = "n",
    as.table = TRUE,
    scales = scales,
    key = key,
    par.strip.text = list(cex = 0.8),
    strip = strip,
    strip.left = strip.left,
    panel = function(x, y, ...) {
      
      ## annotate each panel but don't need to do this for each grouping value
      panel.taylor.setup(
        x, y,
        results = results, maxsd = maxsd,
        cor.col = cor.col, rms.col = rms.col,
        annotate = annotate, ...
      )
      
      ## plot data in each panel
      panel.superpose(
        x, y,
        panel.groups = panel.taylor, ...,
        results = results, results.new = results.new,
        combine = combine, myColors = myColors,
        arrow.lwd = arrow.lwd
      )
    }
  )
  
  ## reset for extra.args
  xyplot.args <- listUpdate(xyplot.args, extra.args)
  
  ## plot
  plt <- do.call(xyplot, xyplot.args)
  
  
  if (length(type) == 1) plot(plt) else plot(useOuterStrips(plt, strip = strip, strip.left = strip.left))
  #newdata <- results
  #output <- list(plot = plt, data = newdata, call = match.call())
  #class(output) <- "openair"
  output <- plt
  
  invisible(output)
}





panel.taylor.setup <- function(x, y, subscripts, results, maxsd, cor.col, rms.col,
                               col.symbol, annotate, group.number, type, ...) {
  ## note, this assumes for each level of type there is a single measured value
  ## therefore, only the first is used  i.e. results$sd.obs[subscripts[1]]
  ## This does not matter if normalise = TRUE because all sd.obs = 1.
  
  ## The data frame 'results' should contain a grouping variable 'MyGroupVar',
  ## 'type' e.g. season, R (correlation coef), sd.obs and sd.mod
  xcurve <- cos(seq(0, pi / 2, by = 0.01)) * maxsd
  ycurve <- sin(seq(0, pi / 2, by = 0.01)) * maxsd
  llines(xcurve, ycurve, col = "black")
  
  xcurve <- cos(seq(0, pi / 2, by = 0.01)) * results$sd.obs[subscripts[1]]
  ycurve <- sin(seq(0, pi / 2, by = 0.01)) * results$sd.obs[subscripts[1]]
  llines(xcurve, ycurve, col = "black", lty = 5)
  
  corr.lines <- c(0.2, 0.4, 0.6, 0.8, 0.9)
  
  ## grid line with alpha transparency
  theCol <- t(col2rgb(cor.col)) / 255
  
  for (gcl in corr.lines) llines(
    c(0, maxsd * gcl), c(0, maxsd * sqrt(1 - gcl ^ 2)),
    col = rgb(theCol, alpha = 0.4), alpha = 0.5
  )
  
  bigtick <- acos(seq(0.1, 0.9, by = 0.1))
  medtick <- acos(seq(0.05, 0.95, by = 0.1))
  smltick <- acos(seq(0.91, 0.99, by = 0.01))
  
  lsegments(
    cos(bigtick) * maxsd, sin(bigtick) *
      maxsd, cos(bigtick) * 0.96 * maxsd, sin(bigtick) * 0.96 * maxsd,
    col = cor.col
  )
  
  lsegments(
    cos(medtick) * maxsd, sin(medtick) *
      maxsd, cos(medtick) * 0.98 * maxsd, sin(medtick) * 0.98 * maxsd,
    col = cor.col
  )
  lsegments(
    cos(smltick) * maxsd, sin(smltick) *
      maxsd, cos(smltick) * 0.99 * maxsd, sin(smltick) * 0.99 * maxsd,
    col = cor.col
  )
  
  ## arcs for standard deviations (3 by default)
  gamma <- pretty(c(0, maxsd), n = 5)
  if (gamma[length(gamma)] > maxsd) {
    gamma <- gamma[-length(gamma)]
  }
  labelpos <- seq(45, 70, length.out = length(gamma))
  
  ## some from plotrix
  for (gindex in 1:length(gamma)) {
    xcurve <- cos(seq(0, pi, by = 0.03)) * gamma[gindex] +
      results$sd.obs[subscripts[1]]
    endcurve <- which(xcurve < 0)
    endcurve <- ifelse(length(endcurve), min(endcurve) - 1, 105)
    ycurve <- sin(seq(0, pi, by = 0.03)) * gamma[gindex]
    maxcurve <- xcurve * xcurve + ycurve * ycurve
    startcurve <- which(maxcurve > maxsd * maxsd)
    startcurve <- ifelse(length(startcurve), max(startcurve) + 1, 0)
    
    llines(
      xcurve[startcurve:endcurve], ycurve[startcurve:endcurve],
      col = rms.col, lty = 5
    )
    ltext(
      xcurve[labelpos[gindex]], ycurve[labelpos[gindex]],
      gamma[gindex],
      cex = 0.7, col = rms.col, pos = 1,
      srt = 0, font = 2
    )
    
    ltext(
      1.1 * maxsd, 1.05 * maxsd,
      labels = annotate, cex = 0.7,
      col = rms.col, pos = 2
    )
  }
  
  ## angles for R key
  angles <- 180 * c(bigtick, acos(c(0.95, 0.99))) / pi
  
  ltext(
    cos(c(bigtick, acos(c(0.95, 0.99)))) *
      1.06 * maxsd, sin(c(bigtick, acos(c(0.95, 0.99)))) *
      1.06 * maxsd, c(seq(0.1, 0.9, by = 0.1), 0.95, 0.99),
    cex = 0.7,
    adj = 0.5, srt = angles, col = cor.col
  )
  
  ltext(
    0.82 * maxsd, 0.82 * maxsd, "correlation",
    srt = 315, cex = 0.7,
    col = cor.col
  )
  
  
  ## measured point and text
  lpoints(results$sd.obs[subscripts[1]], 0, pch = 20, col = "black", cex = 1.5)
  ltext(results$sd.obs[subscripts[1]], 0, "observed", col = "black", cex = 0.7, pos = 3)
}


panel.taylor <- function(x, y, subscripts, results, results.new, maxsd, cor.col,
                         rms.col, combine, col.symbol, myColors, group.number,
                         type, arrow.lwd, ...) {
  R <- NULL
  sd.mod <- NULL ## avoid R NOTEs
  
  ## Plot actual results by type and group if given
  results <- transform(results, x = sd.mod * R, y = sd.mod * sin(acos(R)))
  
  if (combine) {
    results.new <- transform(results.new, x = sd.mod * R, y = sd.mod * sin(acos(R)))
    larrows(
      results$x[subscripts], results$y[subscripts],
      results.new$x[subscripts], results.new$y[subscripts],
      angle = 30, length = 0.1, col = myColors[group.number], lwd = arrow.lwd
    )
  } else {
    lpoints(
      results$x[subscripts], results$y[subscripts],
      col.symbol = myColors[group.number], ...
    )
  }
}
