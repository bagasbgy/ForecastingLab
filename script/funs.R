# forecast value constant readjustment
decompAdj <- function(decompObj, constant, invlog) {

  if (class(decompObj) == "decomposed.ts") {

    # inverse log
    if (invlog == TRUE) {

      decompObj[["x"]] <- decompObj[["x"]] %>% exp()
      decompObj[["trend"]] <- decompObj[["trend"]] %>% exp()
      decompObj[["seasonal"]] <- decompObj[["seasonal"]] %>% exp()
      decompObj[["random"]] <- decompObj[["random"]] %>% exp()

    }

    # constant readjustment
    decompObj[["x"]] <- decompObj[["x"]] + constant
    decompObj[["trend"]] <- decompObj[["trend"]] + constant
    decompObj[["seasonal"]] <- decompObj[["seasonal"]] + constant

  }

  else if (class(decompObj) == "stlm") {

    # select stl time series
    stlObj <- decompObj[["stl"]]

    # inverse log
    if (invlog == TRUE) {

      stlObj[, "Data"] <- stlObj[, "Data"] %>% exp()
      stlObj[, "Trend"] <- stlObj[, "Trend"] %>% exp()
      stlObj[, "Remainder"] <- stlObj[, "Remainder"] %>% exp()

    }

    # constant readjustment
    stlObj[, "Data"] <- stlObj[, "Data"] + constant
    stlObj[, "Trend"] <- stlObj[, "Trend"] + constant

    # return the stl to original list
    decompObj[["stl"]] <- stlObj

  }

  # final object
  return(decompObj)

}

# forecast value constant readjustment
forecastAdj <- function(forecastObj, constant, invlog, rounding = FALSE) {

  # inverse log
  if (invlog == TRUE) {

    forecastObj[["x"]] <- forecastObj[["x"]] %>% exp()
    forecastObj[["mean"]] <- forecastObj[["mean"]] %>% exp()
    forecastObj[["upper"]] <- forecastObj[["upper"]] %>% exp()
    forecastObj[["lower"]] <- forecastObj[["lower"]] %>% exp()

  }

  forecastObj[["x"]] <- forecastObj[["x"]] + constant
  forecastObj[["mean"]] <- forecastObj[["mean"]] + constant
  forecastObj[["upper"]] <- forecastObj[["upper"]] + constant
  forecastObj[["lower"]] <- forecastObj[["lower"]] + constant

  if (rounding == TRUE) {

    forecastObj[["mean"]] <- forecastObj[["mean"]] %>% round()
    forecastObj[["upper"]] <- forecastObj[["upper"]] %>% round()
    forecastObj[["lower"]] <- forecastObj[["lower"]] %>% round()

  }

  return(forecastObj)

}
