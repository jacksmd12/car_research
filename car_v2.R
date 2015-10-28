## load library
library(jsonlite)
library(lubridate)

## download data from Edmunds
APIkey = "rc9g86ntj2xtfdfrpkmjjmrx"
resURL = paste("https://api.edmunds.com/api/vehicle/v2/makes?fmt=json&api_key=", APIkey, sep = "")
d <- fromJSON(resURL)

## MAIN FUNCTION
## get true market value (TMV) by inputing data frame of cars ('c') extracted from carlist variable
tmv <- function(d, make, model, year, zip = "21231", condition = "Average") {

    c <- findcar(d, make, model, year)
    style <- getstyle(c)
    style$style_id <- as.character(style$style_id)
    
    ## empty data frame to store upcoming data
    tmv <- NULL
    
    for (j in 1:nrow(style)) {
        id <- style$style_id[j]
        url <- paste0("https://api.edmunds.com/v1/api/tmv/tmvservice/calculatetypicallyequippedusedtmv?styleid=", id, "&zip=", zip, "&fmt=json&api_key=", APIkey)
        v <- fromJSON(url)
        retail <- v$tmv$totalWithOptions$usedTmvRetail
        private <- v$tmv$totalWithOptions$usedPrivateParty
        tradein <- v$tmv$totalWithOptions$usedTradeIn
        t <- data.frame(retail = retail, private = private, trade_in = tradein)
        t <- cbind(style[j, ], t)
        tmv <- rbind(tmv, t)
    }
    tmv
}

## fuction to extract cars by make, model, year and id when passed in JSON data
carlist <- function(d) {
    carlist <- NULL
    for (i in 1:length(d$makes$niceName)) {
        models <- d$makes$models[[i]]$niceName
        
        years <- unlist(lapply(d$makes$models[[i]]$years, function(x) {x$year}))
        id <- unlist(lapply(d$makes$models[[i]]$years, function(x) {x$id}))
        
        r <- sapply(d$makes$models[[i]]$years, function(x) {length(x$year)})
        
        temp <- data.frame(year = years, model = rep(models, r), id = id, stringsAsFactors = F)
        temp$make <- d$makes$niceName[[i]]
        temp <- temp[ , c(1, 4, 2, 3)]
        carlist <- rbind(carlist, temp)
    }
    carlist
}

## auotmatically call the function to convert JSON to data frame
d <- carlist(d)


## reduce down full car list to list of car of interst based on make, model and year
findcar <- function(d, make, model, year) {
    x <- d[d$make == make, ]
    x <- x[x$model == model, ]
    c <- NULL
    for (i in 1:length(year)) {
        t <- x[x$year == year[i], ]
        c <- rbind(c, t)
    }
    c
}


## Get the vehicle style details (i.e. colors, options, transmission, engine, squishVins, …etc) by the vehicle’s Make/Mode/Year
getstyle <- function(c) {
    styledf <- NULL
    for (i in 1:nrow(c)) {
        url <- paste0("https://api.edmunds.com/api/vehicle/v2/", c$make[i], "/", c$model[i], "/", c$year[i], "/styles?fmt=json&api_key=", APIkey)
        style <- fromJSON(url)
        ## reduce to variables of interest
        style <- data.frame(make = style$styles$make$niceName, model = style$styles$model$niceName, style_name = style$styles$name, style_id = style$styles$id, year = style$styles$year$year, trim = style$styles$trim, stringsAsFactors = F)
        styledf <- rbind(styledf, style)
    }
    styledf
}
