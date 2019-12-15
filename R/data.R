#' Advertising campaign dataset
#'
#' @format A data frame with 29848 observations on the following 9 variables.
#' \describe{
#'   \item{\code{id}}{a factor variable with 80 levels representing the 80 campaigns we want to cluster}
#'   \item{\code{timestamp_ymd}}{a POSIXct variable corresponding to the datetime each data is collected}
#'   \item{\code{yearDay}}{a factor with day levels of the year} 
#'   \item{\code{day}}{a factor variable with 7 levels representing the 7 days of the week}
#'   \item{\code{timeSlot}}{a factor with levels 6 levels representing 6 different timeSlot : 00h-4h, 4h-8h, 8h-12h, 12h-16h, 16h-20h, 20h-00h}
#'   \item{\code{app_or_site}}{a factor with 2 levels \code{app} \code{site}, representing the 2 types of support where an advertising is displayed}
#'   \item{\code{impressions}}{a numeric vector counting the number of times an advertising is displayed on a defined timestamp}
#'   \item{\code{click}}{a numeric vector counting the number of times an advertising is clicked on a defined timestamp}
#'   \item{\code{ctr}}{a numeric vector corresponding to the number of clicks divided by the number of impressions}
#'  }
#' @source These data are extracted from TabMo database. 
#' @examples
#' library(binomialMix)
#' summary(adcampaign)
"adcampaign"
