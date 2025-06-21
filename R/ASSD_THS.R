#' Compares random prospects by ASSD-THS
#'
#' It uses the areas between the SSD functions, and the
#' expected values of the prospects to compare them based on the ASSD-THS rule.
#' If the violation area ratio is less than 0.5 for a prospect, and its expected
#' value is larger, it dominates the other by ASSD-THS.
#'
#' @details
#' epsilon shows the ratio of the violation. Smaller epsilon means more
#' decision-makers agree with the result.
#'
#' The returned list has six elements: `winner` indicates the dominant prospect
#' index. It will be zero if neither dominates the other. `epsilon` is the ratio
#' of violated area to the total area between the SSDs. `area` is a vector, where
#' the values show the area between the SSDs correspond to each segment.
#' `total.area` is the total area between the SSDs. `positive.area` is the
#' amount of area where the `area` vector is positive, meaning the `ssd1` is
#' larger than `ssds2`. `negative.area` is like `positive.area` for negative
#' values.
#'
#' If neither distribution dominates the other by ASSD-THS, the `winner` output will
#' be zero, and it happens only when the distribution with a higher expected
#' value has the `epsilon` which is larger than 0.5.
#'
#' @seealso [expected.values(), area.btwn.ssd.calc()] for more details.
#'
#' @param sd.obj StochasticDominance object.
#' @returns A list, including all the calculation details.
#'
assd.ths.test = function(sd.obj){

  area = area.btwn.ssd.calc(sd.obj)

  exp.val = expected.values(sd.obj)
  mean1 = exp.val$mean1
  mean2 = exp.val$mean2

  total.area = sum(abs(area))
  neg.area = abs(sum(area[area < 0]))
  pos.area = sum(area[area > 0])

  pos.ratio = ifelse(is.na(pos.area/total.area), 0, pos.area/total.area)
  neg.ratio = ifelse(is.na(neg.area/total.area), 0, neg.area/total.area)

  if(neg.ratio < 0.5 & mean2 >= mean1){
    winner = 2
    epsilon = round(neg.ratio,3)
  } else if(pos.ratio < 0.5 & mean2 <= mean1){
    winner = 1
    epsilon = round(pos.ratio,3)
  } else {
    winner = 0
    epsilon = round(min(pos.ratio,neg.ratio),3)
  }

  return(list(winner = winner, epsilon = epsilon, area = area,
              total.area = total.area, positive.area = pos.area,
              negative.area = neg.area))
}

#' Calculates area between SSD functions
#'
#' For every segments, the area between SSD functions will be computed.
#'
#' @seealso [modif.outcome.ssd.calc(), area.below.ssd.calc()] for more details.
#'
#' @param sd.obj StochasticDominance object.
#' @returns Numeric vector, including area differences in every segments.
#'
area.btwn.ssd.calc = function(sd.obj){

  new.outcome.ssd = modif.outcome.ssd.calc(sd.obj)

  area1 = area.below.ssd.calc(new.outcome.ssd$outcome, new.outcome.ssd$ssd1)
  area2 = area.below.ssd.calc(new.outcome.ssd$outcome, new.outcome.ssd$ssd2)

  return(area1 - area2)
}

#' Calculates area below SSD function
#'
#' For every segment, the area below SSD function will be computed.
#'
#' @seealso [calc.area.below.line()] for area below line.
#'
#' @param outcome Numeric vector, including outcome values.
#' @param ssd Numeric vector, including SSD values.
#' @returns Numeric vector, including area below every segment of SSD function.
#'
area.below.ssd.calc = function(outcome, ssd){

  area = c()
  n = length(outcome)
  for (i in 1:(n-1)) {
    a = calc.area.below.line(outcome[i],outcome[i+1],ssd[i],ssd[i+1])
    area = append(area, a)
  }

  return(area)
}
