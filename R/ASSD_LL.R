#' Compares random prospects by ASSD-LL
#'
#' It uses the positive and negative areas that are computed by ASSD-LL and the
#' expected values of the prospects to compare them based on the ASSD-LL rule.
#' If the violation area ratio is less than 0.5 for a prospect, and its expected
#' value is larger, it dominates the other by ASSD-LL.
#'
#' @details
#' epsilon shows the ratio of the violation. Smaller epsilon means more
#' decision-makers agree with the result.
#'
#' The returned list has six elements: `winner` indicates the dominant prospect
#' index. It will be zero if neither dominates the other. `epsilon` is the ratio
#' of violated area to the total area between the CDFs. `area` is a vector, where
#' the values show the area between the CDFs correspond to each segment.
#' `total.area` is the total area between the CDFs. `positive.area` is the
#' amount of area where the `area` vector is positive, meaning the `cdf1` is
#' larger than `cdf2` and `ssd1` is larger than `ssd2`. `negative.area` is like
#' `positive.area` for negative values.
#'
#' If neither distribution dominates the other by ASSD-LL, the `winner` output will
#' be zero, and it happens only when the distribution with a higher expected
#' value has the `epsilon` which is larger than 0.5.
#'
#' @seealso [expected.values(), pos.neg.area.assd.ll(), afsd.test()] for more
#' details.
#'
#' @param sd.obj StochasticDominance object.
#' @returns A list, including all the calculation details.
#'
assd.ll.test = function(sd.obj){

  exp.val = expected.values(sd.obj)
  mean1 = exp.val$mean1
  mean2 = exp.val$mean2

  pos.neg.areas = pos.neg.area.assd.ll(sd.obj)
  pos.area = pos.neg.areas$positive.area
  neg.area = pos.neg.areas$negative.area

  total.area = afsd.test(sd.obj)$total.area

  pos.ratio = ifelse(is.na(pos.area/total.area), 0, pos.area/total.area)
  neg.ratio = ifelse(is.na(neg.area/total.area), 0, neg.area/total.area)

  if(pos.ratio < 0.5 & mean1 >= mean2){
    winner = 1
    epsilon = round(pos.ratio,3)
  } else if(neg.ratio < 0.5 & mean1 <= mean2){
    winner = 2
    epsilon = round(neg.ratio,3)
  } else {
    winner = 0
    epsilon = round(min(pos.ratio,neg.ratio),3)
  }

  return(list(winner = winner, epsilon = epsilon, area = pos.neg.areas$area,
              total.area = total.area, positive.area = pos.area,
              negative.area = neg.area))
}

#' Calculates positive and negative area between CDFs for ASSD-LL
#'
#' It calculates the positive and negative areas between CDFs. The positive is
#' where both CDF and SSD of the first prospect is larger, and vice versa for
#' the negative case.
#'
#' @seealso [modif.outcome.ssd.calc()] for more details.
#'
#' @param sd.obj StochasticDominance object.
#' @returns A list, including three elements corresponding to the positive and
#' negative areas, respectively, and the area vector at the end.
#'
pos.neg.area.assd.ll = function(sd.obj){

  new.outcome.ssd = modif.outcome.ssd.calc(sd.obj)

  pos = 0
  neg = 0
  area.vec = c()

  i=1
  j=1
  while (i < length(sd.obj@outcome)) {
    y1 = new.outcome.ssd$outcome[j]
    cdf1 = sd.obj@cdf1[i]
    cdf2 = sd.obj@cdf2[i]
    if(sd.obj@outcome[i+1] == new.outcome.ssd$outcome[j+1]){
      y2 = sd.obj@outcome[i+1]
      ssd1 = new.outcome.ssd$ssd1[j+1]
      ssd2 = new.outcome.ssd$ssd2[j+1]
      i = i + 1
      j = j + 1
    } else {
      y2 = new.outcome.ssd$outcome[j+1]
      ssd1 = new.outcome.ssd$ssd1[j]
      ssd2 = new.outcome.ssd$ssd2[j]
      j = j + 1
    }
    area = (y2-y1)*(cdf1-cdf2)
    area.vec = append(area.vec, area)
    ssd.diff = ssd1 - ssd2
    if(area > 0 & ssd.diff > 0) pos = pos + area
    if(area < 0 & ssd.diff < 0) neg = neg + abs(area)
  }

  return(list(positive.area = pos, negative.area = neg, area = area.vec))
}
