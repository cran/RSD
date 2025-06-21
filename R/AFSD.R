#' Compares prospects based on AFSD
#'
#' It compares two prospects using AFSD criteria, that is the prospect having
#' the minimum violation area from a classic FSD.
#'
#' @details
#' The returned list has six elements: `winner` indicates the dominant prospect
#' index. It will be zero if neither dominates the other. `epsilon` is the ratio
#' of violated area to the total area between the CDFs. `area` is a vector, where
#' the values show the area between the CDFs correspond to each segment.
#' `total.area` is the total area between the CDFs. `positive.area` is the
#' amount of area where the `area` vector is positive, meaning the `cdf1` is
#' larger than `cdf2`. `negative.area` is like `positive.area` for negative
#' values.
#'
#' If neither distribution dominates the other by AFSD, the `winner` output will
#' be zero, and it happens only when the expected value of distributions are
#' equal.
#'
#' The `epsilon` and `winner` output parameters are the ones that should be
#' taken most. The others are the calculation details and are provided for
#' further investigation. A lower the `epsilon`, lower the violation ratio
#' of the dominant distribution, lower the eliminated extreme utilities, higher
#' the number of decision-makers who agree on the dominant distribution.
#'
#' @seealso [area.btwn.cdfs.calc()] for area calculations.
#'
#' @importFrom methods is
#'
#' @param sd.obj StochasticDominance object.
#' @returns A list, including all the calculation details.
#' @examples
#' sd = createStochasticDominance(outcome1 = c(1,4,7),
#'                                outcome2 = c(2,3,5),
#'                                prob1 = c(1/3,1/3,1/3),
#'                                prob2 = c(1/6,1/6,2/3))
#' afsd.test(sd)
#'
#' @export
afsd.test = function(sd.obj){

  if(!is(sd.obj, 'StochasticDominance')){
    stop("Input must be of class 'StochasticDominance'.")
  }

  area = area.btwn.cdfs.calc(sd.obj)
  total.area = sum(abs(area))
  neg.area = abs(sum(area[area < 0]))
  pos.area = sum(area[area > 0])

  if(isTRUE(all.equal(neg.area, pos.area))){
    winner = 0
    epsilon = 0.5
  } else if(neg.area > pos.area){
    winner = 1
    epsilon = round(pos.area/total.area, 3)
  } else if(neg.area < pos.area){
    winner = 2
    epsilon = round(neg.area/total.area, 3)
  }

  return(list(winner = winner, epsilon = epsilon, area = area,
              total.area = total.area, positive.area = pos.area,
              negative.area = neg.area))
}

#' Calculates area between CDFs
#'
#' It calculates the area between the CDFs of two prospects divided by the CDFs
#' and outcomes segments.
#'
#' @importFrom dplyr lag
#'
#' @param sd.obj StochasticDominance object.
#' @returns A numeric vector, including the area between the segments of CDFs.
#'
area.btwn.cdfs.calc = function(sd.obj){

  outcome = sd.obj@outcome
  cdf1 = sd.obj@cdf1
  cdf2 = sd.obj@cdf2

  y = (outcome - lag(outcome))[2:length(outcome)]
  cdf1.mod = cdf1[-length(cdf1)]
  cdf2.mod = cdf2[-length(cdf2)]

  return(y * (cdf1.mod - cdf2.mod))
}
