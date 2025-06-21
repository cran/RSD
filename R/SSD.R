#' Compares random prospects by SSD
#'
#'  It compares two random prospects by the second-order stochastic dominance (SSD).
#'
#' @details
#'
#'  A prospect dominates when its SSD is below the other one. It means that all
#'  element of the SSD vector must be equal or smaller, and at least one element
#'  should be smaller for the dominant prospect.
#'
#' If neither prospect dominates the other by SSD, it returns 0. It means the
#' SSDs intersect each other.
#'
#' @seealso [ssd.calc()] for the calculation.
#'
#' @importFrom methods is
#'
#' @param sd.obj StochasticDominance object.
#' @returns A number. Indicating dominant prospect index.
#' @examples
#' sd = createStochasticDominance(outcome1 = c(1,4,7),
#'                                outcome2 = c(2,3,5),
#'                                prob1 = c(1/3,1/3,1/3),
#'                                prob2 = c(1/6,1/6,2/3))
#' ssd.test(sd)
#'
#' @export
ssd.test = function(sd.obj){

  if(!is(sd.obj, 'StochasticDominance')){
    stop("Input must be of class 'StochasticDominance'.")
  }

  winner = comparison(sd.obj@ssd1, sd.obj@ssd2)

  return(winner)
}
