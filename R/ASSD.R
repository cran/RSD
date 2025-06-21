#' Compares prospects based on ASSD methods
#'
#' It compares two prospects using ASSD criteria, that is the prospect having
#' the minimum violation area from a classic SSD.
#'
#' @details
#' The `type` argument must be one of the 'll' or 'ths', otherwise it will raise
#' an error.
#'
#' The `epsilon` and `winner` output parameters are the ones that should be
#' taken most. The others are the calculation details and are provided for
#' further investigation. A lower the `epsilon`, lower the violation ratio
#' of the dominant distribution, lower the eliminated extreme utilities, higher
#' the number of decision-makers who agree on the dominant distribution.
#'
#' @seealso [assd.ll.test(), assd.ths.test] for more details.
#'
#' @importFrom methods is
#'
#' @param sd.obj StochasticDominance object.
#' @param type A character vector, including the name of ASSD methods.
#' @returns A list, including calculation details.
#' @examples
#' sd = createStochasticDominance(outcome1 = c(1,4,7),
#'                                outcome2 = c(2,3,5),
#'                                prob1 = c(1/3,1/3,1/3),
#'                                prob2 = c(1/6,1/6,2/3))
#' assd.test(sd, 'll')
#' assd.test(sd, 'ths')
#'
#' @export
assd.test = function(sd.obj, type){

  if(!is(sd.obj, 'StochasticDominance')){
    stop("Input must be of class 'StochasticDominance'.")
  }

  if(!type %in% c('ll', 'ths')) stop("The type must be 'll' or 'ths'.")

  if(type == 'll') return(assd.ll.test(sd.obj))
  if(type == 'ths') return(assd.ths.test(sd.obj))
}
