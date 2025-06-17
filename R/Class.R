#' StochasticDominance Class
#'
#' Represents two distributions (prospects) that are going to be compared using
#' Stochastic Dominance (SD).
#'
#' @details
#' It contains the input validation needed for comparing two prospects. For example,
#' having sorted `outcome`, each of `prob1` and `prob2` adds up to one, arguments
#' having the same lengths, and having matched probability, cumulative, and ssd
#' arguments.
#'
#' @importFrom methods setClass
#'
#' @slot outcome Numeric vector. The combined outcome values in ascending order.
#' @slot prob1,prob2 Numeric vectors. Probabilities corresponding to the prospects.
#' @slot cdf1,cdf2 Numeric vectors. Cumulative values corresponding to the
#' prospects.
#' @slot ssd1,ssd2 Numeric vectors. SSD values corresponding to the prospects.
#'
#' @export
setClass(
  'StochasticDominance',
  slots = list(outcome = 'numeric',
               prob1 = 'numeric', prob2 = 'numeric',
               cdf1 = 'numeric', cdf2 = 'numeric',
               ssd1 = 'numeric', ssd2 = 'numeric'),
  validity = function(object){
    if(is.unsorted(object@outcome)) return('Outcome must be sorted in ascending order.')
    if(!isTRUE(all.equal(sum(object@prob1),1))) return('Prob1 must add up to one.')
    if(!isTRUE(all.equal(sum(object@prob2),1))) return('Prob2 must add up to one.')
    if(length(object@outcome) != length(object@prob1) |
       length(object@outcome) != length(object@prob2) |
       length(object@outcome) != length(object@cdf1) |
       length(object@outcome) != length(object@cdf2) |
       length(object@outcome) != length(object@ssd1) |
       length(object@outcome) != length(object@ssd2)) {
      return('Length of the input arguments must be equal.')
    }
    if(any(cumsum(object@prob1) != object@cdf1) |
       any(cumsum(object@prob2) != object@cdf2)) {
      return('Probability and cumulative arguments do not match.')
    }
    if(any(ssd.calc(object@outcome, object@cdf1) != object@ssd1) |
       any(ssd.calc(object@outcome, object@cdf2) != object@ssd2)) {
      return('SSD arguments do not match with others.')
    }
  }
)

#' Constructor of StochasticDominance Class
#'
#' It is much easier to use this constructor to create an instance of the
#' StochasticDominance class. It handles calculation of the cdf and ssd values
#' in an efficient way.
#'
#' @seealso [StochasticDominance()]
#'
#' @importFrom dplyr full_join mutate arrange group_by summarise ungroup
#' @importFrom tidyr replace_na
#' @importFrom magrittr "%>%"
#' @importFrom methods new
#'
#' @param outcome1,outcome2 Numeric vectors. The outcomes corresponding to each
#' prospect.
#' @param prob1,prob2 Numeric vectors. The probabilities corresponding to each
#' prospect.
#' @returns StochasticDominance object.
#' @examples
#' createStochasticDominance(outcome1 = c(1,4,7),
#'                           outcome2 = c(2,3,5),
#'                           prob1 = c(1/3,1/3,1/3),
#'                           prob2 = c(1/6,1/6,2/3))
#'
#' @export
createStochasticDominance = function(outcome1, outcome2, prob1, prob2){

  if(length(outcome1) != length(prob1)){
    stop("Error: The length of 'outcome1' and 'prob1' must be equal.")
  }

  if(length(outcome2) != length(prob2)){
    stop("Error: The length of 'outcome2' and 'prob2' must be equal.")
  }

  if(!all.equal(sum(prob1),1)){
    stop("Error: The summation of 'prob1' must be one.")
  }

  if(!all.equal(sum(prob2),1)){
    stop("Error: The summation of 'prob2' must be one.")
  }

  df1 = data.frame(Yield = outcome1, pr1 = prob1)
  df2 = data.frame(Yield = outcome2, pr2 = prob2)

  df = (df1 %>%
          group_by(Yield) %>%
          summarise(prob1 = sum(pr1)) %>%
          ungroup()) %>%
    full_join((df2 %>%
                 group_by(Yield) %>%
                 summarise(prob2 = sum(pr2)) %>%
                 ungroup()), by = 'Yield') %>%
    replace_na(list(prob1=0, prob2=0)) %>%
    arrange(Yield) %>%
    mutate(cdf1 = cumsum(prob1), cdf2 = cumsum(prob2)) %>%
    mutate(ssd1 = ssd.calc(Yield, cdf1), ssd2 = ssd.calc(Yield, cdf2))

  new('StochasticDominance', outcome = df$Yield, prob1 = df$prob1, prob2 = df$prob2,
      cdf1 = df$cdf1, cdf2 = df$cdf2, ssd1 = df$ssd1, ssd2 = df$ssd2)
}

#' Calculates the SSD values for a prospect.
#'
#' @importFrom dplyr lag
#'
#' @param outcome Numeric vector, indicating the outcome values.
#' @param cdf Numeric vector, indicating the cumulative probabilities.
#' @returns Numeric vector, indicating the SSD values.
#'
ssd.calc = function(outcome, cdf){

  ssd = cumsum(lag(cdf, default = 0) * (outcome - lag(outcome, default = 0)))

  return(ssd)
}
