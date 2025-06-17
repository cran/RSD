#' Drawing the CDFs
#'
#' It visualizes the CDFs of both prospects.
#'
#' @details
#' The parameter `names` only accepts character vector, otherwise an error will
#' be raised.
#'
#' The function shows the step plot, and returns its object for further modifications.
#'
#' @importFrom dplyr mutate select case_when
#' @importFrom tidyr pivot_longer separate starts_with
#' @importFrom ggplot2 ggplot aes geom_step theme element_text element_blank guides guide_legend
#' @importFrom magrittr "%>%"
#' @importFrom methods is
#'
#' @param sd.obj StochasticDominance object.
#' @param names A character vector, including the names of prospects in order.
#' @returns A list, including plot elements.
#' @examples
#' sd = createStochasticDominance(outcome1 = c(1,4,7),
#'                                outcome2 = c(2,3,5),
#'                                prob1 = c(1/3,1/3,1/3),
#'                                prob2 = c(1/6,1/6,2/3))
#' fsd.plot(sd, names = c('First', 'Second'))
#'
#' @export
fsd.plot = function(sd.obj, names = c('1', '2')){

  if(!is(sd.obj, 'StochasticDominance')){
    stop("Input must be of class 'StochasticDominance'.")
  }

  if(!is.character(names)){
    stop("Error: argument 'names' must be character.")
  }

  outcome = sd.obj@outcome
  cdf1 = sd.obj@cdf1
  cdf2 = sd.obj@cdf2
  name1 = names[1]
  name2 = names[2]

  data = data.frame('Outcomes' = outcome, 'cdf_1' = cdf1, 'cdf_2' = cdf2)

  df = data %>%
    pivot_longer(cols = starts_with('cdf'), names_to = 'prospects',
                 values_to = 'CDF') %>%
    separate(prospects, sep = '_', into = c('cdf', 'prospects')) %>%
    mutate(Prospects = case_when(prospects == 1 ~ name1, TRUE ~ name2)) %>%
    select(-cdf, -prospects)

  plot = ggplot(df, mapping = aes(x=Outcomes, y=CDF)) +
    geom_step(aes(color = Prospects), linewidth = 1.5, alpha = 0.7) +
    theme(axis.title = element_text(size = 18, face = 'bold'),
          axis.text = element_text(size = 14),
          legend.title = element_text(face = 'bold', size = 18),
          legend.text = element_text(size = 14),
          legend.position = 'bottom',
          legend.background = element_blank()) +
    guides(color = guide_legend(override.aes = list(linewidth = 1.5, size = 10)))

  return(plot)
}

#' Drawing the SSD
#'
#' It visualize the SSD values of both prospects.
#'
#' @details
#' The parameter `names` only accepts character vector, otherwise an error will
#' be raised.
#'
#' The function shows the line plot and returns its object for further modification.
#'
#' @importFrom dplyr mutate select case_when
#' @importFrom tidyr pivot_longer separate starts_with
#' @importFrom ggplot2 ggplot aes geom_line theme element_text element_blank guides guide_legend
#' @importFrom magrittr "%>%"
#' @importFrom methods is
#'
#' @param sd.obj StochasticDominance object.
#' @param names A character vector, including the names of prospects in order.
#' @returns A list, including plot elements.
#' @examples
#' sd = createStochasticDominance(outcome1 = c(1,4,7),
#'                                outcome2 = c(2,3,5),
#'                                prob1 = c(1/3,1/3,1/3),
#'                                prob2 = c(1/6,1/6,2/3))
#' ssd.plot(sd, names = c('First', 'Second'))
#'
#' @export
ssd.plot = function(sd.obj, names = c('1', '2')){

  if(!is(sd.obj, 'StochasticDominance')){
    stop("Input must be of class 'StochasticDominance'.")
  }

  if(!is.character(names)){
    stop("Error: argument 'names' must be character.")
  }

  outcome = sd.obj@outcome
  ssd1 = sd.obj@ssd1
  ssd2 = sd.obj@ssd2
  name1 = names[1]
  name2 = names[2]

  data = data.frame('Outcome' = outcome, 'ssd_1' = ssd1, 'ssd_2' = ssd2)

  df = data %>%
    pivot_longer(cols = starts_with('ssd'), names_to = 'prospects',
                 values_to = 'SSD') %>%
    separate(prospects, sep = '_', into = c('ssd', 'prospects')) %>%
    mutate(Prospects = case_when(prospects == 1 ~ name1, TRUE ~ name2)) %>%
    select(-ssd, -prospects)

  plot = ggplot(df, mapping = aes(x=Outcome, y=SSD)) +
    geom_line(aes(color = Prospects), linewidth = 1.5, alpha = 0.7) +
    theme(axis.title = element_text(size = 18, face = 'bold'),
          axis.text = element_text(size = 14),
          legend.title = element_text(face = 'bold', size = 18),
          legend.text = element_text(size = 14),
          legend.position = 'bottom',
          legend.background = element_blank()) +
    guides(color = guide_legend(override.aes = list(linewidth = 1.5, size = 10)))

  return(plot)
}
