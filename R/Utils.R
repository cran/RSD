#' Calculating Expected value
#'
#' It calculates the expected value of both prospects given their distributions.
#'
#' @param sd.obj StochasticDominance object.
#' @returns A list, including two double elements as the expected value of each
#' prospect.
#'
expected.values = function(sd.obj){

  return(list(mean1 = sum(sd.obj@outcome * sd.obj@prob1),
              mean2 = sum(sd.obj@outcome * sd.obj@prob2)))
}

#' Comparing two numeric vectors
#'
#' This function compares two numeric vectors. The vector whose all elements
#' smaller or equal to all elements of the other one where at least one element
#' is smaller, will be the winner.
#'
#' @details
#' The function returns the index of the winner, meaning 1 or 2, corresponding
#' to parameters `x` and `y`, respectively. If no vector meets the condition, 0
#' will be returned.
#'
#' @param x,y Numeric vectors.
#' @returns An integer, among 0, 1, 2.
#'
comparison = function(x, y){

  if(all(x <= y) & any(x < y)){
    return(1)
  } else if(all(x >= y) & any(x > y)){
    return(2)
  } else{
    return(0)
  }
}

#' Calculates the area between x-axis and a straight line
#'
#' It takes four parameters as the coordinates of two points at the beginning
#' and end of a straight line, and calculates the area exactly below the line
#' (between the line and the x-axis).
#'
#' @param x1,x2 Float values, indicating the first coordinates of start and end
#' points, respectively.
#' @param y1,y2 Float values, indicating the second coordinates of start and end
#' points, respectively.
#' @returns A float.
#'
calc.area.below.line = function(x1, x2, y1, y2){

  area.rect = y1*(x2-x1)
  area.trngl = (y2-y1)*(x2-x1)/2

  return(area.trngl + area.rect)
}

#' If two lines have intersection or no.
#'
#' It determines if two lines have intersection point or not. The start and
#' end points of both lines have equal first coordinate value (the one
#' corresponds to the x-axis).
#'
#' @param x1,x2 Float values.
#' @param y11,y12 Float values, corresponding to the first line.
#' @param y21,y22 Float values, corresponding to the second line.
#' @returns A Boolean.
#'
has.intersection = function(x1, x2, y11, y12, y21, y22){
  if((y11<y21 & y12>y22) | (y11>y21 & y12<y22)){
    return(TRUE)
  }
  return(FALSE)
}

#' Calculates the intersection point of two lines.
#'
#' Given the start and end coordinates of two straight lines, we calculate the
#' intersection point of them.
#'
#' @details
#' Having intersection point has been checked before.
#'
#' @seealso [has.intersect()]
#'
#' @param x1,x2 Float values.
#' @param y11,y12 Float values, corresponding to the first line.
#' @param y21,y22 Float values, corresponding to the second line.
#' @returns A list, including the coordinates of the intersection point.
#'
calc.intersection = function(x1,x2,y11,y12,y21,y22){

  m1 = (y12-y11)/(x2-x1)
  m2 = (y22-y21)/(x2-x1)
  b1 = y11 - m1*x1
  b2 = y21 - m2*x1
  xis = (b2-b1)/(m1-m2)
  yis = m1*xis + b1

  return(list(x.intersect = xis, y.intersect = yis))
}

#' Modify outcome and SSD vectors
#'
#' It modifies outcome vector to have all original plus intersection
#' points in ascending order. The corresponding SSD vectors will also be
#' returned.
#'
#' @param sd.obj StochasticDominance object.
#' @returns A list of three elements.
#'
modif.outcome.ssd.calc = function(sd.obj){

  new.outcome = sd.obj@outcome
  new.ssd1 = sd.obj@ssd1
  new.ssd2 = sd.obj@ssd2

  n = length(sd.obj@outcome)
  for (i in 1:(n-1)) {
    if(has.intersection(sd.obj@outcome[i], sd.obj@outcome[i+1],
                     sd.obj@ssd1[i], sd.obj@ssd1[i+1],
                     sd.obj@ssd2[i], sd.obj@ssd2[i+1])){
      point = calc.intersection(sd.obj@outcome[i], sd.obj@outcome[i+1],
                            sd.obj@ssd1[i], sd.obj@ssd1[i+1],
                            sd.obj@ssd2[i], sd.obj@ssd2[i+1])
      new.outcome = append(new.outcome, point$x.intersect)
      new.ssd1 = append(new.ssd1, point$y.intersect)
      new.ssd2 = append(new.ssd2, point$y.intersect)
    }
  }

  return(list(outcome = sort(new.outcome), ssd1 = sort(new.ssd1),
              ssd2 = sort(new.ssd2)))
}
