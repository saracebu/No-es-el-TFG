#' @title  Calculate the area under a polygonal line.
#'
#' @description We introduce two positive numeric vectors x and y, which 
#' must have the same length. To calculate the area, the difference in 
#' absolute value of the current and previous elements of both vectors are
#' calculated, naming them base and height. Next, we calculate the area of 
#' the triangle that has the base and height calculates previously, as well 
#' as the area of the rectangle whose height is the minimum between the 
#' current and previous one and the base is equal to the calculated one. 
#' Finally we add the areas obtaining the total area.
#'
#' @param x Numeric vector of x coordinates
#' 
#' @param y Numeric vector of y coordinates
#' 
#' @return Numeric vector of length 1 with the area under the curve
#' 
#' @example 
#' area(c(0, 1, 2, 3), c(4, 3, 5, 1))
#' 
area <- function(x, y){
  
  if(length(x) != length(y)){
    
    stop("The length of x and y must be the same.")
    
  }
  if(any(x < 0) | any(y < 0)){
    
    stop("Data must be positive.")
    
  }
  
  if (max(y) < .Machine$double.eps) return(1)
  
  ydesp  <- y[-1]
  base   <- abs(diff(x))
  heigth <- abs(diff(y))
  y     <- y[-length(y)]
  A     <- base * heigth / 2 + base * pmin(y, ydesp)
  area  <- sum(A)
  return(area)
}
