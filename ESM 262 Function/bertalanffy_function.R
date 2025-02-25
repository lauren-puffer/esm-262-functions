#Bertalanffy Function
#ESM 262 HW1
#Author: Lauren Puffer

#' Bertalanffy Age-Length Function for Coho Salmon
#' 
#' This is a function that calculates the age of a fish in years given the fork length in centimeters and the weight in grams
#' 
#' @param tl Fork length (cm)
#' @param w Weight (g)
#' @param 109.22 (cm) The maximum length of an adults Coho salmon. 
#' @param k The mean biological condition coefficient which indicates the health of the fish population. 
#' @return t (years)

#create function where tl is total length (cm) and w is weight (g) in 

bertalanffy <- function(tl, w) {
  
  # Error checks for negative values
  ifelse((tl <0), return("Total length cannot be negative."), tl)
  
  ifelse((w<0), return("Weight cannot be negative."), w)
  
  #Check that both vectors are numeric
  if (!is.numeric(tl) || !is.numeric(w)) {
    return("Both total length (tl) and weight (w) must be numeric.")
  }
  
  
  k = ( 100 )*(w / (tl ^ 3)) 
  #mean biological condition factor used in Bertalanffy eqn. 
  
  t = ( - k ) * log( (109.22-tl) /109.22) 
  #avg. fry length is 2.5 cm. max adult length is 109.22
  
  #write error codes for uneven lengths of values
  if (length(tl) != length(w)) {
    return("Number of length values is not equal to number of weight values.")
  }
  
  return(t) #get the age of the fish
}