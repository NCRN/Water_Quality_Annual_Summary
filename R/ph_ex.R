##function to show directionality of pH exceedances
ph_ex <- function(df, mysite) {
  
  if(nrow(df[df$pH_exceed=="acidic",]) > nrow(df[df$pH_exceed=="basic",])) {
    paste("Most pH exceedances at", mysite, "are acidic (lower than 6).", sep=" ")
  } else if(nrow(df[df$pH_exceed=="basic",]) > nrow(df[df$pH_exceed=="acidic",])) {
    paste("Most pH exceedances at", mysite, "are basic (greater than 9).", sep=" ") 
  } else {
    print("")
  }
  
}