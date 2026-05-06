##function to find month and year of last exceedance
find_last_ex <- function(df, char) {
  
  print(unique(na.omit(df$month_year[df$Date==max(df$Date[df$pass_fail=="F" & df$CharNameShort==char])])))
  
}
