##function to find month and year last passing measurement
find_last_pass <- function(df, char) {
  
  print(unique(na.omit(df$month_year[df$Date==max(df$Date[df$pass_fail=="P" & df$CharNameShort==char], na.rm=T)])))
  
}