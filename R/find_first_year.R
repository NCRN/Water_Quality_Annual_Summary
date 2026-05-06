##function to find first year a parameter was recorded
find_first_year <- function(df, char) {
  
  print(min(df$sample_year[df$CharNameShort==char], na.rm=T))
  
}
