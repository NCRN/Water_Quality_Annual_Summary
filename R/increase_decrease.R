##function to show increase/decrease in % pass and magnitude of change between past and current data 
increase_decrease <- function(df, char) {
  
  if(df$percent_pass.x[df$CharNameShort==char]-df$percent_pass.y[df$CharNameShort==char] >= 25) {
    print("increased considerably")
  } else if (df$percent_pass.x[df$CharNameShort==char]-df$percent_pass.y[df$CharNameShort==char] < 25 & df$percent_pass.x[df$CharNameShort==char]-df$percent_pass.y[df$CharNameShort==char] >= 10) {
    print("increased")
  } else if (df$percent_pass.x[df$CharNameShort==char]-df$percent_pass.y[df$CharNameShort==char] < 10 & df$percent_pass.x[df$CharNameShort==char]-df$percent_pass.y[df$CharNameShort==char] > 0) {
    print("increased slightly")
  } else if (df$percent_pass.x[df$CharNameShort==char]-df$percent_pass.y[df$CharNameShort==char] < 0 & df$percent_pass.x[df$CharNameShort==char]-df$percent_pass.y[df$CharNameShort==char] > -10) {
    print("decreased slightly")
  } else if (df$percent_pass.x[df$CharNameShort==char]-df$percent_pass.y[df$CharNameShort==char] <= -10 & df$percent_pass.x[df$CharNameShort==char]-df$percent_pass.y[df$CharNameShort==char] > -25) {
    print("decreased")
  } else if (df$percent_pass.x[df$CharNameShort==char]-df$percent_pass.y[df$CharNameShort==char] < -25) {
    print("decreased considerably")
  } else if (df$percent_pass.x[df$CharNameShort==char]==df$percent_pass.y[df$CharNameShort==char]) {
    print("remained the same")
  } 
  
}
