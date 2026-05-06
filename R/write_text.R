##creating function to paste text
write_text <- function(df, char, data_wide, data_long) {
  
  ##text options
  text1 <- paste("All", data_wide$CharNameLower[data_wide$CharNameShort==char], "measurements at", mysite, "were passing in", myyear, "and the five years prior.", sep=" ")
  text2 <- paste("All", data_wide$CharNameLower[data_wide$CharNameShort==char], "measurements at", mysite, "were passing in", myyear, sep=" ")
  text3 <- paste("All", data_wide$CharNameLower[data_wide$CharNameShort==char], "measurements at", mysite, "exceeded the acceptable threshold", "in", myyear, "and the five years prior.", sep=" ")
  text4 <- paste("All", data_wide$CharNameLower[data_wide$CharNameShort==char], "measurements at", mysite, "exceeded the acceptable threshold", "in", myyear, sep=" ")
  text5 <- paste(paste(data_long$major_P_F[data_long$CharNameShort==char & data_long$years==myyear], ".", sep=""),"The percentage of passing measurements",  
                 increase_decrease(data_wide, char), "compared to the five years prior.", sep=" ")
  text6 <- paste(paste(data_long$major_P_F[data_long$CharNameShort==char & data_long$years==myyear], ".", sep=""),"The percentage of passing measurements",  
                 increase_decrease(data_wide, char), "as the five years prior.", sep=" " )
  text7 <- paste("The percentage of passing measurements",  increase_decrease(data_wide, char), "compared to the five years prior.", sep=" ")
  
  ##conditions for each text option
  if (mean(df$percent_pass[df$CharNameShort==char & df$sample_year>=start_year], na.rm=T)==100) {
    my_text <- text1
  } else if (mean(df$percent_pass[df$CharNameShort==char & df$sample_year==myyear], na.rm=T)==100 &
             mean(df$percent_pass[df$CharNameShort==char & df$sample_year>=start_year], na.rm=T)!=100) {
    my_text <- paste(paste(text2, ".", sep=""), text7, sep=" ")
  } else if (mean(df$percent_pass[df$CharNameShort==char & df$sample_year>=start_year], na.rm=T)==0) {
    my_text <- text3
  } else if (mean(df$percent_pass[df$CharNameShort==char & df$sample_year==myyear], na.rm=T)==0 &
             mean(df$percent_pass[df$CharNameShort==char & df$sample_year>=start_year], na.rm=T)!=0) {
    my_text <- paste(paste(text4, ".", sep=""), text7, sep=" ")
  } else if (increase_decrease(data_wide, char) %in% c("decreased considerably", "decreased", "decreased slightly", "increased considerably", "increased", "increased slightly")) {
    my_text<-text5
  } else if (mean(df$percent_pass[df$CharNameShort==char & df$sample_year>=start_year], na.rm=T)!=100 &
             mean(df$percent_pass[df$CharNameShort==char & df$sample_year>=start_year], na.rm=T)!=0 &
             increase_decrease(data, char)=="remained the same") {
    my_text <- text6
  }else {
    my_text <- print("test")
  }
  
  return(my_text)
  
}