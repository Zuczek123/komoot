f_ab_test <- function(page1, page2,crit_val_z){
  #Standard Error (SE) = Square root of (p * (1-p) / n)
  SE1 = sqrt( page1[1] * (1-page1[1]) / page1[2] )
  SE2 = sqrt( page2[1] * (1-page2[1]) / page2[2] )
  #confidence intervals of the conversion rates for each variation of the site
  conf_interval1 = c((page1[1] - crit_val_z*SE1) * 100, (page1[1] + crit_val_z*SE1) * 100)
  print(round(conf_interval1,1) )
  
  conf_interval2 = c((page2[1] - crit_val_z*SE2) * 100, (page2[1] + crit_val_z*SE2) * 100)
  print(round(conf_interval2,1) )
}

page_ver_A = c(34/1000, 1000) # Version A
page_ver_B = c(84/2000, 2000) # Version B

f_ab_test(page_ver_A, page_ver_B,1.28)  #80%
f_ab_test(page_ver_A, page_ver_B,1.645) #90%
f_ab_test(page_ver_A, page_ver_B,1.96)  #95%
f_ab_test(page_ver_A, page_ver_B,2.33)  #98%
f_ab_test(page_ver_A, page_ver_B,2.58)  #99%