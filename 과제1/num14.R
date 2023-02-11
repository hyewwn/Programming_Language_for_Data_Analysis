menus <- c('떡', '어묵', '소스', '떡볶이')
calories <- c(541, 213, 120, NA)
menu_cal <- data.frame(menus, calories)
menu_cal

#1
cal_chk <- c('떡', '어묵', '소스')
total_cal <- 0

for(menu in cal_chk) {
  for(i in 1:nrow(menu_cal)) {
    if(menu_cal[i,1]==menu){
      cal=menu_cal[i,2]
      total_cal=total_cal + cal
      print(paste0(menu, '의 칼로리는 ',cal))
    }
  }
}
print(paste0('떡/어묵/소스 칼로리의 합은 ',total_cal))
#2
for(i in 1:nrow(menu_cal)) {
  for(j in 1:ncol(menu_cal)) {
    if(is.na(menu_cal[i,j])) {
      menu_cal[i,j]=total_cal
    }
  }
}
menu_cal
