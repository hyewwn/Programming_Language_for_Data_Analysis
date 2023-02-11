signal <- c('초록', '초록', '노랑', '빨강', '노랑', '초록')
for(sig in signal)
  if(sig == '초록') {
    print(paste0(sig, '불입니다. 이동'))
  } else if(sig == '노랑'){
    print(paste0(sig, '불입니다. 천천히'))
  } else{
    print(paste0(sig, '불입니다. 정지'))
  }
