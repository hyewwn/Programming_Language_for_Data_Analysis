sentence <- "데/이/터/사/이/언/스"
wrong_sentence <- "테/이/어/사/이/언/스"
sentence_letter <- strsplit(sentence,"/")
wrong_sentence_letter <- strsplit(wrong_sentence, "/")
m <- 0
v <- vector()
a <- unlist(sentence_letter)
b <- unlist(wrong_sentence_letter)

repeat {
 m <- m+1
 v <- c(v, subset(m, a[m] != b[m]))
 if(m>=length(a)) break
}

cat(v, "번째 글자가 틀렸습니다. '데이터사이언스'로 다시 입력하세요.")
