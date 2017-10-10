
#計算したいr×r正方分割表の観測度数を以下の様にベクトルとして入力してください。
#freq <- c((1,1),(1,2),...(1,r),(2,1),...(2,r),(r,1)...(r,r))
#(例)
#374,602,170,64
#18,225,139,71
#4,23,42,55
#2,6,17,53
#の4×4正方分割表の場合
#freq <- c(374,602,170,64,18,255,139,71,4,23,42,55,2,6,17,53)
#として下さい。


#実装例(Tahata et al., 2016)
freq <- c(374,602,170,64,18,255,139,71,4,23,42,55,2,6,17,53)

NI <- ifelse(floor(sqrt(length(freq)))<ceiling(sqrt(length(freq))),stop(message="freq's length is not square number."),sqrt(length(freq)))

row <- gl(NI,NI,length=NI^2)
col <- gl(NI,1,length=NI^2)

sample <- data.frame(freq,row,col)

array_s <- array(0,dim=c(NI,NI))
l = 1
for(i in 1:NI){
  for(j in i:NI){
    if(i==j){
      array_s[i,i]=l
    }else{
      array_s[i,j]=l
      array_s[j,i]=l
    }
    l = l + 1
  }
}

 s <- c(array_s)
 s <- factor(s)

array_cs <-  array(1,dim=c(NI,NI))
for(i in 1:NI){
  for(j in i:NI){
    if(j>i){
      array_cs[i,j]=2
    }
  }
}
cs <- c(aperm(array_cs))



array_f <- array(0,dim=c(NI,NI,NI-1))
for(k in 1:NI-1){
  for(i in 1:NI){
    for(j in 1:NI){
      if(j>i){
        array_f[i,j,k]= j^k-i^k
      }
    }
  }
}
f <- list()
for(k in 2:NI-1){
   f[[k]] <- c(aperm(array_f[,,k]))
 }

#以下手動入力
ELS1 <- glm(freq ~s+cs+f[[1]],family =poisson,data = sample)
ELS2 <- glm(freq ~s+cs+f[[1]]+f[[2]],family =poisson,data = sample)
ELS3 <- glm(freq ~s+cs+f[[1]]+f[[2]]+f[[3]],family =poisson,data = sample)


#ELSk_1 <- glm(freq ~s+cs+f[[1]]+f[[2]]+....f[[NI-1]],family =poisson,data = sample) 一般化した表現
