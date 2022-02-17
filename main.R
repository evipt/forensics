load("sample_data.Rdata")

D = data

#stili V1 
#x<-c(D[ ,1])

#stili V2
#y<-c(D[ ,2])

G1 = D[,c(1,2)]
G2 = D[,c(3,4)]
G3 = D[,c(5,6)]
G4 = D[,c(7,8)]
G5 = D[,c(9,10)]

#o mesos oros (deigmatikos mesos) tis emfanisis 
#tou Gi gonidiou ston plithismo
#kathe pij tautizetai me tin sixnotita pij^2 

freq <- function(g, a, b){
  j <- 0
  for(i in 1:10000) {
    if (g[i,1] == a && g[i,2] == b) {
      j <- j + 1
    }
  }
  return(j/10000)
}


#GIA TO G1

#(1,1)
p11 <- freq(G1, 1, 1) ; p11

#(2,2)
p12 <- freq(G1, 2, 2) ; p12

#(1,2) or (2,1)
p112 <- 1 - p11 - p12 ; p112
# <-0 
#for(i in 1:10000) {
#  if((G1[i,1] == 1 && G1[i,2] == 2) || (G1[i,1] == 2 && G1[i,2] == 1)) {
#    j <- j+1
#  }
#}
#p12 <- j/10000 ; p12

#GIA TO G2

#(2,2)
p22 <- freq(G2, 2, 2) ; p22

#(1,1)
p21 <- freq(G2, 1, 1) ; p21

#(1,2) or (2,1)
p212 = 1 - p21 - p22 ; p212

#GIA TO G3   

#(2,2)
p32 <- freq(G3, 2, 2) ; p32

#(1,1)
p31 <- freq(G3, 1, 1) ; p31

#(3,3)
p33 <- freq(G3, 3, 3) ; p33

#(1,2) or (2,1)
j <- 0 
for(i in 1:10000) {
    if((G3[i,1] == 1 && G3[i,2] == 2) || (G3[i,1] == 2 && G3[i,2] == 1)) {
      j <- j+1
    }
  }
p312 <- j/10000 ; p312

#(1,3) or (3,1)  
j <- 0 
for(i in 1:10000) {
  if((G3[i,1] == 1 && G3[i,2] == 3) || (G3[i,1] == 3 && G3[i,2] == 1)) {
    j <- j+1
  }
}
p313 <- j/10000 ; p313

#(2,3) or (3,2)
j <- 0 
for(i in 1:10000) {
  if((G3[i,1] == 3 && G3[i,2] == 2) || (G3[i,1] == 2 && G3[i,2] == 3)) {
    j <- j+1
  }
}
p323 <- j/10000 ; p323


#GIA TO G4

#(1,1)
p41 <- freq(G4, 1, 1) ; p41

#(2,2)
p42 <- freq(G4, 2, 2) ; p42

#(3,3)
p43 <- freq(G4, 3, 3) ; p43

#(1,2) or (2,1)
j <- 0 
for(i in 1:10000) {
  if((G4[i,1] == 1 && G4[i,2] == 2) || (G4[i,1] == 2 && G4[i,2] == 1)) {
    j <- j+1
  }
}
p412 <- j/10000 ; p412

#(1,3) or (3,1) 
j <- 0 
for(i in 1:10000) {
  if((G4[i,1] == 1 && G4[i,2] == 3) || (G4[i,1] == 3 && G4[i,2] == 1)) {
    j <- j+1
  }
}
p413 <- j/10000 ; p413

#(2,3) or (3,2)
j <- 0 
for(i in 1:10000) {
  if((G4[i,1] == 3 && G4[i,2] == 2) || (G4[i,1] == 2 && G4[i,2] == 3)) {
    j <- j+1
  }
}
p423 <- j/10000 ; p423

#GIA TO G5

#(3,3)
p53 <- freq(G5, 3, 3) ; p53

#(1,1)
p51 <- freq(G5, 1, 1) ; p51

#(2,2)
p52 <- freq(G5, 2, 2) ; p52

#(1,2) or (2,1)
j <- 0 
for(i in 1:10000) {
  if((G5[i,1] == 1 && G5[i,2] == 2) || (G5[i,1] == 2 && G5[i,2] == 1)) {
    j <- j+1
  }
}
p512 <- j/10000 ; p512

#(1,3) or (3,1) 
j <- 0 
for(i in 1:10000) {
  if((G5[i,1] == 1 && G5[i,2] == 3) || (G5[i,1] == 3 && G5[i,2] == 1)) {
    j <- j+1
  }
}
p513 <- j/10000 ; p513

#(2,3) or (3,2)
j <- 0 
for(i in 1:10000) {
  if((G5[i,1] == 3 && G5[i,2] == 2) || (G5[i,1] == 2 && G5[i,2] == 3)) {
    j <- j+1
  }
}
p523 <- j/10000 ; p523


frequency <- c(p11, p22, p32, p41, p53) ; frequency

#i aixnotita emfanisis tou identified gonidiou simfona me ti 
#deigmatiki sixnotita emfanisis tou kathe gonidiou (efoson einai anexartita)
#L0 <- p11*p22*p32*p41*p53 ; L0
L0 <- prod(frequency) ; L0

#i pithanotita o Babis ws gios twn mother & father na exei to identified
#dna sequence gia ta Gi (efoson einai anexartita)
L1 <- 1*1 * (1/2)*(1/2) * (1/2)*(1/2) * (1)*(1/2) * (1/2)*(1/2) ; L1

LR <- L1/L0; LR


#ftiaxnw deigmata dna me tis pithanotites sixnotitas pou exw vrei 
l <- 1e+07 ; l
new_data = c()
set.seed(185)
for(i in 1:5){
  a = sample(c(11,22,12), l, replace = T, prob = c(p11, p12, p112))
  b = sample(c(11,22,12), l, replace = T, prob = c(p11, p12, p112)) 
  c = sample(c(11,22,33,12,13,23), l, replace = T, prob = c(p31, p32, p33, p312 , p313, p323))
  d = sample(c(11,22,33,12,13,23), l, replace = T, prob = c(p41, p42, p43, p412 , p413, p423))
  e = sample(c(11,22,33,12,13,23), l, replace = T, prob = c(p51, p52, p53, p512 , p513, p523))
  
  rdata <- cbind(a,b,c,d,e)
  colnames(rdata) = c("G1", "G2","G3","G4","G5")
  
  j <- 0 
  for (i in 1:l){
    if(rdata[i,1] == 11 && rdata[i,2] == 22 && 
       rdata[i,3] == 22 && rdata[i,4] == 11 && 
       rdata[i,5] == 33){
      
        j <- j+1 
      }
  }
  new_data = c(new_data, j) ;   #lista me plithos atomon me "dna = evidence dna" gia 5 tuxaia deigmata apo 1e+07=10000000 atoma
}

#sixnotita emfanisis atomou me "dna = evidence dna" gia 5 tyxaia peiramata 
new_freq <- new_data/l ; print(new_freq)

#oi logoi pithanofaneiwn an eixa ta parapanw 5 deigmata gia data
new_LR <- c()
for (i in 1:5){
  new_LR = c(new_LR, L1/new_freq[i])
}
new_LR




