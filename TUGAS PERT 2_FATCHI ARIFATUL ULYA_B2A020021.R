#Nama : Fatchi Arifatul Ulya
#NIM  : B2A020021 (Ganjil)

Additive_RNG<-function(a,z0,c,m,n) {
  xi<-matrix(NA,n,3)
  colnames(xi)<-c("aZ(i-1)+c","Xi","Ui")
  for (i in 1:n) 
  {
    xi[i,1]<-(a*z0+c)
    xi[i,2]<-xi[i,1]%%m
    xi[i,3]<-xi[i,2]/m
    z0<-xi[i,2]
  }
  hist(xi[,3])
  View(xi)
}
Additive_RNG(35,11123,437,138,100)

Additive_RNG<-function(a,z0,c,m,n)
{
  z<-rep(0,100)
  for (i in 1:100) 
  {
    z[i]<-((a*z0)+c)%%m
    z0<-z[i]
  }
  print(z)
}
Additive_RNG(35,11123,437,138,100)


Bernouli_2<-function(n,p) {
  i<-100
  p<-.65
  X<-runif(i)
  Y<-(X<=p)+0
  (tabel<-table(Y)/length(Y))
}
Bernouli_2(100,0.65)
barplot(tabel,main = "Bernoulli")