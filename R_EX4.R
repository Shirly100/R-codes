#Shirly Ohanona 314793910

library("rentrez")
seq<-entrez_fetch(db="nuccore",id=508550,rettype="fasta")
cat(seq,sep="\n")
write(seq, file="mySeq.fasta")
install.packages("seqinr")
library("seqinr")
myFile <- read.fasta(file="mySeq.fasta")
mySequence<-myFil?[[1]]



AT<-function(seq)
{
  count<-count(mySequence, 2)
  fat<-count[[4]]/sum(count)
   
}
result <- AT(mySequence)
print(result)



GC_window<-function(mySequence,sizeWindow)
{
  starts <- seq(1,length(mySequence), by = sizeWindow)
  n <- length(starts?-1
  for (i in 1:n){
    chunk <- mySequence[starts[i]:(starts[i]+(sizeWindow-1))]
    chunkGC <- GC(chunk)
    print(chunkGC)
  }
  chunkGCs <- numeric(n)
  for (i in 1:n){
    chunk <- mySequence[starts[i]:(starts[i]+(sizeWindow-1))]
    chunkGC <- GC(ch?nk)
    print(chunkGC)
    chunkGCs[i] <- chunkGC
  }
  plot(starts[1:n],chunkGCs,type="b", main=paste("Window size: ",sizeWindow ),xlab="Nucleotide start position",ylab="GC content")
  
}


window <- readline(prompt="Enter a window size: ")
window<- as.in?eger(window)
result <- GC_window(mySequence,window)
print(result)


representation<-function(seq)
{
  c1<-count(seq, 1)
  c2<-count(seq, 2)
  letter=c('a','c','g','t')
  len=length(c1)
  flag<-0
  for(i in 1:len)
  {
    
    for(j in 1:len)
    {
      fl?g<-flag+1
      f1 = count(seq, 1)[[i]]/sum(count(seq, 1))
      f2 = count(seq, 1)[[j]]/sum(count(seq, 1))
      f12 = count(seq, 2)[[flag]]/sum(count(seq, 2))
      title=paste(letter[i],letter[j],":")
      t=gsub( " ", "", title)
      result=paste(t,(?12/(f1*f2)))
      if ((f12/(f1*f2))>1) {
        type<-"over represented:"
      } else {
        type<-"under represented:"
      }
      result=paste(type,result)
      print(result)
      
    }
    
  }
  
  
}

result2<-representation(mySequence)


