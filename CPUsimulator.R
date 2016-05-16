

BinToDec <- function(x) 
  sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1))

Register1 <- rep(0,32)

example <- paste("000000","00001","00010","00011","10100","100000",sep="")

#See https://en.wikipedia.org/wiki/MIPS_instruction_set

MachineCode <- function(Register,input){
  require(magrittr)
  
  #Addition
  if(substr(input,start=1,stop=6) == "000000"){
    t <- substr(input,start=7,stop=11) %>% BinToDec
    s <- substr(input,start=12,stop=16) %>% BinToDec
    d <- substr(input,start=17,stop=21) %>% BinToDec
    output <- Register[t] + Register[s]
    return(list(position=d,number=output))
  }
}


#Example
Register1[1] <- 10
Register1[2] <- 5
MachineCode(Register=Register1,input=example)