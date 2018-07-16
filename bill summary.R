### money
setwd('/Users/yaolanqiu/Documents/Octopus/list')

## parameters 
p=4 # the number of people here to consider
ourname=c('ylq','lxl','wyt','dxy')   ## our names 

library(stringr)
table=read.csv('try.csv')

n=dim(table)[1]

people_owned=rep(0,p)
people_paid=rep(0,p)
table_owned=c()
for(i in 1:n){
  temp=table[i,3:6]
  share_num=sum(temp=='*')
  amount=table[i,'money']/share_num
  owned=which(temp=='*')
  people_owned[owned]=people_owned[owned]+amount
  empty=rep(0,p)
  empty[owned]=empty[owned]+amount
  table_owned=rbind(table_owned,empty)
  
  paid=as.character(table[i,'paid'])
  paid=c(str_split(c(paid),"[,]"))[[1]]
  paid=as.numeric(paid)
  
  people_paid[paid]=people_paid[paid]+table[i,'money']/length(paid)
}

people_owned
people_paid
people_absolute=people_owned-people_paid

table_owned=round(table_owned,1)
rownames(table_owned)=NULL
table_owned=rbind(table_owned,apply(table_owned,2,sum))
c(table[,2],c(sum(table[,2])))
table_owned=cbind(c(table[,2],c(sum(table[,2]))),table_owned)
c(as.character(table[,1]),'total')
table_owned=cbind(c(as.character(table[,1]),'total'),table_owned)
table_owned=data.frame(table_owned)
colnames(table_owned)=c('item','money',ourname)

### output the report
table_owned
people_absolute
for(i in 2:p){
  print(paste(ourname[i],' should give ylq: $',round(people_absolute[i])))
}