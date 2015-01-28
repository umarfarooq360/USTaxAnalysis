N1=c(1:nrow(mdata))
ids=c(1:nrow(mdata))
newdata = as.data.frame(ids , N1 )

tmp = 0
j=1;
for(i in 1:nrow(mdata)  ){

  if(i%%8 ==0 && i!=1 ){
    tmp = tmp + mdata[i,"N1"];
    newdata$N1[j] = tmp;
    j=j+1;
    tmp=0;
  }else{
    tmp = tmp + mdata[i,"N1"];
  }


}

