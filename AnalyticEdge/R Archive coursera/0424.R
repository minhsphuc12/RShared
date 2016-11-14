#for loop 
count=0
while(count < 10) {
  print(count) 
  count=count+1
}
#while loop
z=5
while (z>=3 && z<=10)
{print (z)
  zx=rbinom(1,1,0.5)
  if (zx==1){z=z+1} 
    else {z=z-1}}
#repeat loop with break
x=1
tolerance=1e-7
repeat {
  x1=computeEstimate()
  if(abs(x1-x)<tolerance){
    break()
  }
  else{
    x=x1
    print(x)
  }
}
#next
for (i in 1:100){
  if (i<=20){
    next
  }
  print (i)
}

#first function
add=function(a,b,m=5){
  s=a+b+m
  print(s)
}
add(3,4)

above=function(x,k){
  x[x>k]
}
above=function(x,k){
  length(x[x>=k])
}

rowmean=function(x){
  row=nrow(x)
  result=numeric(row)
  for (i in 1:row){
    result[i]= mean(x[i,])
  }
  result
}

#function inside function
power=function(n){
  pow=function(x){
    x^n
  }
}
cube=power(3)
square=power(2)
