#intro_to_R_W5
?numeric
?rnorm
#dnorm为概率密度函数，返回给定x下的概率值。dnorm(z)等价于f(x=z)
#pnorm为概率分布函数。pnorm(z)等价于P[x<=z]
#qnorm为pnorm的逆运算，返回给定P下的z。
#rnorm生成符合正态分布的随机数。

n=1000
x=numeric(n)
x[1]=rnorm(1)
for (i in 2:n){
  e=rnorm(1)
  x[i]=0.8*x[i-1]+0.6*e
}
plot(x,type="l")

##setting negative to 0
for (I in seq_along(x))
  if(x[i]<0)
    x[i]=0

#syn:result=ifelse(condition,yes,no)
x=ifelse(x<0,0,x)

n=5
?sample
x=sample(n)
y=sample(n)
x;y
#set zi=max{xi,yi}
z=numeric(n)
for (i in seq_along(z)){
  if(x[i]>y[i]){
    z[i]=x[i]
  }else{
    z[i]=y[i]
  }
}

z=ifelse(x>y,x,y)
z

#avoid loops
x=rnorm(1e6)
y=rnorm(1e6)
system.time({z=x+y})

system.time({
  z=numeric(length(x))
  for(i in seq_along(x))
    z[i]=x[i]+y[i]
})

?numeric
n=length(x)
d=numeric(n-1)
typeof(numeric(n-1))
for(i in seq_along(d))
  d[i]=x[i+1]-x[i]
d
###avoid loop
n=length(x)
d=x[-1]-x[-n]
d





