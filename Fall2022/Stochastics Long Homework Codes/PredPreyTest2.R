set.seed(1)

numberOfseeds = 1;
seeds = sample.int(11235813, numberOfseeds);

x0 = 1.25;
y0 = 1.25;
r=1.0015;
s=0.9994;
alpha = 0.0006;
beta = 0.00025;

XTable =  matrix(0,numberOfseeds,tFinal);
YTable =  matrix(0,numberOfseeds,tFinal);

tFinal = 7100;

for(k in 1:numberOfseeds)
{
  xn=x0;
  yn=y0;
  x= matrix(0,1,tFinal);
  y = matrix(0,1,tFinal);
  x[1] =  r*xn-alpha * xn * yn+rnorm(1,0,0.01);
  y[1] =  s*yn+beta * xn * yn+rnorm(1,0,0.01);
  for (i in 2:tFinal)
  {
    # x[i] = r*x[i-1]-alpha * x[i-1] * y[i-1]+rnorm(1,0,0.01);
    # y[i] = s*y[i-1]+ beta * x[i-1] * y[i-1]+rnorm(1,0,0.01);
    x[i] = r*x[i-1]-alpha * x[i-1] * y[i-1]+rnorm(1,0,0.01);
    y[i] = s*y[i-1]+ beta * x[i-1] * y[i-1]+rnorm(1,0,0.01);
  }
  sampleMeanX[k] =mean(x); 
  sampleMeanY[k] = mean(y);
  sampleVarX[k] = var(x);
  sampleVarY[k] = var(y);
  XTable[k,] = t(x);
  YTable[k,] = t(y);
  print(k)
}
# Req ggplot2
library(ggplot2)


# X=t(x)
# Y=t(y)
X = t(XTable)[,1]
Y = t(YTable)[,1]
df1 = data.frame(X,Y)
ggplot(data=df1, aes(X, Y))+ geom_point() + geom_path()
time=1:tFinal;
plot( time, X, type="l", col="red" )
par(new=TRUE)
plot( time, Y, type="l", col="green" )

