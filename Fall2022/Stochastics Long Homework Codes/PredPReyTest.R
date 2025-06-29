set.seed(1)

numberOfseeds = 100;
seeds = sample.int(11235813, numberOfseeds);

x0 = 0.25;
y0 = 0.25;
initialConditionXList = runif(numberOfseeds, min = 0.25, max = 1.5);
initialConditionYList = runif(numberOfseeds, min = 0.25, max = 1.5);
sampleMeanX = matrix(0,1,numberOfseeds);
sampleMeanY = matrix(0,1,numberOfseeds);
sampleVarX = matrix(0,1,numberOfseeds);
sampleVarY = matrix(0,1,numberOfseeds);
r=1.0015;
s=0.9994;
alpha = 0.0006;
beta = 0.00025;
tFinal = 20000;

for(k in 1:numberOfseeds)
{
  set.seed(seeds[k])  
  xn=initialConditionXList[k];
  yn=initialConditionYList[k];
  x= matrix(0,1,tFinal);
  y = matrix(0,1,tFinal);
  x[1] =  r*xn*(1-xn)-alpha * xn * yn;
  y[1] =  s*yn*(1-yn)+beta * xn * yn;
  for (i in 2:tFinal)
  {
    x[i] = r*x[i-1]-alpha * x[i-1] * y[i-1]+rnorm(1,0,0.01);
    y[i] = s*y[i-1]+ beta * x[i-1] * y[i-1]+rnorm(1,0,0.01);
  }
  sampleMeanX[k] =mean(x); 
  sampleMeanY[k] = mean(y);
  sampleVarX[k] = var(x);
  sampleVarY[k] = var(y);
  print(k)
}
# Req ggplot2
# library(ggplot2)
# time=1:tFinal;
# plot( time, x, type="l", col="red" )
# par(new=TRUE)
# plot( time, y, type="l", col="green" )
# X=t(x)
# Y=t(y)
# df1 = data.frame(X,Y)
# ggplot(data=df1, aes(X, Y))+ geom_point() + geom_path()

