set.seed(1)

x0 = 1.25;
y0 = 1.25;
r=1.0015;
s=0.9994;
alpha = 0.0006;
beta = 0.00025;

tFinal = 7100;
xn=x0;
yn=y0;
x= matrix(0,1,tFinal);
y = matrix(0,1,tFinal);
x[1] =  r*xn-alpha * xn * yn;
y[1] =  s*yn+beta * xn * yn;
for (i in 2:tFinal)
{
  # x[i] = r*x[i-1]-alpha * x[i-1] * y[i-1]+rnorm(1,0,0.01);
  # y[i] = s*y[i-1]+ beta * x[i-1] * y[i-1]+rnorm(1,0,0.01);
  x[i] = r*x[i-1]-alpha * x[i-1] * y[i-1];
  y[i] = s*y[i-1]+ beta * x[i-1] * y[i-1];
}
# Req ggplot2
library(ggplot2)
# time=1:tFinal;
# plot( time, x, type="l", col="red" )
# par(new=TRUE)
# plot( time, y, type="l", col="green" )
X=t(x)
Y=t(y)
df1 = data.frame(X,Y)
ggplot(data=df1, aes(X, Y))+ geom_point() + geom_path()

       