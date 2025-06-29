set.seed(1)

numberOfseeds = 100;
stepTotal = 10000;
sunRadius = 6.957*10^(10);
speedOfLightInCM = 2.998*10^(10);
meanFreePath = 1; #0.0031
secondPerYear= 3.1e+7
timePerStep = meanFreePath/speedOfLightInCM;
seeds = sample.int(11235813, numberOfseeds);

# sqrt(5.36*10^(13)*meanFreePath*speedOfLightInCM)

distanceFromOrigin = replicate(numberOfseeds,0);

for (i in 1:numberOfseeds) {
 set.seed(seeds[i])
 S0 <- c(0,0,0);
 Sn <- S0;
 steps=0;
 while(steps<stepTotal) #Loop to simulate RW up to a maxSteps
 {
   randomTheta=runif(1, min=0,max=pi); #generate a random polar angle
   randomPhi = runif(1, min=0,max=2*pi); #generate random azimuthal angle
   # define (x,y,z) motions by standard formulae
   x = meanFreePath * sin(randomTheta)*cos(randomPhi); 
   y = meanFreePath * sin(randomTheta)*sin(randomPhi);
   z = meanFreePath * cos(randomTheta);
   SNew = Sn + c(x,y,z); #update the three-space location of the walker
   Sn=SNew; 
   steps = steps +1;
   radialDistance = sqrt((Sn[1])^2+(Sn[2])^2+(Sn[3])^2); #compute the radial dist.1
 }
 distanceFromOrigin[i] = radialDistance;
}
hist(distanceFromOrigin)