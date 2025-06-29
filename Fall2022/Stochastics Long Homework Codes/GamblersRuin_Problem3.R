set.seed(7)

#First test case, n=100, w=10, people=100, maxBets=500, seed(1)=> 34% W, 1% L, $79.44 mean ending
#Second test case, n=100, w=20, people=100, maxBets=500, seed(1)=> 10% W, 0% L, $74.8 mean ending
#Third test case, n=100, w=50, people=100, maxBets=500, seed(1)=> 0% W, 0% L, $74.36 mean ending
#Fourth test case, n=100, w=100, people=100, maxBets=500, seed(1)=> 0% W, 0% L, $74.36 mean ending



numberOfseeds = 30;
seeds = sample.int(11235813, numberOfseeds);


initialBankRoll = 10000;
desiredWinnings = 44; 
p = 18/38;



numberOfGamblers = 100;
maxNumberOfBets = 5000;

distributionOfWins   = matrix(,numberOfseeds);

# timeToLose[1]=1;
# mean(timeToLose, na.rm = TRUE)

for (j in 1:numberOfseeds) {
  set.seed(seeds[j])
  timeToLose = matrix(,numberOfGamblers);
  timeToWin  = matrix(,numberOfGamblers);
  potAtEnd   = matrix(,numberOfGamblers);
  
  for(i in 1:numberOfGamblers)
  {
    betCount = 0; 
    winner   = 0;
    loser    = 0;
    n = initialBankRoll;
    
    while(betCount < maxNumberOfBets)
    { 
      betCount = betCount + 1; 
      nNew = n + sample(c(-1,1), size = T, replace = TRUE, prob = c(1-p, p)); 
      n = nNew; 
      
      if(n == 0)
      {
        loser=1;
        timeToLose[i] = betCount;
        break;
      }
      
      if(n == (initialBankRoll+desiredWinnings) )
      {
        winner=1;
        timeToWin[i] = betCount;
        break;
      }
    }
    potAtEnd[i] = n; 
  }
  distributionOfWins[j] = 100*sum(!is.na(timeToWin))/numberOfGamblers;
}

# meanTimeToBust = mean(timeToLose, na.rm = TRUE)
# percentageOfBusts =  100*sum(!is.na(timeToLose))/numberOfGamblers;
# meanTimeToWin = mean(timeToWin, na.rm = TRUE);
# percentageOfWinners =  100*sum(!is.na(timeToWin))/numberOfGamblers;
# 
# 
# print(paste0("Mean time to bust = ", meanTimeToBust))
# print(paste0("Percentage of busts = ", percentageOfBusts,"%"))
# print(paste0("Mean time to win = ", meanTimeToWin))
# print(paste0("Percentage of wins = ", percentageOfWinners,"%"))

