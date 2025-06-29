set.seed(1)
# n = initialBankRoll,  m = desiredWinnings
#First test case, n=100, m=10, people=100, maxBets=500, seed(1) => 34% W, 1% L, $79.44 mean ending
#Second test case, n=100, m=20, people=100, maxBets=500, seed(1) => 10% W, 0% L, $74.8 mean ending
#Third test case, n=100, m=50, people=100, maxBets=500, seed(1) => 0% W, 0% L, $74.36 mean ending
#Fourth test case, n=100, m=100, people=100, maxBets=500, seed(1) => 0% W, 0% L, $74.36 mean ending

initialBankRoll = 100;
desiredWinnings = 10; 
p = 18/38; #probability of gambler winning



numberOfGamblers = 100;
maxNumberOfBets = 500;

timeToLose = matrix(,numberOfGamblers);
timeToWin  = matrix(,numberOfGamblers);
potAtEnd   = matrix(,numberOfGamblers);
  
for(i in 1:numberOfGamblers) #outer loop iterating over each gambler
{
  # Initialization of internal loop variables
  betCount = 0; 
  winner   = 0;
  loser    = 0;
  n = initialBankRoll; # initialBankRoll defined outside of loops
  
  while(betCount < maxNumberOfBets) # Betting loop(capped) for each gambler 
  { 
    betCount = betCount + 1; 
    nNew = n + sample(c(-1,1), size = T, replace = TRUE, prob = c(1-p, p)); 
    n = nNew; 
    
    if(n == 0) # Control statement to record data if gambler goes bust
    {
      loser=1;
      timeToLose[i] = betCount;
      break; # Gambler went bust, exit to outer loop
    }
    
    if(n == (initialBankRoll+desiredWinnings) )# record if gambler wins
    {
      winner=1;
      timeToWin[i] = betCount;
      break; # Gambler wins, exit to outer loop
    }
  } #End inner loop simulating gambler's run
  potAtEnd[i] = n; # Records winnings of each gambler. 
} #End outer loop iterating over gamblers


meanTimeToBust = mean(timeToLose, na.rm = TRUE)
percentageOfBusts =  100*sum(!is.na(timeToLose))/numberOfGamblers;
meanTimeToWin = mean(timeToWin, na.rm = TRUE);
percentageOfWinners =  100*sum(!is.na(timeToWin))/numberOfGamblers;


print(paste0("Mean time to bust = ", meanTimeToBust))
print(paste0("Percentage of busts = ", percentageOfBusts,"%"))
print(paste0("Mean time to win = ", meanTimeToWin))
print(paste0("Percentage of wins = ", percentageOfWinners,"%"))
print(paste0("Pot at end = $", mean(potAtEnd)))

