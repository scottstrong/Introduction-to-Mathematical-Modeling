set.seed(1)

numberOfseeds = 30;
seeds = sample.int(11235813, numberOfseeds);


initialBankRoll = 2;
desiredWinnings = 1000; 
p = 18/38;

numberOfGamblers = 100;
maxNumberOfBets = 5000;




distributionOfWins       = matrix(,numberOfseeds);
distributionOfTimeToLose = matrix(,numberOfseeds);

# timeToLose[1]=1;
# mean(timeToLose, na.rm = TRUE)

for (j in 1:numberOfseeds) { #Outer loop runs several groups of gamblers
  set.seed(seeds[j]) #Update the see
  # Re-initilization of vectors to hold win/loss/pot data
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
  distributionOfWins[j] = 100*sum(!is.na(timeToWin))/numberOfGamblers; 
  distributionOfTimeToLose[j] = mean(timeToLose, na.rm = TRUE);
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

