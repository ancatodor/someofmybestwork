# Anca Todor

#### NOTES #######################################################################

# Once you have opened the script, just click Source, it will take approximatley
# 30-40 minutes to execute the whole script. Once executed, proceed to the last 
# 2 lines of the script (lines 459 and 460) and remove the # . 
# These lines will execute both functions and return a data.frame of the output 
# of both functions side by side for comparison. Please enter your year of choice
# in the arguments of the functions which currently are indicated with the 
# placeholder "YEAR". You do not need to change the first argument of the 
# functions, as I have already entered the appropriate object to call it.
# The functions take two arguments: data (which is the table where the data
# will be pulled from to execute the function), and yr (which is the year you
# wish to see the ranking of). The data that is used for these functions is titled
# "data". When comparing the results of these functions to the actual rankings
# on the Colley website, you will see that the Colley Matrix function is slightly
# more accurate than the Iterative function. If you wish to run each function 
# seperatley then just run colleymatrix(data, "YEAR") or Iterative(data, "YEAR")
#
# lines 28  to 300 are the data import and removing ties and D2 teams
# lines 302 to 351 are where I built the table that you suggested
# lines 354 to 404 are where I built the colleymatrix function
# lines 406 to 457 are where I built the Iterative function

### Importing Data

year=seq(from=1960, to=2010, by=1)
year
x=data.frame()
for(yrItor in year)
{
  names=paste(yrItor, "yr", sep="")
  linkTemp=paste("http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/cf",yrItor,"gms.txt", sep="")
  dataTemp=read.fwf(file=linkTemp, widths=c(10,28,4,28,4,28), header=F)
  season=matrix(yrItor, ncol=1, nrow=nrow(dataTemp))
  dataTemp=cbind(season,dataTemp)
  x=rbind(x,dataTemp)
  }


# Removing date column and location

x=data.frame(x[,-2])
x=data.frame(x[,-6])

# Naming Columns

colnames(x)=c("season", "away.team", "away.score", "home.team", "home.score")

# Removing games ending in a tie

diff=data.frame(diff=x$away.score-x$home.score)

Data=data.frame(cbind(x,diff))

Data=Data[!(Data$diff %in% 0),]

# check if it worked

sum(Data$diff==0)

# remove full data to avoid confusion

rm(x)

### Setting all text data to character data and removing white space


Data[,2]=as.character(Data$away.team)
Data[,4]=as.character(Data$home.team)


trim = function (x) gsub("^\\s+|\\s+$", "", x)

Data[,2]=trim(Data[,2])
Data[,4]=trim(Data[,4])

# Adding columns for game outcome

a.outcome=matrix(NA, ncol=1, nrow=nrow(Data))
h.outcome=matrix(NA, ncol=1, nrow=nrow(Data)) 

## creating dataframe with all neccesary columns

away.team=Data$away.team
away.score=Data$away.score
home.team=Data$home.team
home.score=Data$home.score
season=Data$season
score.diff=Data$diff

Data=data.frame(a.season=season, away.team, away.score, a.outcome, h.season=season,
                home.team, home.score, h.outcome, score.diff)

# function for determining win or loss for each team

hwinloss = function(x){
  if(x<0)
    return("W")
  if(x>0)
    return("L")
  else
    return(NA)
}

awinloss = function(x){
  if(x<0)
    return("L")
  if(x>0)
    return("W")
  else
    return(NA)
}

# applying the function to the outcome columns

Data$a.outcome = sapply(Data$score.diff, awinloss)
Data$h.outcome = sapply(Data$score.diff, hwinloss)

# remove excess data sets and values to avoid confusion

rm(a.outcome,dataTemp, diff,h.outcome, away.score,away.team,home.score,home.team,
   linkTemp,names,score.diff,season,year,yrItor)

# fixing data type

Data$away.team=as.character(Data$away.team)
Data$a.outcome=as.character(Data$a.outcome)
Data$home.team=as.character(Data$home.team)
Data$h.outcome=as.character(Data$h.outcome)

# creating objects for each column

a.season=Data$a.season
away.team=Data$away.team
away.score=Data$away.score
a.outcome=Data$a.outcome
h.season=Data$h.season
home.team=Data$home.team
home.score=Data$home.score
h.outcome=Data$h.outcome
score.diff=Data$score.diff


# Stack home and away data

SeasonStack=c(a.season,h.season)
TeamStack=c(away.team, home.team)
Stack=data.frame(cbind(SeasonStack,TeamStack))

# Counting number of games for Stacked data


seasons=unique(SeasonStack)

GameStack=data.frame()
for(yrItor in seasons)
{
  stack.teams=unique(Stack$TeamStack[Stack$SeasonStack==yrItor])
  tot.stack.teams=Stack$TeamStack[Stack$SeasonStack==yrItor]
  for(teamItor in stack.teams)
  {
    t.temp=table(tot.stack.teams)
    t.temp=t.temp[tot.stack.teams]
    ngames=sum(t.temp[teamItor])    
    ngames=cbind(yrItor, teamItor,ngames)
    GameStack=rbind(GameStack, ngames)
  }
  
}

# add a blank column for division based on ngames

division=matrix(NA, ncol=1, nrow=nrow(GameStack))
GameStack$yrItor=as.character(GameStack$yrItor)
GameStack$teamItor=as.character(GameStack$teamItor)
GameStack$ngames=as.numeric(GameStack$ngames)
str(GameStack)

GameStack= data.frame(year=GameStack$yrItor, team=GameStack$teamItor, 
                      ngames=GameStack$ngames, division=division)

# Generate Division tags for each team in GameStack

div = function(x){
  if(x<=2)
    return("D2")
  if(x>2)
    return("D1")
  else
    return(NA)
}

GameStack$division = sapply(GameStack$ngames, div)

# Making columns for division in Data

a.division=matrix(NA, ncol=1, nrow=nrow(Data))
h.division=matrix(NA, ncol=1, nrow=nrow(Data)) 
Data=data.frame(a.season,away.team, away.score, a.outcome,a.division,h.season,
                home.team, home.score, h.outcome, h.division, score.diff)

# Populating division columns in Data

large=data.frame()
for(yrItor in seasons)
{
  teama=as.character(Data$away.team[Data$a.season==yrItor])
  find=match(teama, GameStack$team[GameStack$year==yrItor])
  find=(GameStack$division[GameStack$year==yrItor])[find]
  tester=cbind(yrItor, teama, find)
  large=rbind(large, tester)
}

Data$a.division=large[,3]

# removing exces vars from environment

rm(Stack,a.division, division,h.division,large,ngames,tester,SeasonStack,TeamStack,
   a.outcome,a.season,away.score,away.team,find,h.outcome,h.season,home.score,
   home.team,score.diff,stack.teams, t.temp, teamItor, teama,tot.stack.teams)

### Home teams now

large=data.frame()
for(yrItor in seasons)
{
  teamh=as.character(Data$home.team[Data$h.season==yrItor])
  find=match(teamh, GameStack$team[GameStack$year==yrItor])
  find=(GameStack$division[GameStack$year==yrItor])[find]
  tester=cbind(yrItor, teamh, find)
  large=rbind(large, tester)
}

Data$h.division=large[,3]
h.division=Data$h.division

## Removing all D2 Teams and Games

Data=Data[!(Data$a.division=="D2"),]

Data=Data[!(Data$h.division=="D2"),]

# Remove all un-neccesary data, including GameStack and re-calculate ngames

rm(GameStack,large,tester,find,teamh,yrItor)

# fixing data type

Data$away.team=as.character(Data$away.team)
Data$a.outcome=as.character(Data$a.outcome)
Data$home.team=as.character(Data$home.team)
Data$h.outcome=as.character(Data$h.outcome)

# creating objects for each column for simplicity

a.season=Data$a.season
away.team=Data$away.team
away.score=Data$away.score
a.outcome=Data$a.outcome
h.season=Data$h.season
home.team=Data$home.team
home.score=Data$home.score
h.outcome=Data$h.outcome
score.diff=Data$score.diff

# Stack home and away data

SeasonStack=c(a.season,h.season)
TeamStack=c(away.team, home.team)
Stack=data.frame(cbind(SeasonStack,TeamStack))

# Counting number of games for Stacked data

GameStack=data.frame()
for(yrItor in seasons)
{
  stack.teams=unique(Stack$TeamStack[Stack$SeasonStack==yrItor])
  tot.stack.teams=Stack$TeamStack[Stack$SeasonStack==yrItor]
  for(teamItor in stack.teams)
  {
    t.temp=table(tot.stack.teams)
    t.temp=t.temp[tot.stack.teams]
    ngames=sum(t.temp[teamItor])    
    ngames=cbind(yrItor, teamItor,ngames)
    GameStack=rbind(GameStack, ngames)
  }
  
}

colnames(GameStack)=c("year","team","ngames")

summary(GameStack$ngames) # no teams with fewer than 3 games remain

#removing excess

rm(Stack,ngames, SeasonStack,TeamStack,a.outcome,a.season,away.score,away.team,
   h.outcome,h.season,home.score, home.team, score.diff, stack.teams, t.temp,
   teamItor,tot.stack.teams,yrItor, seasons)

# Making the suggested table


season=unique(GameStack$year)

GameStack2=data.frame()
for(yrItor in season)
{
  teams=unique(GameStack$team[GameStack$year==yrItor])
  for(teamItor in teams)
  {
    t.h.teams=Data$h.outcome[Data$h.season==yrItor & Data$home.team==teamItor]
    t.a.teams=Data$a.outcome[Data$a.season==yrItor & Data$away.team==teamItor]
    tot.teams=c(t.h.teams,t.a.teams)
    wins=sum(tot.teams=="W")
    loss=sum(tot.teams=="L")
    games=length(tot.teams)
    op1=Data$home.team[Data$a.season==yrItor & Data$away.team==teamItor]
    op2=Data$away.team[Data$h.season==yrItor & Data$home.team==teamItor]
    op=c(op1,op2)
    id=cbind(yrItor, teamItor,wins, loss, games, list(op))
    GameStack2=rbind(GameStack2, id)
    
  }
}

# removing excess for clarity

rm(id, games, h.division, loss,op,op1,op2,season,
   t.a.teams,t.h.teams, teamItor,teams, tot.teams,wins,yrItor)

# fixing column names

colnames(GameStack2)=c("year", "team", "wins", "losses", "games", "opponents")

# creating data frame

year=as.numeric(as.vector(unlist(GameStack2$year)))
team=as.character(as.vector(unlist(GameStack2$team)))
wins=as.numeric(as.vector(unlist(GameStack2$wins)))
losses=as.numeric(as.vector(unlist(GameStack2$losses)))
games=as.numeric(as.vector(unlist(GameStack2$games)))

opponents=matrix(ncol=1, nrow=6398)
for(ritor in 1:6398)
{
  opponents[ritor]=toString(noquote(c(GameStack2$opponents[[ritor]])))
}

data=data.frame(year,team,wins,losses,games,opponents)


# Colley matrix function

colleymatrix = function(data,yr)
{
{
  workingdata=data[(data[,1]==yr),]
  workingdata=workingdata[with(workingdata, order(team)),]
  workingdata[,2]=gsub(" ", "", as.character(workingdata[,2]))
  workingdata[,6]=gsub(" ", "", as.character(workingdata[,6]))
  cnames=sort(workingdata$team)
  rnames=sort(workingdata$team)
  matrix=matrix(nrow=length(rnames), ncol=length(cnames),
                dimnames=list(rnames,cnames))
  for(i in 1:nrow(matrix))
  {
    for(j in 1:ncol(matrix))
    {
      if(i==j){
        matrix[i,j]=(workingdata[i,5])+2
      } else {
        if(any(unlist(strsplit(workingdata[i,6], split=","))==workingdata[j,2]))
        {
          matrix[i,j]=-sum(unlist(strsplit(workingdata[i,6],
                                           split=","))==workingdata[j,2])
        } else {
          matrix[i,j]=0
        }
      }
    }
  }
  b=matrix()
  workingdata=data[(data[,1]==yr),]
  workingdata=workingdata[with(workingdata, order(team)),]
  cnames=sort(workingdata$team)
  for(teamI in cnames)
  {
    n_wi=workingdata$wins[workingdata$team==teamI]
    n_li=workingdata$losses[workingdata$team==teamI]
    b_i=1+(n_wi-n_li)/2
    b=rbind(b, b_i)
  }
  b=as.numeric(b[-1,])
  r=solve(matrix,b)
  rankings=data.frame(teams=cnames,Ranking=r)
  Ranking=data.frame(rankings[with(rankings, order(rankings$Ranking, 
                                                   decreasing=TRUE)),])
  Ranking=cbind(data.frame(Ranking[,-2]), data.frame(Ranking[,-1]))
  colnames(Ranking)=c("Team", "Ranking Metric")
}
  return(Ranking)
}
  
# Iterative method

Iterative = function(data,yr)
{
  workingdata=data[(data[,1]==yr),]
  workingdata=workingdata[with(workingdata, order(team)),]
  workingdata[,2]=gsub(" ", "", as.character(workingdata[,2]))
  workingdata[,6]=gsub(" ", "", as.character(workingdata[,6]))  
  n_w=workingdata$wins
  n_l=workingdata$losses
  n_tot=workingdata$games
  neff=matrix(ncol=1, nrow=nrow(workingdata))
  rnew=matrix(ncol=1, nrow=nrow(workingdata))
  r_i=(1+n_w)/(2+n_tot)
  x=data.frame(team=workingdata$team,n_w, n_l,n_tot,r_i, neff, rnew,
               opponents=workingdata$opponents)
  x[,1]=as.character(x[,1])
  counter=1
  while(counter<3){  
  k=1
  for(tItor in x$team)
  {
    ops=x$opponents[x$team==tItor]
    ops=unlist(strsplit(as.character(ops), split=","))
    temp=matrix(ncol=1, nrow=(length(ops)))
    i=1
    for(oItor in ops)
    {
    r_ij=x$r_i[x$team==oItor]
    temp[i]=r_ij
    i=i+1
    }
    sr_ij=sum(temp)
    nwi=x$n_w[x$team==tItor]
    nli=x$n_l[x$team==tItor]
    n_eff=((nwi-nli)/2)+sr_ij
    x[k,6]=n_eff
    rnew=(1+n_eff)/(2+(nwi+nli))
    x[k,7]=rnew
    k=k+1
  }
  x[,2]=x[,6]
  x[,5]=x[,7]
  counter=counter+1
}

rankings=data.frame(Team=x[,1], RankScore=x[,7])
Ranking=data.frame(rankings[with(rankings, order(rankings$RankScore, 
                                                 decreasing=TRUE)),])

return(Ranking)
}

#comp=data.frame(cbind(colleymatrix(data,"YEAR"), Iterative(data,"YEAR")))
#comp
























    
    
    
    
    
    