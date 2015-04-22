# reading in data
setwd("~/MS ECON/Fall 2014/Computational Methods/Project")
Data=read.csv("fulldata.csv", header=TRUE, as.is=TRUE)
GameStack=read.csv("gamestack.csv", header=TRUE, as.is=TRUE)

Data=Data[-2]
head(Data)
GameStack=GameStack[-2]
head(GameStack)

# Building the colley matrix

cname=sort(unique(Data$home.team[Data$h.season==2010]))
rname=sort(unique(Data$home.team[Data$h.season==2010]))
dims=list(cname,rname)

colley=matrix(NA,nrow=length(unique(Data$home.team[Data$h.season==2010])),
              ncol=length(unique(Data$home.team[Data$h.season==2010])),dimnames=dims)
colley

seasons=unique(GameStack$year)
for(yrItor in seasons)
{
  gstack=cbind(sort(GameStack$team[GameStack$year==2010], GameStack$ngames[GameStack$year==2010]))
  cname=sort(unique(Data$home.team[Data$h.season==yrItor]))
  rname=sort(unique(Data$home.team[Data$h.season==yrItor]))
  dims=list(cname,rname)
  
  colley=matrix(NA,nrow=length(unique(Data$home.team[Data$h.season==yrItor])),
                ncol=length(unique(Data$home.team[Data$h.season==yrItor])),dimnames=dims)
  for(diagItor in 1:nrow(colley))
  {
    
      colley[diagItor]
    
  }
    
    
    
}
  
  
    
colley[1,1]    
    
    
    
    
    
    
    
    
    








    