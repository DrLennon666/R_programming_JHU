
rankhospital<-function(state,cause_of_death,num="best"){


# Check that the state and cause of death are both valid arguments
  
# Make a list of unique state abbreviations then check to see if state is in list
state_list<-unique(outcome$State)
if (is.element(state,state_list)==FALSE){stop("invalid state")}


cause_of_death<-"heart attack"
cause_of_death<-tolower(cause_of_death)

three_ways_to_die<-c("heart attack","heart failure", "pneumonia")  
if (is.element(cause_of_death,three_ways_to_die)==FALSE){
  stop("invalid outcome")}


# Read in datafile, I don't feel like settting my workdir keep this in the fn
outcome<-read.csv("C:\\Users\\Craig\\Documents\\R Class\\JHU R Class\\Week 4\\outcome-of-care-measures.csv", colClasses="character")

# The columns of note are:
# 2=Hospital Name
# 11=Heart attack
# 17=Heart Failure
# 23=Pneumonia

# Convert outcome columns to numeric
outcome[,11]<-suppressWarnings(as.numeric(outcome[,11]))
outcome[,17]<-suppressWarnings(as.numeric(outcome[,17]))
outcome[,23]<-suppressWarnings(as.numeric(outcome[,23]))

# Some conditional statements for column choice
if (cause_of_death=="heart attack"){deathCol=11}
if (cause_of_death=="heart failure"){deathCol=17}
if (cause_of_death=="pneumonia"){deathCol=23}


# Make a new DF that only contains rows where
# the value in the state column is equal to state
# Reduce the date down to a substate in which only rows with the chosen state 
# are present
state_outcome=na.omit(outcome[outcome$State==state,])

# Order the hospital name row.  This generates the elements
#  or row to order in the DF
# sort DF by hospital name first
alpha_hosp<-order(state_outcome[,2])
state_outcome<-state_outcome[alpha_hosp,]


# Order the outcome row.  This generates the elements
#  or row to order in the DF
# sort DF rows according to element order
orderCOD<-order(state_outcome[,deathCol])
state_outcome=state_outcome[orderCOD,]

# Generate a ranking list and bind to DF as a new column (47)
rank_list<-1:length(state_outcome$State)
state_outcome<-cbind(state_outcome,rank_list)

# extreme cases
if (num=="best"){num=1}
if (num=="worst"){num=length(state_outcome$State)}
if (num>length(state_outcome$State)|num<1){return(NA)}

the_hospital<-state_outcome[state_outcome$rank_list==num,]
the_hospital<-the_hospital[2]

return(the_hospital)

}
