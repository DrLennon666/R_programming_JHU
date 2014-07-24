
best<-function(state,cause_of_death){

# Read in datafile, I don't feel like settting my workdir keep this in the fn
outcome<-read.csv("C:\\Users\\Craig\\Documents\\R Class\\JHU R Class\\Week 4\\outcome-of-care-measures.csv", colClasses="character")

# The columns of note are:
# 2=Hospital Name
# 11=Heart attack
# 17=Heart Failure
# 23=Pneumonia

outcome[,11]<-suppressWarnings(as.numeric(outcome[,11]))
outcome[,17]<-suppressWarnings(as.numeric(outcome[,17]))
outcome[,23]<-suppressWarnings(as.numeric(outcome[,23]))

# Check that the state and cause of death are both valid arguments

# Make a list of unique state abbreviations then check to see if state is in list
state_list<-unique(outcome$State)
if (is.element(state,state_list)==FALSE){stop("invalid state")}

three_ways_to_die<-c("heart attack","heart failure", "pneumonia")
#Test case
#cause_of_death<-"heart attack"
cause_of_death<-tolower(cause_of_death)

if (is.element(cause_of_death,three_ways_to_die)==FALSE){
  stop("invalid outcome")}

# Reduce the date down to a substate in which only rows with the chosen state 
# are present
state_outcome=na.omit(outcome[outcome$State==state,])

# Order the hospital name row.  This generates the elements
#  or row to order in the DF
# sort DF by hospital name first
alpha_hosp<-order(state_outcome[,2])
state_outcome<-state_outcome[alpha_hosp,]


# change index
# Returns hospital with least death

if (cause_of_death=="heart attack"){
  return(state_outcome[state_outcome[,11]==min(state_outcome[,11]),2])}

else if (cause_of_death=="heart failure"){
  return(state_outcome[state_outcome[,17]==min(state_outcome[,17]),2])  
}

else if (cause_of_death=="pneumonia"){
  return(state_outcome[state_outcome[,23]==min(state_outcome[,23]),2])  
}

}
