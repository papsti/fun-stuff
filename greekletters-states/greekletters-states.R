#################
## Set up data ##
#################

states <- c("Alabama", "Alaska", "Arizona", "Arkansas",
            "California", "Colorado", "Connecticut",
            "Delaware", "Florida",
            "Georgia", "Hawaii", "Idaho", "Illinois",
            "Indiana", "Iowa", "Kansas", "Kentucky",
            "Louisiana", "Maine", "Maryland",
            "Massachusetts", "Michigan", "Minnesota",
            "Mississippi", "Missouri", "Montana",
            "Nebraska", "Nevada", "NewHampshire",
            "NewJersey", "NewMexico", "NewYork",
            "NorthCarolina", "NorthDakota", "Ohio",
            "Oklahoma", "Oregon", "Pennsylvania",
            "RhodeIsland", "SouthCarolina",
            "SouthDakota", "Tennessee", "Texas",
            "Utah", "Vermont", "Virginia", "Washington",
            "WestVirginia", "Wisconsin", "Wyoming")

## Convert list to lowercase
states <- unlist(lapply(states, tolower))

letters <- c("alpha", "beta", "gamma", "delta",
             "epsilon", "zeta", "eta", "theta",
             "iota", "kappa", "lambda", "mu",
             "nu", "xi", "omicron", "pi", "rho",
             "sigma", "tau", "upsilon", "phi",
             "chi", "psi", "omega")

############################
## MATCH FINDING FUNCTION ##
############################

## Define function to find matches and store in data farme
## for a single letter (we will use lapply over the list of
## letters on this later)
find_letter_in_states <- function(letter, states){
  ## letter: (string) one letter
  ## states: (list of strings) list of states
  
  ## Set up data frame to store matches
  maxdim <- length(states)
  matchset <- data.frame(letter=rep(NA,maxdim),
                          state=rep(NA,maxdim))
  
  ## Find indices in state list corresponding to
  ## state containing greek letter
  matched_states <- grep(letter, states, value=T)
  n <- length(matched_states)
  if (n>0){
    rows <- 1:n
    ## Save letter
    matchset$letter[rows] <- rep(letter,n)
    ## Save state(s)
    matchset$state[rows] <- matched_states
  }
  ## Remove extra NAs in matchset
  matchset <- matchset[complete.cases(matchset), ]
  
  ## Return match set (if there are any matches to return)
  if (nrow(matchset)>0){
    return(matchset)
  }
}

##################
## FIND MATCHES ##
##################

## Find matches (without a loop!!!)
matches <- lapply(letters, find_letter_in_states, states)

## Bind list of resulting dataframes together and print to console
print(do.call("rbind",matches))