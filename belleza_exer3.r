#Author: Trixia Belleza 
#Section: B-5L CMSC 150
#input equations
E1 <- function (x1, x2, x3) 25 * x1 + 5 * x2 + 1 * x3 + -106.8;
E2 <- function (x1, x2, x3) 64 * x1 + 1 * x3 + 8 * x2 + -177.2;
E3 <- function (x1, x2, x3) 144 * x1 + 12 * x2 + 1 * x3 + -279.2;

#put them in a list
system <- list(E1, E2, E3);


#gets the vector of rownames by number of rows.
#returns -> c(1, 2, 3, ..., n)
getRowNames <- function(system) {
  rownames <- c()
  for (i in 1:length(system)) {
    rownames <- c(rownames, i)      
  }
  return(rownames)
}

#gets the vector of colnames by number of cols then appending an "x" with it.
#returns -> c("x1", "x2", "x3", ..., "RHS")
getColNames <- function(vars) {
  colnames <- c()
  for (i in 1:length(vars)) {
    #concat x and number i
    colname = paste("x",i, sep="")
    colnames <- c(colnames, colname)  
  }
  colnames <- c(colnames, "RHS") #adds RHS at the last index of the vector
  return(colnames)
}

#creates a list of terms per equation
listOfTerms <- function(system) {
  terms_list = c()
  for (i in 1:length(system)) {
    eq = deparse(system[i])[2];    #returns "0.3 * x1 + -0.2 * x2 + 10 * x3 + -71.4)"
    eq = substr(eq,1,nchar(eq)-1)  #remove unnecessary parenthesis at the last character.
                                    #returns "0.3 * x1 + -0.2 * x2 + 10 * x3 + -71.4"
    
    eq_split = strsplit(eq, " \\+ ")  #split by " + "
    terms_list <- c(terms_list, eq_split)   #returns "0.3 * x1"  "-0.2 * x2" "10 * x3"   "-71.4"
  }
  return(terms_list);
}

AugCoeffMatrix <- function(system){
  count=0 #keeps track kung pang-ilang row yung paglalagyan ng coefficient sa matrix
  unknown_count = 0
  shouldBeNA=0
  
  #get var count for each equation
  for(eq in 1:length(system)) {
    funcDef = deparse(system[[eq]])[1]  #from E1 <- function (x1, x2, x3) 0.3 * x1 + -0.2 * x2 + 10 * x3 + -71.4; 
                                                    #it returns "function (x1, x2, x3) "
    funcParams = substr(funcDef, 11, nchar(funcDef)-2) #from "function (x1, x2, x3) " it returns "x1, x2, x3"
    vars = strsplit(funcParams, "\\, ")  #split by comma + space 
                                        #from "x1, x2, x3" it returns "x1" "x2" "x3"
    #checks if unknown counts are all the same. Else, return NA
    if( (length(vars[[1]]) != unknown_count) & (unknown_count == 0) ) {
      unknown_count = length(vars[[1]])
    }
    #else if num of var for each equations != unknowns of all equation OR length of equations != num of unknowns
    else if((length(vars[[1]]) != unknown_count) || (length(system) != length(vars[[1]]))) { 
      shouldBeNA = 1
      break
    }
  }
  
  if(shouldBeNA == 0) {
    #declare matrix
    augcoeffmatrix = matrix(data=NA, nrow=length(system), ncol=length(vars[[1]])+1, dimnames=list(getRowNames(system), getColNames(vars[[1]])))
  
    for(j in 1:length(listOfTerms(system))){
      sorted_terms <- c()
      term = listOfTerms(system)[[j]]
  
      for(i in 1:length(term)) {
        #checks if there's an "x" in the term
        if(grepl("x", term[i])) {
          
          index_ng_paglalagyan = as.numeric(substring(term[i], nchar(term[i]))) #let's say terms[i] = 0.3 * x1 
                                                                                #gets the number after x, thus, from this example, it returns 1.
          sorted_terms[index_ng_paglalagyan] = term[i];
        }
        #if there's no "x", it's a b coefficient!
        else{ 
          bCoeff = term[i]
        }
      }
      sorted_terms <- c(sorted_terms, as.numeric(bCoeff)*(-1))
      count = count + 1
    
      for(k in 1:length(sorted_terms)) {
        coeff = strsplit(sorted_terms[k], "\\*")[[1]][1] #Gets the coefficient by splitting the term by "*" then getting the first index of the split array
        augcoeffmatrix[count,k] = as.numeric(coeff)
      }
    }
    return(list(variables = vars[[1]], augcoeffmatrix = augcoeffmatrix))
  }
  else {
    return(NA)
  }
}
result = AugCoeffMatrix(system)
print(result)

#print("result$augcoeffmatrix")
#print(result$augcoeffmatrix)

#THANK GOD IT FINALLY WORKS (i think.........)
