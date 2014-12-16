## collection of functions

## outputs a data frame to a file
write.file <- function(input, file) {
  write.table(input, file = file, row.names = F, col.names = F, quote = F, 
              sep = ",")
}

## goes through a data frame and returns the NAs by column
missing.values <- function(x) {
    NA.values <- c()
    for (i in 1:ncol(x)) {
        NA.values[i] <- length(which(is.na(x[,i])))  
    }
    missing <- data.frame(Variables = names(x), 
                          Missing.Values = NA.values)
    return(missing)
}

## goes through a data frame and sums the zero values by column
sum.zero.values <- function(input) {
    zeroes <- c()
    for (i in 1:ncol(input)) {
        zeroes[i] <- length(which(input[, i] == "0"))
    }
    output <- data.frame(Meter = names(input), Zero.values = zeroes)
    return(output)
}

## batch converting column classes in a data frame
batch.class <- function(input, k, l) {
for (i in k:l) input[,i] <- as.numeric(input[,i])
}

## select every nth line from a data frame
Nth.line <- function(dataframe, n) dataframe[(seq(n, to=nrow(dataframe), by=n)),]