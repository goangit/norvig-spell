dict <- 'big.txt'

if (!file.exists(dict))
    download.file(file.path('http://norvig.com',dict), dict)

## !! text handling between R and python yields different results!!
##
## python: len(words(file('big.txt').read())) ## 1105285
## R: text   <- tolower(scan('big.txt', what=character()))
## length(text) ## 900834
##
## words  <- unlist(regmatches(text, gregexpr('[a-z]+', text)))
## length(unique(words))
## nwords <- table(words)

## use the python generated tokens until time to debug
d <- read.csv('python.nwords.csv', header=FALSE)
nwords <- as.table(d$V2); names(nwords) <- d$V1

edits1 <- function(word) {
    n          <- nchar(word)
    splits     <- lapply(seq(n+1), function(i) c(substr(word,1,i-1), substr(word,i,n)))
    a          <- sapply(splits,'[',1); b <- sapply(splits,'[',2)
    deletes    <- paste(a, substr(b,2,n), sep='')[1:n]
    transposes <- paste(a,substr(b,2,2),substr(b,1,1),substr(b,3,n),sep='')[1:(n-1)]
    ## replaces   <- paste(a[1:n],rep(letters,n),substr(b[1:n],2,n),sep='')
    replaces   <- sapply(seq(n),   function(i) paste(a[i],letters,substr(b[i],2,n),sep=''))
    inserts    <- sapply(seq(n+1), function(i) paste(a[i],letters,b[i],sep=''))
    unique(unlist(c(deletes,transposes,replaces,inserts)))
}

## ## checks against Norvig's numbers:
## length(edits1('something')) ## 494
## edits2 <- function(word) unique(unlist(lapply(edits1(word), edits1)))
## length(edits2('something')) ## 114324

known <- function(words) { words[ words %in% names(words) ]

known_edits2 <- function(word) known(unique(unlist(lapply(edits1(word), edits1))))

select.candidates <- function(word) {
    a <- known(word)
    if (length(a)>0) return(a)
    a <- edits1(word)
    b <- known(a)
    if (length(b)>0) return(b)
    c <- known(unlist(lapply(a,edits1)))
    if (length(c)>0) return(c)
    word
}

correct <- function(word) {
    candidates <- select.candidates(word)
    validated  <- nwords[candidates]
    ifelse(all(is.na(validated)), word, names(which.max(validated)))
}

