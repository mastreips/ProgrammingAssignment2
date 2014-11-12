makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

crazy <- function() {                # create a new environment with a local variable 'x' and access to another variable 'x'
        # declared somewhere outside this function
        x <- 3.14                                # assign the numeric value 3.14 to local variable 'x'
        print(x)                                   # output the current value of local variable 'x' (1)
        
{ print(x);                                 # output the current value of local variable 'x' (2)
  x <<- 42;                              # assign the numeric value 42 to variable 'x' declared outside this function (3)
  print(x)                                 # output the current value of local variable 'x' (4)
        }

print(x)                                   # output the current value of local variable 'x' (5)
}
