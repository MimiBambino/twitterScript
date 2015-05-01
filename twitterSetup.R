# EnsurePackages(x) - Installs and loads a package, if necessary
EnsurePackage = function(x){
  x = as.character(x)
  if (!require(x, character.only=TRUE)) {
    install.packages(pkgs=x, repos="http://cran.r-project.org")
    require(x, character.only=TRUE)
  }
}

# PrepareTwitter() - Load packages for working with twitteR
PrepareTwitter = function() {
  EnsurePackage("bitops")
  EnsurePackage("RCurl")
  EnsurePackage("RJSONIO")
  EnsurePackage("twitteR")
  EnsurePackage("httr")
}

# TweetFrame() - Return a dataframe based on a search of Twitter
TweetFrame = function(searchTerm, maxTweets) {
  twtList = searchTwitter(searchTerm, n=maxTweets)
  
  # as.data.frame() coerces each list element into a row
  # lapply() applies this to all of the elements in twtList
  # rbind() takes all of the rows and puts them together
  # do.call() gives rbind() all the rows as individual elements
  
  twtDF = do.call("rbind", lapply(twtList, as.data.frame))
  
  return (twtDF[order(as.integer(twtDF$created)),])
}

# TwitterRate(string1, string2) - Return results of a Poisson test comparing the 
# frequency of tweets citing different hashtags
TwitterRate = function(string1, string2) {
  DF1 = TweetFrame(string1, 500)
  eventDelays1 = as.integer(diff(DF1$created))
  m = mean(eventDelays1)
  s = sum(eventDelays1 <= m)
  
  DF2 = TweetFrame(string2, 500)
  eventDelays2 = as.integer(diff(DF2$created))
  s2 = sum(eventDelays2 <= m)
  
  return(poisson.test(c(s, s2), c(500, 500)))
}

# ArrivalProbability() - Given a list of arrival times, calculates
# the dalays between them using lagged differences then computes
# a list of cumulative probabilities of arrival for the sequential
# list of time increments

# times - A sorted, ascending list of arrival times in POSIXct
# increment - the time increment for each new slot, e.g. 10 sec
# max - the highest time increment, e.g., 240 sec

# Returns - an ordered list of probabilities in a numeric vector
# suitable for plotting with plot()
ArrivalProbability = function(times, increment, max) {
  # Inintialize an empty vector
  plist = NULL
  
  # Probability is defined over the size of this sample of arrival times
  timeLen = length(times)
  
  # Check for input mistakes
  if (increment > max) { return(NULL) }
  for (i in seq(increment, max, by=increment)) {
    # diff() requires a sorted list of times
    # diff() calculates the delays between neighboring times
    # the logical test < i provides a list of TRUE and FALSE of
    # length = timeLen, then sum() counts the TRUE values.
    # Divide by timeLen to calculate a proportion
    plist = c(plist, (sum(as.integer(diff(times)) < i)) / timeLen)
  }
  return(plist)
}

# Like ArrivalProbability(), but works with an unsorted list of
# delay times
DelayProbability = function(delays, increment, max) {
  plist = NULL
  
  # Probability is defined over the size of this sample of arrival times
  delayLen = length(delays)
  
  # Check for input mistakes
  if (increment > max) { return(NULL) }
  
  for (i in seq(increment, max, by=increment)) {
    # create a list of booleans and then summ the TRUE values
    plist = c(plist, (sum(delays <= i)/delayLen))
  }
  return(plist)
}