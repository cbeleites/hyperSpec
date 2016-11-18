# C. Beleites
# get all tests attached to objects 
tests <- eapply(env=getNamespace ("hyperSpec"), FUN=get.test, all.names=TRUE) 
tests <- tests [! sapply (tests, is.null)] 

for (t in seq_along (tests))   
  tests [[t]] ()

