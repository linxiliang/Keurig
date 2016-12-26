# From 
# https://www.r-bloggers.com/running-r-jobs-quickly-on-many-machines/
require(parallel)
primary <- 'bushgcn39'
machineAddresses <- list(
  list(host=primary,user='xlin0',
       ncore=4),
  list(host='bushgcn38',user='xlin0',
       ncore=4)
)

spec <- lapply(machineAddresses,
               function(machine) {
                 rep(list(list(host=machine$host,
                               user=machine$user)),
                     machine$ncore)
               })
spec <- unlist(spec,recursive=FALSE)

cl <- parallel::makeCluster(type='PSOCK',
                                         master=primary,
                                         spec=spec)
print(cl)

func <- function(i) "Hello World!"
parLapply(cl, 1:20, func)