# Test of linking beecs to R through c-shared library
# Change path to beecs dir
beecs_dir <- "/Users/martinovic/git/beecs"
# Get working directory (directory of R package)
work_dir <- getwd()
# Remove previous headers and compiled objects
system("rm src/*.so; rm src/*.o; rm src/*.h")
# system("go clean -cache;go clean -fuzzcache")
build_command <- paste0("cd ",
                        beecs_dir,
                        ";go build -o ", work_dir, "/src/libbeecs.so -buildmode=c-shared ./_examples/c_shlib")
system(build_command)
system("cd src; R CMD SHLIB -L. -lbeecs beecs.c;")

# Change working directory to /src folder
setwd(paste0(work_dir, "/src"))
# Load shared library executing beecs into R
dyn.load("beecs.so")
# Run beecs
rows <- 1

definition <- list(
  InitialPopulation =
    list(Count = 50000),
  ForagingPeriod =
    list(Years = matrix(c(rep(10, 30), rep(0,335)),
           nrow = rows)),
  Termination =
    list(MaxTicks = 800)
)

json <- jsonlite::toJSON(definition,
                         auto_unbox = TRUE,
                         pretty = TRUE)

.Call("gobeecs", json)
# Unload shared library executing beecs
dyn.unload("beecs.so")
#Change working directory back to original one
setwd(work_dir)
