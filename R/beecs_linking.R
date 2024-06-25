# Test of linking beecs to R through c-shared library
# Change path to beecs dir
beecs_dir <- "/Users/martinovic/git/beecs"
# Get working directory (directory of R package)
work_dir <- get_wd()
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
.Call("gobeecs", "{test: [seems to be working]}")
# Unload shared library executing beecs
dyn.unload("src/beecs.so")
#Change working directory back to original one
setwd(work_dir)
