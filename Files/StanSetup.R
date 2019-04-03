#####Latest Stan setup ----
remove.packages("rstan")
if (file.exists(".RData")) file.remove(".RData")

install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
pkgbuild::has_build_tools(debug = TRUE)

##The code below is optional. May result in faster runtimes, but may cause some problems. They also provide code to undo these changes if
##neccessary 

dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, ifelse(.Platform$OS.type == "windows", "Makevars.win", "Makevars"))
if (!file.exists(M)) file.create(M)
cat("\nCXX14FLAGS=-O3 -march=native -mtune=native",
    if( grepl("^darwin", R.version$os)) "CXX14FLAGS += -arch x86_64 -ftemplate-depth-256" else 
      if (.Platform$OS.type == "windows") "CXX11FLAGS=-O3 -march=native -mtune=native" else
        "CXX14FLAGS += -fPIC",
    file = M, sep = "\n", append = TRUE)

## If the above code results in errors, run the below code to fix it. I installed this on two computers. One of the 
## computers had problems and the other one didn't. 

M <- file.path(Sys.getenv("HOME"), ".R", ifelse(.Platform$OS.type == "windows", "Makevars.win", "Makevars"))
file.edit(M)

##The link below describes options to try to fix compiling errors. The option that worked for me was deleting the contents of the 
##makevars file. 
# https://discourse.mc-stan.org/t/stan-compiling-error/6486


####Installing R packages ----


install.packages("brms")
install.packages("mvtnorm")
install.packages("lme4")
install.packages("devtools")
install.packages("ggplot2")
install.packages("bayesplot")



