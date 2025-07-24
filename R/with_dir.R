with_dir <- function (new, code) 
{
    old <- setwd(dir = new)
    on.exit(setwd(old))
    force(code)
}
