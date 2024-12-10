
deps_set <- function(pkg, db, recursive = TRUE){
    unique(unlist(tools::package_dependencies(pkg, db, recursive = recursive), use.names = FALSE))
}
