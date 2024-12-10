
deps_table <- function(pkg, stdlib = FALSE, self = FALSE, lib_loc = NULL){
    stopifnot(
        '`pkg` should be a character vector' = is.character(pkg),
        '`stdlib` should be TRUE or FALSE' = isTRUE(stdlib) || isFALSE(stdlib),
        '`self` should be TRUE or FALSE' = isTRUE(self) || isFALSE(self),
        '`lib_loc` should be NULL or a character vector' = is.null(lib_loc) || is.character(lib_loc)
    )
    installed <- installed.packages(lib_loc)
    missing <- setdiff(pkg, rownames(installed))
    if (length(missing) > 0){
        stop('unrecognized packages in `pkg`: ', paste(missing, collapse = ', '))
    }
    all_deps <- deps_set(pkg, installed)
    if (self){
        all_deps <- union(all_deps, pkg)
    }
    deps_list <- lapply(all_deps, deps_set, db = installed)
    names(deps_list) <- all_deps
    deps_free <- all_deps[lengths(deps_list) == 0]
    deps_seq <- deps_seq(deps_list, deps_free)
    deps_mat <- installed[match(deps_seq, rownames(installed)),]
    fields <- c(
        package      = 'Package',
        version      = 'Version',
        license      = 'License',
        depends      = 'Depends',
        imports      = 'Imports',
        linking_to   = 'LinkingTo',
        suggests     = 'Suggests',
        enhances     = 'Enhances'
    )
    deps_df <- as.data.frame(deps_mat, row.names = FALSE)
    if (!stdlib){
        deps_df <- deps_df[!deps_df$Priority %in% c('base', 'recommended'),]
    }
    setNames(deps_df[, fields], names(fields))
}
