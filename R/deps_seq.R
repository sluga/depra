
deps_seq <- function(l, seq = character(0)){
    top <- vapply(l, function(x) all(x %in% seq), logical(1))
    if (all(top)){
        return(union(seq, names(top)))
    }
    deps_seq(l[!top], union(seq, names(l[top])))
}
