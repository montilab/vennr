#' Python-style string formatting
#' @keywords internal
format.str <- function(string, ...) {
    args <- list(...)
    for (i in 1:length(args)) {
        pattern <- paste("\\{", i, "}", sep="")
        replacement <- args[[i]]
        string <- gsub(pattern, replacement, string)
    }
    return(string)
}

#' Cloned from gtools
#' @keywords internal
combinations <- function(n, r, v = 1:n, set = TRUE, repeats.allowed=FALSE)
{
    if(mode(n) != "numeric" || length(n) != 1 
       || n < 1 || (n %% 1) != 0) stop("bad value of n") 
    if(mode(r) != "numeric" || length(r) != 1 
       || r < 1 || (r %% 1) != 0) stop("bad value of r") 
    if(!is.atomic(v) || length(v) < n) 
        stop("v is either non-atomic or too short")
    if( (r > n) & repeats.allowed==FALSE)
        stop("r > n and repeats.allowed=FALSE")
    if(set) {
        v <- unique(sort(v))
        if (length(v) < n) stop("too few different elements")
    }
    v0 <- vector(mode(v), 0)
    ## Inner workhorse
    if(repeats.allowed)
        sub <- function(n, r, v)
        { 
            if(r == 0) v0 else
                if(r == 1) matrix(v, n, 1) else
                    if(n == 1) matrix(v, 1, r) else
                        rbind( cbind(v[1], Recall(n, r-1, v)),
                               Recall(n-1, r, v[-1]))
        }
    else
        sub <- function(n, r, v)
        { 
            if(r == 0) v0 else
                if(r == 1) matrix(v, n, 1) else
                    if(r == n) matrix(v, 1, n) else
                        rbind(cbind(v[1], Recall(n-1, r-1, v[-1])),
                              Recall(n-1, r, v[-1]))
        }
    sub(n, r, v[1:n])
}

#' Remove one element from a named list
#' @keywords internal
list.rm <- function(x, rm) x[names(x)[names(x) != rm]]

#' Flatten a list of character vectors
#' @keywords internal
list.flatten <- function(x) unlist(x, use.names=FALSE)

#' Find set difference between one and multiple lists
#' @keywords internal
list.setdiff <- function(x, y) length(setdiff(list.flatten(x), list.flatten(y)))

#' Find intersection of two or more lists
#' @keywords internal
list.intersect <- function(x) length(Reduce(intersect, x))

#' Intersect a name list of character vectors for venn.js
#' @keywords internal
intersect.sets <- function(sets, labels=TRUE, iter=1, json=list()) {

    sets <- lapply(sets, unique)
    
    # Setdiffs for each set
    for (i in names(sets)) {
        size <- length(sets[[i]])
        exclusive <- list.setdiff(sets[i], list.rm(sets, i))
        if (exclusive == 0 | !labels) label <- i else label <- format.str("{1} ({2})", i, exclusive)
        json[[paste(iter)]] <- list(sets=as.list(i), size=size, label=label)
        iter <- iter+1
    }
    
    # Intersections for each unique combination
    groups <- lapply(2:length(sets), function(x) {
        combinations(length(sets), x, names(sets), repeats.allowed=FALSE)
    })
    for (i in groups) {
        for (j in seq(nrow(i))) {
            k <- i[j,]
            size <- list.intersect(sets[k])
            label <- ifelse(labels, paste(size), "")
            json[[paste(iter)]] <- list(sets=as.list(k), size=size, label=label)
            iter <- iter+1
        }
    }
    
    names(json) <- NULL
    return(json)
}

#' Area proportional venn and euler diagrams through venn.js
#' 
#' @param sets A named list of character vectors or raw input to venn.js
#' @param raw Use true if sets is raw input to venn.js
#' @param labels Use true to label overlap regions
#' @param font.size Font size for labels
#' @param font.family Font family for labels
#' @param width Width of plot (e.g "800px")
#' @param height Height of plot (e.g "600px") 
#' 
#' @import htmlwidgets
#' @export
vennr <- function(sets, 
                  raw=FALSE,
                  labels=TRUE,
                  font.size="14px",
                  font.family="Helvetica,Arial,sans-serif",
                  width=NULL,
                  height=NULL) {

    if (raw) {
        data <- htmlwidgets:::toJSON(sets)
    } else {
        data <- htmlwidgets:::toJSON(intersect.sets(sets, labels))
    }

    settings <- list(fontSize=font.size,
                     fontFamily=font.family)

    htmlwidgets::createWidget(
        name="vennr", 
        x=list(data=data, settings=settings),
        width=width,
        height=height,
        sizingPolicy = htmlwidgets::sizingPolicy(
            viewer.fill=TRUE,
            browser.fill=TRUE
        )    
    )
}

#' Shiny output
#' @export
vennrOutput <- function(outputId, width = "100%", height = "400px") {
  shinyWidgetOutput(outputId, "vennr", width, height, package = "vennr")
}

#' Shiny render
#' @export
vennrRender <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, vennrOutput, env, quoted = TRUE)
}
