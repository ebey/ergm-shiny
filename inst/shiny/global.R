
library(statnetWeb)

# version of textInput with more size options.
# specify class = 'input-small' or class='input-mini' in
# addition to other textInput args
customTextInput <- function(inputId, label, value = "",
                            labelstyle = "dispay:inline;", ...) {
  tagList(tags$label(label, `for` = inputId, style = labelstyle),
          tags$input(id = inputId, type = "text", value = value,
                     ...))
}

customNumericInput <- function(inputId, label, value = 0,
                               labelstyle = "display:inline;", ...) {
  tagList(tags$label(label, `for` = inputId, style = labelstyle),
          tags$input(id = inputId, type = "number", value = value,
                     ...))
}

# version of selectInput...shorter box and label
# inline lapply allows us to add each element of
# choices as an option in the select menu
inlineSelectInput <- function(inputId, label, choices, ...) {
  if(is.null(label)){
    labeldisp <- "display: none;"
  } else {
    labeldisp <- "display: inline;"
  }

  tagList(tags$label(label, `for` = inputId, style = labeldisp),
          tags$select(id = inputId, choices = choices, ...,
                      class = "shiny-bound-input inlineselect",
                      lapply(choices, tags$option)))
}

# create a list of unique term names
splitargs <- function(searchterm, nw){
  sink("NUL")
  allterms <- search.ergmTerms(keyword = searchterm, net = nw)
  sink()
  ind1 <- regexpr(pattern = "\\(", allterms)
  ind2 <- regexpr(pattern = "\\)", allterms)
  termnames <- substr(allterms, start = rep(1, length(allterms)), stop = ind1 - 1)
  termargs <- substr(allterms, start = ind1, stop = ind2)
  dups <- duplicated(termnames)
  termargs <- termargs[-which(dups)]
  termnames <- unique(termnames)
  list(names = termnames, args = termargs)
}



# disable widgets when they should not be usable
disableWidget <- function(id, session, disabled = TRUE) {
  if (disabled) {
    session$sendCustomMessage(type = "jsCode",
                              list(code = paste("$('#", id, "').prop('disabled',true)",
                                                sep = "")))
  } else {
    session$sendCustomMessage(type = "jsCode",
                              list(code = paste("$('#", id, "').prop('disabled',false)",
                                                sep = "")))
  }
}

# function to return tests on simulated graphs
cugstats <- function(x, term, directed, loops) {
  nw <- network(x, directed = directed, loops = loops)
  summary.statistics(as.formula(paste("nw ~ ", term)))
}


# Takes an ergm object and gathers some of the information from
# summary.ergm, in preparation to be passed to the function
# coef.comparison.
ergm.info <- function(object) {
  coefs <- object$coef
  terms <- names(object$coef)
  coefmatrix <- summary(object)$coefs
  pval <- coefmatrix[, 4]
  signif.stars <- symnum(pval, corr = FALSE, na = FALSE,
                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                         symbols = c("***", "**", "*", ".", " "), legend = F)
  starredcoef <- paste0(format(coefs, digits = 3), signif.stars)

  ans <- list()
  count <- 1
  for (i in terms) {
    ans[i] <- starredcoef[count]
    count <- count + 1
  }
  ans$AIC <- format(summary(object)$aic, digits = 3)
  ans$BIC <- format(summary(object)$bic, digits = 3)
  ans
}


# Takes in a list of multiple outputs from ergm.info,
# formatting it all into a table comparing up to 5 models.
coef.comparison <- function(coeflist) {
  nmod <- length(coeflist)
  if (nmod == 0)
    stop("No model summaries were passed to model.comparison")
  if (nmod > 5)
    stop("List of length greater than 5 passed to model.comparison")
  terms <- c()
  for (j in 1:nmod) {
    terms <- append(terms, names(coeflist[[j]]))
  }
  terms <- unique(terms)
  terms <- c(terms[-which(terms == "AIC")], "AIC")
  terms <- c(terms[-which(terms == "BIC")], "BIC")
  mat <- matrix(nrow = length(terms), ncol = nmod)

  for (k in 1:nmod) {
    row <- 1
    for (l in terms) {
      if (is.null(coeflist[[k]][[l]])) {
        mat[row, k] <- "NA"
      } else {
        mat[row, k] <- coeflist[[k]][[l]]
      }
      row <- row + 1
    }
  }

  rownames(mat) <- terms
  colnames(mat) <- paste0("Model", 1:nmod)
  return(print(mat, quote = FALSE))
}

stat.comparison <- function(statlist) {
  nmod <- length(statlist)
  if (nmod == 0)
    stop("No model summaries were passed to stat.comparison")
  if (nmod > 5)
    stop("List of length greater than 5 passed to stat.comparison")

  statvec <- c()
  for (j in 1:nmod) {
    for (k in 1:length(statlist[[j]])) {
      if (!(names(statlist[[j]][k]) %in% names(statvec))) {
        statvec <- append(statvec, statlist[[j]][k])
      }
    }
  }

  return(statvec)
}

attr.info <- function(df, colname, numattrs, breaks) {
  lvls <- length(unique(df[[colname]]))
  if(colname %in% numattrs & lvls > 9){
    tab <- hist(df[[colname]], breaks = breaks, plot = FALSE)
    barname <- paste(tab$breaks[1:2], collapse = "-")
    for(i in seq(length(tab$breaks) - 2)){
      barname <- append(barname, paste(tab$breaks[i+1]+1,
                                       tab$breaks[i+2], sep = "-"))
    }
    tab <- tab$counts
    names(tab) <- barname
  } else {
    tab <- table(df[[colname]])
  }
  return(tab)
}

glossary <- list()
glossary$vertex <- "A vertex (also known as a node or actor) is the basic element of a network. It can represent a person, country, organization, etc."
glossary$actor <- "An actor (also known as a node or vertex) is the basic element of a network. It can represent a person, country, organization, etc."
glossary$node <- "A node (also known as a vertex or actor) is the basic element of a network. It can represent a person, country, organization, etc."
glossary$edge <- "An edge (also known as a tie or a link) represents the relationship between two nodes. It can be directed (gets advice from) or undirected (is friends with) and weighted or binary."
glossary$tie <- "A tie (also known as an edge or a link) represents the relationship between two nodes. It can be directed (gets advice from) or undirected (is friends with) and weighted or binary."
glossary$link <- "An link (also known as an edge or a tie) represents the relationship between two nodes. It can be directed (gets advice from) or undirected (is friends with) and weighted or binary."
glossary$dyad <- "A dyad is a pair of nodes. In a network where the total number of nodes is n, the number of dyads in a directed network is (n-1)*n and in an undirected network is (1/2)*(n-1)*n"
glossary$degree <- "The degree of a node is equal to the number of edges it has. For directed networks this can be subset by in-degree (the number of edges that point to the node) and out-degree (the number of edges that point away from the node)."
glossary[["degree distribution"]] <- "The degree distribution is a network-level summary that shows how the edges in a network are distributed among the nodes. The degree distributions of directed graphs can be subset by in-degree or out-degree."
glossary$geodesic <- 'A geodesic is a dyad-level measure for the shortest possible path between a pair of nodes. If there is no path between a pair of nodes, the geodesic distance is said to be infinite, or "inf" for short.'
glossary[["geodesic distribution"]] <- "The geodesic distribution is a network-level summary of the geodesics between all possible pairs of nodes."
glossary[["adjacency matrix"]] <- ""
glossary[["incidence matrix"]] <- ""
glossary[["edge list"]] <- ""
glossary$bipartite <- ""