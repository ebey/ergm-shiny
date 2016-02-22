
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
glossary$vertex <- c("A vertex (also known as a node or actor) is the basic element of a network. It can represent a person, country, organization, etc.")
glossary$actor <- c("An actor (also known as a node or vertex) is the basic element of a network. It can represent a person, country, organization, etc.")
glossary$node <- c("A node (also known as a vertex or actor) is the basic element of a network. It can represent a person, country, organization, etc.")
glossary$edge <- c("An edge (also known as a tie or a link) represents the relationship between two nodes. It can be directed (gets advice from) or undirected (is friends with) and weighted or binary.")
glossary$tie <- c("A tie (also known as an edge or a link) represents the relationship between two nodes. It can be directed (gets advice from) or undirected (is friends with) and weighted or binary.")
glossary$link <- c("An link (also known as an edge or a tie) represents the relationship between two nodes. It can be directed (gets advice from) or undirected (is friends with) and weighted or binary.")
glossary$dyad <- c("A dyad is a pair of nodes. In a network where the total number of nodes is n, the number of dyads in a directed network is (n-1)*n and in an undirected network is (1/2)*(n-1)*n")
glossary$degree <- c("The degree of a node is equal to the number of edges it has. For directed networks this can be subset by in-degree (the number of edges that point to the node) and out-degree (the number of edges that point away from the node).")
glossary[["degree distribution"]] <- c("The degree distribution is a network-level summary that shows how the edges in a network are distributed among the nodes. The degree distributions of directed graphs can be subset by in-degree or out-degree.")
glossary$geodesic <- c('A geodesic is a dyad-level measure for the shortest possible path between a pair of nodes. If there is no path between a pair of nodes, the geodesic distance is said to be infinite, or "inf" for short.')
glossary[["geodesic distribution"]] <- c("The geodesic distribution is a network-level summary of the geodesics between all possible pairs of nodes.")
glossary[["adjacency matrix"]] <- c("One method for representing network data is with an adjacency matrix. These are square matrices with one row for each node and one column for each node. The contents of each cell depend on whether there is an edge between the nodes in the correspond row and column.",
                                    "For directed networks, an entry in row i and column j represents an edge originating from node i and terminating at node j. Matrices for undirected networks are symmetric around the diagonal (The entry in row i, column j equals the entry in row j, column i).")
glossary[["incidence matrix"]] <- c("A matrix where the rows correspond to nodes and the columns correspond to edges.",
                                    "For directed networks, the element in row i, column j is -1 if edge j leaves node i, 1 if edge j enters node i, and 0 otherwise.",
                                    "For undirected networks, the element in row i, column j is 1 if edge j is incident on node i, and 0 otherwise.")
glossary[["edge list"]] <- c("A two column matrix that lists the origin and termination nodes of every edge.",
                             "This format does not include isolates.")
glossary[["bipartite network"]] <- c("A bipartite network is one with two different types of nodes, where it is impossible for edges to exist between nodes of the same type.",
                                     "This is sometimes called an affiliation network when the two types of nodes are people and organizations, and the relationship of interest is organization membership.")
glossary$missing <- "A piece of data is missing if we don't know what value it takes. A missing edge is different than a null edge (one that we know does not exist)."
glossary$isolate <- "An isolate is a node that is not connected to any other nodes."
glossary$homophily <- "The tendency for nodes to form ties with others that have similar characteristics. For example, friendship networks among students might display homophily within age group."
glossary$transitivity <- "The tendency to close triangles when two edges already exist. For example, the notion that the friend of my friend is my friend."
glossary$hub <- "A hub is a node with many links"
glossary[["empty network"]] <- ""
glossary[["complete network"]] <- ""
glossary$adjacent <- ""
glossary$incident <- ""
glossary$neighbor <- ""
glossary$betweenness <- ""
glossary$directed <- ""
glossary$undirected <- ""
glossary$attribute <- ""
glossary$triangle <- ""


