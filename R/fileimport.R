
topic <- "ProcurementDiagnostics"

sheets <- c("InfoExchange", "Budgets", "Advice", "PolicyDocs",
            "Logistics", "ProductReqs", "Regulatory", "Reports",
            "WorkTogether")

filenames <- paste0(topic, "_", sheets)

for(i in 1:9){
  mt <- as.matrix(read.csv(
    paste0("C:/Users/ebeylerian/Dropbox/ADP_Social Network Assessment Data_2016Feb16/CSV Files/",
           filenames[i],".csv"), header = TRUE, row.names = 1, skip = 2))
  assign(filenames[i], network(mt, directed = FALSE, matrix.type = "adjacency", multiple = FALSE))

  assign(paste0("el_", sheets[i]), as.edgelist(get(filenames[i])))
}

for(j in 2:8){
  el_compare <- get(paste0("el_", sheets[j]))
  attrval <- 1*tail(
    duplicated(as.data.frame(rbind(el_compare, el_WorkTogether))),
    n = nrow(el_WorkTogether))

  set.edge.attribute(ProcurementDiagnostics_WorkTogether, attrname = sheets[j],
                     value = attrval)
}

mt <- as.matrix(read.csv(
  paste0("C:/Users/ebeylerian/Dropbox/ADP_Social Network Assessment Data_2016Feb16/CSV Files/",
         filenames[9],".csv"), header = TRUE, row.names = 1, skip = 2))
set.edge.value(ProcurementDiagnostics_WorkTogether, attrname = "WorkTogether",
               value = mt)
#check
get.edge.value(ProcurementDiagnostics_WorkTogether, "WorkTogether")


###ATTRIBUTES
ProcurementDiagnostics_Attributes <- read.csv(
  paste0("C:/Users/ebeylerian/Dropbox/ADP_Social Network Assessment Data_2016Feb16/CSV Files/",
         "ProcurementDiagnostics_Attributes.csv"), header = TRUE, row.names = NULL)
classattr <- c("MOHSW", "Other Government of Tanzania Divisions", "Regional Health Facilities",
               "District Health Facilities", "NGO or FBO", "International Bilaterals",
               "Other", "Primary Health Facility", "Private Health Facility")
classattr <- classattr[ProcurementDiagnostics_Attributes$Classification]
lvlattr <- c("national", "regional", "district", "local", "international")
lvlattr <- lvlattr[ProcurementDiagnostics_Attributes$Level]

set.vertex.attribute(ProcurementDiagnostics_WorkTogether, attrname = "Level",
                     value = lvlattr)
set.vertex.attribute(ProcurementDiagnostics_WorkTogether, attrname = "Classification",
                     value = classattr)

setwd("C:/Users/ebeylerian/Documents/RProjects/statnetWeb-ADP/inst/shiny")
save(ProcurementDiagnostics_WorkTogether, file = "ProcurementDiagnostics_WorkTogether")

## QUANTIFICATION nws (edgelists)

QuantificationDiagnostics <- read.csv(
  paste0("C:/Users/ebeylerian/Dropbox/ADP_Social Network Assessment Data_2016Feb16/CSV Files/",
         "QuantificationDiagnostics.csv"), header = TRUE, row.names = NULL,
  stringsAsFactors = FALSE, strip.white = TRUE)

sheets <- c("InfoExchange", "ProductReqs", "Budgets", "Regulatory", "PolicyDocs",
            "Advice", "Reports", "Logistics", "Collaboration.score")

allnodes <- unique(c(QuantificationDiagnostics$From.org, QuantificationDiagnostics$To.org))
from.pos <- match(QuantificationDiagnostics$From.org, table = allnodes)
to.pos <- match(QuantificationDiagnostics$To.org, table = allnodes)
# QuantificationDiagnostics$To.org[QuantificationDiagnostics$To.org == "Procurement Supplies Unit (PSU)"] <- "Procurement Supplies Unit"
# QuantificationDiagnostics$To.org[QuantificationDiagnostics$To.org == "Pharmaceutical Service Unit (PSU)"] <- "PSU"
# QuantificationDiagnostics$To.org[QuantificationDiagnostics$To.org == "NTD Control Program"] <- "NTDCP"
# QuantificationDiagnostics$To.org[QuantificationDiagnostics$To.org == "TB Control Program"] <-

QuantificationDiagnostics_InfoExchange <- network.initialize(n = length(allnodes), directed = FALSE)
network.vertex.names(QuantificationDiagnostics_InfoExchange) <- allnodes
elist_info <- cbind(from.pos, to.pos)[which(QuantificationDiagnostics$InfoExchange == 1),]
evalmat_info <- QuantificationDiagnostics[sheets][which(QuantificationDiagnostics$InfoExchange == 1),]
evaldf_info <- split(evalmat_info, f = 1:NROW(evalmat_info))

sl <- as.list(sheets)
enl <- list()
enl <- lapply(1:nrow(elist_info), FUN = function(x){
  enl[x] <- sl
  })


add.edges(x = QuantificationDiagnostics_InfoExchange,
          tail = as.list(elist_info[,1]), head = as.list(elist_info[,2]),
          names.eval = enl,
          vals.eval = evaldf_info)

QuantificationDiagnostics_WorkTogether <- network.initialize(n = length(allnodes), directed = FALSE)
network.vertex.names(QuantificationDiagnostics_WorkTogether) <- allnodes
elist_work <- cbind(from.pos, to.pos)[which(QuantificationDiagnostics$WorkTogether.Binary == 1),]
evalmat_work <- QuantificationDiagnostics[sheets][which(QuantificationDiagnostics$WorkTogether.Binary == 1),]
evaldf_work <- split(evalmat_work, f = 1:NROW(evalmat_work))

enl <- list()
enl <- lapply(1:nrow(elist_work), FUN = function(x){
  enl[x] <- sl
})


add.edges(x = QuantificationDiagnostics_WorkTogether,
          tail = as.list(elist_work[,1]), head = as.list(elist_work[,2]),
          names.eval = enl,
          vals.eval = evaldf_work)

###ATTRIBUTES
QuantificationDiagnostics_Attributes <- read.csv(
  paste0("C:/Users/ebeylerian/Dropbox/ADP_Social Network Assessment Data_2016Feb16/CSV Files/",
         "QuantificationDiagnostics_Attributes.csv"), header = TRUE, row.names = NULL)
classattr <- c("MOHSW", "Other Government of Tanzania Divisions", "Regional Health Facilities",
               "District Health Facilities", "NGO or FBO", "International Bilaterals",
               "Other", "Primary Health Facility", "Private Health Facility")
classattr <- classattr[QuantificationDiagnostics_Attributes$Classification]
lvlattr <- c("national", "regional", "district", "local", "international")
lvlattr <- lvlattr[QuantificationDiagnostics_Attributes$Level]

set.vertex.attribute(QuantificationDiagnostics_InfoExchange, attrname = "Level",
                     value = lvlattr)
set.vertex.attribute(QuantificationDiagnostics_InfoExchange, attrname = "Classification",
                     value = classattr)

