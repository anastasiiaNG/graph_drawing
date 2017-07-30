
setwd("~/R-studio/gatom")
library(gatom)
library(data.table)
library(igraph)
library(ForceAtlas2)
library(ggnet)
library(network)
library(sna)
library(ggplot2)
library(ggrepel)
library(grid)
library(Rcpp)


data("networkEx")
data("org.Mm.eg.gatom.annoEx")
data("met.kegg.dbEx")

data("met.de.rawEx")
data("gene.de.rawEx")

g <- makeAtomGraph(network=networkEx, 
                   org.gatom.anno=org.Mm.eg.gatom.annoEx, 
                   gene.de=gene.de.rawEx,
                   met.db=met.kegg.dbEx, 
                   met.de=met.de.rawEx)
print(g)

gs <- scoreGraph(g, k.gene = 25, k.met=25)

set.seed(42)
m <- solveSgmwcsRandHeur(gs, max.iterations = 2000)


options(stringsAsFactors = FALSE)
produce_node_attrs <- gatom:::getDotNodeStyleAttributes(as_data_frame(m, what = "vertices"))
produce_edge_attrs <- gatom:::getDotEdgeStyleAttributes(as_data_frame(m))


produce_node_attrs$color
produce_node_attrs$width
produce_node_attrs$fontsize

produce_edge_attrs$color
produce_edge_attrs$penwidth
produce_edge_attrs$fontsize



print(m)
tail(ggnetwork(m))
E(m)$label
V(m)$label


saveModuleToDot(m, file="M0.vs.M1.dot", name="M0.vs.M1")
system("neato -Tsvg M0.vs.M1.dot > M0.vs.M1.svg", ignore.stderr = T)
knitr::include_graphics("M0.vs.M1.svg")

saveModuleToXgmml(m, file="M0.vs.M1.xgmml", name="M0.vs.M1")

m.ext <- addHighlyExpressedEdges(m, gs)
saveModuleToDot(m.ext, file="M0.vs.M1.ext.dot", name="M0.vs.M1")
system("neato -Tsvg M0.vs.M1.ext.dot > M0.vs.M1.ext.svg", ignore.stderr = T)
knitr::include_graphics("M0.vs.M1.ext.svg")

m1 <- connectAtomsInsideMetabolite(m.ext)
saveModuleToDot(m1, file="M0.vs.M1.v1.dot", name="M0.vs.M1")
system("neato -Tsvg M0.vs.M1.v1.dot > M0.vs.M1.v1.svg", ignore.stderr = T)
knitr::include_graphics("M0.vs.M1.v1.svg")



load(url("http://artyomovlab.wustl.edu/publications/supp_materials/GATOM/network.rda"))
load(url("http://artyomovlab.wustl.edu/publications/supp_materials/GATOM/org.Mm.eg.gatom.anno.rda"))
load(url("http://artyomovlab.wustl.edu/publications/supp_materials/GATOM/org.Mm.eg.gatom.anno.rda"))
load(url("http://artyomovlab.wustl.edu/publications/supp_materials/GATOM/met.kegg.db.rda"))

library(readr)
met.de.raw <- read_tsv("http://artyomovlab.wustl.edu/publications/supp_materials/GAM/Ctrl.vs.MandLPSandIFNg.met.de.tsv.gz")

gene.de.raw <- read_tsv("http://artyomovlab.wustl.edu/publications/supp_materials/GAM/Ctrl.vs.MandLPSandIFNg.gene.de.tsv.gz")# install.packages("devtools")
###_____________________________________________________________

### 57
print(layout1)
V(m)
class(V(m))

###_________________________Graphviz____________________________
library(DiagrammeR)
library(gridGraphviz)
library(Rgraphviz)
browseVignettes("Rgraphviz")
set.seed(123)
V <- letters[1:10]
M <- 1:4
g1 <- randomGraph(V, M, 0.2)
plot(g1)
# read.dot(file)
