library(OpasnetUtils)
library(tidyverse)
ovas <- character()
for(i in ls()) {
  if("ovariable" %in% class(get(i))) ovas <- c(ovas, i)
}

asse <- Ovariable(
  "asse",
  dependencies=data.frame(
    Name=ovas
  ),
  formula=function(...) {return(data.frame(Result=1))}
)

objects.latest("Op_en3861","makeGraph")

tst <- scrape.assessment(asse)

gr <- makeGraph(tst)
render_graph(gr)

