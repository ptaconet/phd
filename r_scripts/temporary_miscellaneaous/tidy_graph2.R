require(googlesheets4)
require(tidyverse)
require(tidygraph)
require(ggraph)
require(igraph)
require(visNetwork)

url <- "https://docs.google.com/spreadsheets/d/1s3VJ4M6BlQ6zkaVJiEE86n8hhYJfm4UBj9Gibn3YFQY/edit?usp=sharing"

level <- "name"

googlesheets4::sheets_deauth()
edges <- googlesheets4::read_sheet(url,sheet = "relations3")
nodes <- googlesheets4::read_sheet(url,sheet = "agents3") %>% 
  select(name,L1,L2,poids) %>% 
  filter(name %in% unique(c(edges$from,edges$to))) %>%
  mutate(poids=1)

if(level!="name"){
  edges <- edges %>%
    left_join(nodes, by = c("from"="name")) %>%
    dplyr::select(-from) %>%
    rename(from = !!level) %>%
    left_join(nodes, by = c("to"="name")) %>%
    dplyr::select(-to) %>%
    rename(to = !!level) %>%
    dplyr::select(from, to, sens) %>%
    distinct()
  
  nodes <- nodes %>%
    dplyr::select(!!level, L2,poids) %>%
    rename(name = !!level) %>%
    distinct()
  
  if(level=="L2"){ nodes$L2 <- nodes$name }
 
}
  
  


  # convert to tidygraph object
  graph <- tidygraph::tbl_graph(nodes=nodes,edges=edges)
  graph %>%
    ggraph(layout = "kk") +
    geom_edge_diagonal(color = "gray", alpha = 0.4, arrow = arrow(length = unit(3, 'mm'))) +
    geom_node_point(aes(color = L2, size = poids)) +
    geom_node_text(aes(label = name, color = L2), size = 3, repel = TRUE) +
    theme_graph()  +
    labs(title = "Agressivité des vecteurs",
         subtitle = "Modèle conceptuel")



## shortest path between two nodes
from <- which(nodes == "Intensité lumineuse (atm.)")
to <-  which(nodes == "Dispersion (vec.aé.)")

subgraph <- graph %>%
  morph(to_shortest_path, from, to)


## given 1 node, all nodes either i) influenced by this node, ii) that influence this node, iii) that is in contact with this node (either influence or being influenced by)
what_node <- "Agressivité (vec.aé.)"
#nod <- which(nodes == which_node)
mod <- "out"  # out = nodes influenced by this node , in = nodes that this node influence, all = that is in contact with this node (either influence or being influenced by)
ord <- 10
  

nodes$poids[which(nodes$name==what_node)] <- 2


subgraph <- tbl_graph(nodes=nodes,edges=edges) %>% 
  morph(to_local_neighborhood, which(nodes == what_node), order = ord, mode = mod)




subgraph <- subgraph %>%
  mutate(selected_node = TRUE) %>%
  activate(edges) %>%
  mutate(selected_edge = TRUE) %>%
  unmorph() %>%
  activate(nodes) %>%
  mutate(selected_node = ifelse(is.na(selected_node), 1, 2)) %>%
  activate(edges) %>%
  mutate(selected_edge = ifelse(is.na(selected_edge), 1, 2)) %>%
  arrange(selected_edge)

# plot
subgraph %>%
  ggraph(layout = "kk") +
  geom_edge_diagonal(aes(alpha = selected_edge), color = "gray", arrow = arrow(length = unit(3, 'mm'))) +
  geom_node_point(aes(color = L2, alpha = selected_node, size = poids)) +
  geom_node_text(aes(label = name, color = L2, alpha = selected_node), size = 3, repel = TRUE) +
  theme_graph()




## what happens to the system if we change the status of one given node ? 
#from <- which(nodes == "Température air")
#change <- "increase" # one of : cut, increase, decrease 
#change_strenght <- "small" # one of : small, big
## all the nodes directly or indirectly linked to "Température air" will be impacted









## some tests
#a <- graph %>%
#  morph(to_split, L1) %>%
#  crystallise()
#a$graph[1][[1]] %>%
#  ggraph(layout = "kk") +
#  geom_edge_diagonal(color = "gray", alpha = 0.4, arrow = arrow(length = unit(3, 'mm'))) +
#  geom_node_text(aes(label = name, color = L2), size = 3)
