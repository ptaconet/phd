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
  filter(name %in% unique(c(edges$from,edges$to)))

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
from <- which(nodes == "Croissance (vec.aq.)")
to <-  which(nodes == "Agressivité (vec.aé.)")

shortest <- graph %>%
  morph(to_shortest_path, from, to) %>%
  mutate(selected_node = TRUE) %>%
  activate(edges) %>%
  mutate(selected_edge = TRUE) %>%
  unmorph() %>%
  activate(nodes) %>%
  mutate(selected_node = ifelse(is.na(selected_node), 1, 2)) %>%
  activate(edges) %>%
  mutate(selected_edge = ifelse(is.na(selected_edge), 1, 2)) %>%
  arrange(selected_edge)

shortest %>%
  ggraph(layout = "kk") +
  geom_edge_diagonal(aes(alpha = selected_edge), color = "gray", arrow = arrow(length = unit(3, 'mm'))) +
  geom_node_point(aes(color = L2, alpha = selected_node), size = 3) +
  geom_node_text(aes(label = name, color = L2, alpha = selected_node), size = 3, repel = TRUE) +
  theme_graph()
  
  
## all nodes influenced by 1 given node
from <- which(nodes == "Température air")

## all nodes that influence 1 given node
to <- which(nodes == "Température air")

## 














## some tests
a <- graph %>%
  morph(to_split, L1) %>%
  crystallise()
a$graph[1][[1]] %>%
  ggraph(layout = "kk") +
  geom_edge_diagonal(color = "gray", alpha = 0.4, arrow = arrow(length = unit(3, 'mm'))) +
  geom_node_text(aes(label = name, color = L2), size = 3)
