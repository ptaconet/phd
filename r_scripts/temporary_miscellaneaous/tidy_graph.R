# Sytème/trait/indicateur/métrique "Aggressivité des vecteurs" (densité aggressive) composé de :
# Package 1 : "Emergeance et développement des vecteurs"
# Package 2 : "Comportement de piqure des vecteurs"

# Sytème/trait/indicateur/métrique "Résistance des vecteurs aggressifs" composé de :
# Package 1 : "Emergeance et développement des résistances physiologiques"
# Package 2 : "Emergeance et développement des résistances comportementales"



# Ce qu'on cherche à visualiser :
# - relations entre les agents/classes (diagramme de classes)
# - états possibles des agents et transitions entre les différents états (diagramme d'état-transition)
#

# Diagramme d'état-transition (UML) pour l'agent "Vecteur" (classe vecteur)
# Diagramme d'état-transition (UML) pour l'agent "Hôte" (classe hôte)
# Diagramme d'état-transition (UML) pour l'agent "Parasite" (classe parasite)
# Graphe pour le système "transmission du paludisme" {vecteur, hôte, parasite}
# Graphe montrant les interactions entre les l'environnement et le système "transmission du paludisme"


require(googlesheets4)
require(tidyverse)
require(tidygraph)
require(ggraph)
require(igraph)
require(visNetwork)

# tutos:
# visNetwork : https://www.rpubs.com/Steven_Surya/visNetwork
# tidygraph : https://www.rpubs.com/Steven_Surya/tidygraph-ggraph , http://perso.ens-lyon.fr/lise.vaudor/des-graphes-bien-jolis/

# url to table
url <- "https://docs.google.com/spreadsheets/d/1g7dIWXhFDtpaCPHlq7WRRM8fgVoqM7mhDDfSf9NWFO8/edit?usp=sharing"

# table relation with nodes and edges
edges <- googlesheets4::read_sheet(url,sheet = "relations")
edges_prop <- googlesheets4::read_sheet(url,sheet = "relations_proprietes")
nodes <-  googlesheets4::read_sheet(url,sheet = "agents") %>% select(name,L4) %>% filter(name %in% unique(c(edges$from,edges$to)))

# joindre la hiérarchie et les propriétés des relations :
edges_relations <- edges %>%
  dplyr::left_join(nodes,by = c("from"="name")) %>%
  rename(from_L4=L4) %>%
  dplyr::left_join(nodes,by = c("to"="name")) %>%
  rename(to_L4=L4) %>%
  dplyr::left_join(edges_prop) %>%
  dplyr::mutate(phrase = paste("Le/La",trait_from,"de/d'",from,propriete,"le/La",trait_to,"de/d'",to))

# si on veut hiérarchie 1 pour from et to :
edges_relations <- edges_relations %>%
  dplyr::select(-c(from,to)) %>%
  dplyr::rename(from=from_L4,to=to_L4) %>%
  dplyr::group_by(from,to) %>%
  dplyr::summarise(count=n()) %>%
  as.data.frame()
nodes <- nodes %>%
  dplyr::select(L4) %>%
  dplyr::rename(name=L4) %>%
  dplyr::distinct()

# convert to tidygraph object
graph=tidygraph::tbl_graph(nodes=nodes,edges=edges_relations)
plot(graph)

g=graph %>%
  ggraph(layout="auto")

#g1=g + geom_node_point(color="goldenrod",size=3)
#g2=g + geom_node_label(aes(label=name),fill="steelblue",alpha=0.2)
#g3=g + geom_node_text(aes(label=name), color="olivedrab")
#g4=g + geom_node_circle(aes(fill=groupe , r=0.3))
#library(patchwork)
#(g1+g2)/(g3+g4)
#(g1+g2)/(g3)


gf=g +
  geom_edge_link(alpha=0.3, edge_width=2)+
  geom_node_label(aes(label=name), alpha=0.3, label.size=0)+
  ggplot2::scale_x_continuous(expand=ggplot2::expand_scale(mult=0.1))
gf




#media.edge<-read.csv("https://raw.githubusercontent.com/kateto/R-igraph-Network-Workshop-NetSciX/master/Dataset1-Media-Example-EDGES.csv")
#media.node<-read.csv("https://raw.githubusercontent.com/kateto/R-igraph-Network-Workshop-NetSciX/master/Dataset1-Media-Example-NODES.csv")


############ visNetwork ##############
# a nodes data.frame with id column, and a edges data.frame with from and to columns
nodes <- nodes %>%
  mutate(label=name) %>%
  rename(id=name)

# Si on ne veut pas que chacune des relations apparaisse :

edges_relations <- edges_relations %>%
  group_by(from,to) %>%
  summarise(count=n()) %>%
  as.data.frame()

visNetwork(nodes, edges_relations, width = "100%") %>%
  visNodes(color = list(background = "lightblue",
                        border = "darkblue",
                        highlight = "yellow"),
           shadow = list(enabled = TRUE, size = 10))




















require(googlesheets4)
require(tidyverse)
require(tidygraph)
require(ggraph)
require(igraph)
require(visNetwork)

# tutos:
# visNetwork : https://www.rpubs.com/Steven_Surya/visNetwork
# tidygraph : https://www.rpubs.com/Steven_Surya/tidygraph-ggraph , http://perso.ens-lyon.fr/lise.vaudor/des-graphes-bien-jolis/

# url to table
url <- "https://docs.google.com/spreadsheets/d/1s3VJ4M6BlQ6zkaVJiEE86n8hhYJfm4UBj9Gibn3YFQY/edit?usp=sharing"

# table relation with nodes and edges
edges <- googlesheets4::read_sheet(url,sheet = "liens")
nodes <-  googlesheets4::read_sheet(url,sheet = "noeuds")

graph=tidygraph::tbl_graph(nodes=nodes,edges=edges)
plot(graph)


vie_aquatique <- graph %>% activate(edges) %>% filter(type=="Vie aquatique")
vie_aerienne <- graph %>% activate(edges) %>% filter(type=="Vie aérienne")


vie_aquatique %>% graph_join(vie_aerienne) %>% plot()
vie_aquatique %>% bind_graphs(vie_aerienne) %>% plot()

g=graph %>% ggraph(layout="auto")

g4=g +
  geom_node_circle(aes(fill=etat, r=0.2)) +
  #geom_node_text(aes(label=name), color="black") +
  geom_node_label(aes(label=name),fill="steelblue",alpha=0.9) +
  geom_edge_link(arrow = arrow(length = unit(1, 'mm')), end_cap = circle(3, 'mm'))
g4




graph %>%
  ggraph(layout = 'sugiyama') +
  geom_edge_link(arrow = arrow(length = unit(2, 'mm')), end_cap = circle(3, 'mm')) +
  geom_node_point(size = 3) +  geom_node_text(aes(label = name, color = etat), size=3,repel = T) + theme_graph()


