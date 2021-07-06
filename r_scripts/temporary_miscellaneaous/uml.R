# https://plantuml.com/fr/state-diagram
# https://github.com/rkrug/plantuml

# UML généralités : https://openclassrooms.com/fr/courses/2035826-debutez-lanalyse-logicielle-avec-uml/2048781-les-differents-types-de-diagrammes

require(plantuml)

x_female_vector <- '
hide empty description
[*] --> Oeuf
Oeuf --> Larve
Larve --> Nymphe
Nymphe --> Immature : émergeance
Immature --> Unfed : accouplement
Unfed --> RechercheHôte
state fork_state <<fork>>
RechercheHôte --> fork_state : Piqûre
fork_state --> Infectieux : sur hôte \\n infecté
fork_state --> NonInfectieux : sur hôte \\n sain
state join_state <<join>>
Infectieux --> join_state
NonInfectieux --> join_state
join_state --> Repos
Repos --> RechercheGitePonte
RechercheGitePonte --> Ponte
Ponte --> RechercheHôte
Repos --> RechercheHôte
Ponte --> [*]
'
x_plantuml <- plantuml(x_female_vector)
plot(x_plantuml,file = "/home/ptaconet/Bureau/plantuml.png" )


x_lav<-paste(x_female_vector,
'Oeuf --> [*]
Larve --> [*]
Nymphe --> [*]
Immature --> [*]
Unfed --> [*]
RechercheHôte --> [*]
Repos --> [*]
RechercheGitePonte --> [*]
')
x_plantuml <- plantuml(x_lav)
plot(x_plantuml)


'
state Piqûre {
  state "Idle mode" as Idle
  state "Configuring mode" as Configuring
  [*] --> Idle
  Idle --> Configuring : EvConfig
  Configuring --> Idle : EvConfig
}
''
