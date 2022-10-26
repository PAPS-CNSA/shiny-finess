# Fonctions utiles pour l'output

output_national <- function(annee1, annee2, base, variable, nom_de_sortie,type_compte = "places", correspondance=NULL) {
  # Fonction qui permet de sortir un tableau national comptant, en fonction de la "variable" 
  # le nombre de places totales (capinstot)
  
  for (annee in (annee1:annee2)) {
    tempo <-  base[[annee]]
    if (type_compte == "places") capacite <- tempo %>% group_by_at(variable) %>% summarise(capacite = sum(capinstot, na.rm=T))
    if (type_compte == "structures_pres") capacite <- tempo %>% group_by_at(variable) %>% summarise(capacite = n_distinct(nofinesset))
    if (type_compte == "structures_places") capacite <- tempo %>% filter(indsupinst=="N") %>% group_by_at(variable) %>% summarise(capacite = n_distinct(nofinesset))
    
    if (annee==annee1) {
      nat_sta <- capacite
      colnames(nat_sta) <- c("variable", annee)
    } else {
      colnames(capacite) <- c("variable", annee)
      nat_sta <- nat_sta %>% full_join(capacite, "variable")
    }

  }
  if (!is.null(correspondance)) {
    nom_test <- colnames(nat_sta)[1]
    nat_sta <- nat_sta %>% left_join(correspondance, by = c("variable" = variable))
    nat_sta <- nat_sta %>% select("equivalent", everything()) %>%
      select(-"variable")
    colnames(nat_sta) <- c(nom_de_sortie, colnames(nat_sta)[2:(annee2-annee1+2)])
  }
  nat_sta <- nat_sta %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum, na.rm=T),
                        across(where(is.character), ~"Total")))
  return(nat_sta)
}


reduire_base_activite <- function(annee1, annee2, base, critere) {
  output <- list()
  for (annee in (annee1:annee2)) {
    tempo <- base[[annee]]
    tempo <- tempo[tempo$categetab %in% critere,]
    output[[annee]] <- tempo
  }
  return(output)
}

reduire_dept <- function(annee1, annee2, base, critere) {
  output <- list()
  for (annee in (annee1:annee2)) {
    tempo <- base[[annee]]
    tempo <- tempo[tempo$departement %in% critere,]
    output[[annee]] <- tempo
  }
  return(output)
}