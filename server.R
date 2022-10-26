#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(DT)
library(highcharter)
library(tidyverse)

source("fonctions.R")
load("Data/Base_Finess.RData")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Adaptation de la base en fonction du choix
  
  # Chargement des tables de correspondance
  liste_dep <- read_delim("Data/departements-france.csv", delim = ";")
  corresp_hebergement <- read_delim("Data/corresp_hebergement.csv", delim = ';')
  corresp_statut <- read_delim("Data/corresp_statut.csv", delim = ';')
  corresp_struct_PH_A <- read_delim("Data/corresp_struct_PH_A.csv", delim = ';')
  corresp_struct_PH_E <- read_delim("Data/corresp_struct_PH_E.csv", delim = ';')
  corresp_struct_PA <- read_delim("Data/corresp_struct_PA.csv", delim = ';')
  corresp_ta_ph <- read_delim("Data/corresp_ta_ph.csv", delim = ';',locale=locale(encoding = "UTF-8"))
  
  selection_categorie <- renderText({input$ch_struct})
  dep_selection_categorie <- renderText({input$dep_ch_struct})
  
  # Chaque fois qu'on change de type de structures (PH-A, PA,...) on cache éventuellement certaines tables
  observeEvent(input$ch_struct, {
    if (selection_categorie() %in% c("PH-A","PH-E")) {
      hideTab(inputId = "Tabs_Nat", target = "2")
      showTab(inputId = "Tabs_Nat", target = "5")
    } else {
      showTab(inputId = "Tabs_Nat", target = "2")
      hideTab(inputId = "Tabs_Nat", target = "5")    }
  })
  
  table_corresp <- reactive({
    table_corresp <- switch(selection_categorie(),
                            "PH-A" = corresp_struct_PH_A,
                            "PH-E" = corresp_struct_PH_E,
                            "PA" = corresp_struct_PA)
  })
  
  dep_table_corresp <- reactive({
    table_corresp <- switch(dep_selection_categorie(),
                            "PH-A" = corresp_struct_PH_A,
                            "PH-E" = corresp_struct_PH_E,
                            "PA" = corresp_struct_PA)
  })
  
  base_a_utiliser <- reactive({
    inp <- switch(
      selection_categorie(),
      "PH-A" = input$detail_struct_pha,
      "PH-E" = input$detail_struct_phe, 
      "PA" = input$detail_struct_pa)
    
    if ("All" %in% inp) {
        tempo <- table_corresp()$categetab
    } else {
      tabb <- table_corresp()
      tempo <- tabb[tabb$categetab %in% inp,]$categetab
    }
    base_a_utiliser <- reduire_base_activite(annee_de_depart, annee_de_fin, base_finess_reduite, tempo)
  })
  
  choix_departement <- reactive({ input$choix_dep })
  
  ############### PAR DEPARTEMENT
  
  dep_base_a_utiliser <- reactive({
    dep_base_a_utiliser <- reduire_dept(annee_de_depart, annee_de_fin, base_finess_reduite, choix_departement())
    inp <- switch(
      dep_selection_categorie(),
      "PH-A" = input$dep_detail_struct_pha,
      "PH-E" = input$dep_detail_struct_phe, 
      "PA" = input$dep_detail_struct_pa)
    
    if ("All" %in% inp) {
      tempo <- dep_table_corresp()$categetab
    } else {
      tabb <- dep_table_corresp()
      tempo <- tabb[tabb$categetab %in% inp,]$categetab
    }
    
    
    dep_base_a_utiliser <- reduire_base_activite(annee_de_depart, annee_de_fin, dep_base_a_utiliser, tempo)
  })
  

  liste_departements <- unique(base_finess_reduite[[2021]]$departement)

  options_affichage_reduit <- list(searching = FALSE, dom = "Blfrtip", buttons = c('copy', 'csv', 'excel'), columnDefs = list(list(visible = F, targets = 0)))
  # Remarque : on a conservé les rownames pour que la dernière ligne en gras marche, puis on les enlève dans les options globales(visible = FALSE)

  ########
  # VUE NATIONALE
  ########
  
  nat_heb <- reactive({
    nat_heb <- output_national(annee_de_depart, annee_de_fin, base_a_utiliser(), "hebergement", "Type d'Hébergement", correspondance = corresp_hebergement, type_compte = input$ch_places)
    dimension <- dim(nat_heb)[1]
    nat_heb <- nat_heb %>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    nat_heb <- nat_heb %>% formatStyle(
      0, target = "row",
      fontWeight = styleEqual(dimension, "bold"))
  })
  
  nat_sta <- reactive({
    nat_sta <- output_national(annee_de_depart, annee_de_fin, base_a_utiliser(), "statut", "Statut de l'Hebergement", correspondance = corresp_statut, type_compte = input$ch_places)
    dimension <- dim(nat_sta)[1]
    nat_sta <- nat_sta%>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    nat_sta <- nat_sta %>% formatStyle(
      0, target = "row",
      fontWeight = styleEqual(dimension, "bold"))
  })
  
  nat_str <- reactive({
    nat_str <- output_national(annee_de_depart, annee_de_fin, base_a_utiliser(), "categetab", "Catégorie de la structure",correspondance = table_corresp(), type_compte = input$ch_places)
    dimension <- dim(nat_str)[1]
    nat_str <- nat_str%>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    nat_str <- nat_str %>% formatStyle(
      0, target = "row",
      fontWeight = styleEqual(dimension, "bold"))
  })
  
  nat_dep <- reactive({
    nat_dep <- output_national(annee_de_depart, annee_de_fin, base_a_utiliser(), "departement", "Département", type_compte = input$ch_places)
    nat_dep <- nat_dep %>% left_join(liste_dep[,c("code_departement","nom_departement", "nom_region")], by = c("variable"="code_departement"))
    nat_dep <- nat_dep[,c("nom_region","nom_departement",seq(annee_de_depart,annee_de_fin))]
    colnames(nat_dep) <- c("Région","Département",seq(annee_de_depart,annee_de_fin))
    nat_dep <- nat_dep%>% datatable(options = options_affichage_reduit, extensions = 'Buttons') %>% formatCurrency(3:(annee_de_fin-annee_de_depart+1),currency = "", interval = 3, digits = 0, mark = " ")
  })
  
  nat_ta <- reactive({
    nat_ta <- output_national(annee_de_depart, annee_de_fin, base_a_utiliser(), "ta_ph", "Type d'activité", correspondance = corresp_ta_ph, type_compte = input$ch_places)
    dimension <- dim(nat_ta)[1]
    nat_ta <- nat_ta%>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    nat_ta <- nat_ta %>% formatStyle(
      0, target = "row",
      fontWeight = styleEqual(dimension, "bold"))
  })
  
  nat_disp <- reactive({
    nat_disp <- output_national(annee_de_depart, annee_de_fin, base_a_utiliser(), "disp", "Comptage des dispositifs", type_compte = input$ch_places)
    dimension <- dim(nat_disp)[1]
    nat_disp <- nat_disp%>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    nat_disp <- nat_disp %>% formatStyle(
      0, target = "row",
      fontWeight = styleEqual(dimension, "bold"))
  })
  
  output$vue_nat_heb <- renderDataTable({nat_heb()})
  
  output$vue_nat_sta <- renderDataTable({nat_sta()})
  
  output$vue_nat_dep <- renderDataTable({nat_dep()})
  
  output$vue_nat_str <- renderDataTable({nat_str()})
  
  output$vue_nat_ta <- renderDataTable({nat_ta()})
  
  output$vue_nat_disp <- renderDataTable({nat_disp()})
  
  ######
  # VUE PAR DEPARTEMENT
  ######
  
  dep_heb <- reactive({
    dep_heb <- output_national(annee_de_depart, annee_de_fin, dep_base_a_utiliser(), "hebergement", "Type d'Hébergement", correspondance =corresp_hebergement,  type_compte = input$ch_places_dep)
    dimension <- dim(dep_heb)[1]
    dep_heb <- dep_heb %>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    dep_heb <- dep_heb %>% formatStyle(
      0, target = "row",
      fontWeight = styleEqual(dimension, "bold"))
  })
  
  dep_sta <- reactive({
    dep_sta <- output_national(annee_de_depart, annee_de_fin, dep_base_a_utiliser(), "statut", "Statut de l'Hebergement", correspondance = corresp_statut,  type_compte = input$ch_places_dep)
    dimension <- dim(dep_sta)[1]
    dep_sta <- dep_sta%>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    dep_sta <- dep_sta %>% formatStyle(
      0, target = "row",
      fontWeight = styleEqual(dimension, "bold"))
  })
  
  dep_str <- reactive({
    dep_str <- output_national(annee_de_depart, annee_de_fin, dep_base_a_utiliser(), "categetab", "Catégorie de la structure",correspondance = dep_table_corresp(),  type_compte = input$ch_places_dep)
    dimension <- dim(dep_str)[1]
    dep_str <- dep_str%>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    dep_str <- dep_str %>% formatStyle(
      0, target = "row",
      fontWeight = styleEqual(dimension, "bold"))
  })
  
  dep_disp <- reactive({
    dep_disp <- output_national(annee_de_depart, annee_de_fin, dep_base_a_utiliser(), "disp", "Comptage des dispositifs", type_compte = input$ch_places_dep)
    dimension <- dim(dep_disp)[1]
    dep_disp <- dep_disp%>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    dep_disp <- dep_disp %>% formatStyle(
      0, target = "row",
      fontWeight = styleEqual(dimension, "bold"))
  })
  
  output$vue_dep_heb <- renderDataTable({dep_heb()})
  
  output$vue_dep_sta <- renderDataTable({dep_sta()})
  
  output$vue_dep_str <- renderDataTable({dep_str()})
  
  output$vue_dep_disp <- renderDataTable({dep_disp()})
  
})
