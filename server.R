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
library(data.table)

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
  corresp_struct <- fread("Data/corresp_struct.csv", sep = ';')
  
  corresp_struct_PH_E <- read_delim("Data/corresp_struct_PH_E.csv", delim = ';')
  corresp_struct_PA <- read_delim("Data/corresp_struct_PA.csv", delim = ';')
  corresp_struct_PAPH <- read_delim("Data/corresp_struct_PAPH.csv", delim = ';')
  corresp_defic <- read_delim("Data/corresp_deficiences.csv", delim = ';',col_types = list(col_character(),col_character()))
  corresp_defic$client <- pad_left(corresp_defic$client,3)
  corresp_ta_ph <- read_delim("Data/corresp_ta_ph.csv", delim = ';',locale=locale(encoding = "UTF-8"))
  
  selection_categorie <- renderText({input$ch_struct})
  dep_selection_categorie <- renderText({input$dep_ch_struct})
  
  # Chaque fois qu'on change de type de structures (PH-A, PA,...) on cache éventuellement certaines tables
  observeEvent(input$ch_struct, {
    if (selection_categorie() %in% c("PH-A","PH-E")) {
      hideTab(inputId = "Tabs_Nat", target = "2")
      # hideTab(inputId = "Tabs_Nat", target = "6")
      showTab(inputId = "Tabs_Nat", target = "5")
    } else {
      showTab(inputId = "Tabs_Nat", target = "2")
      hideTab(inputId = "Tabs_Nat", target = "5") 
      # showTab(inputId = "Tabs_Nat", target = "6")
      }
  })
  
  # Gestion de l'affichage par département
  observeEvent(input$dep_ch_struct, {
    if (dep_selection_categorie() %in% c("PH-A","PH-E")) {
      hideTab(inputId = "Tabs_Dep", target = "2")
      showTab(inputId = "Tabs_Dep", target = "6")
      
    } else {
      showTab(inputId = "Tabs_Dep", target = "2")
      hideTab(inputId = "Tabs_Dep", target = "6")}
  })
  
  table_corresp <- reactive({
    table_corresp <- switch(selection_categorie(),
                            "PH-A" = corresp_struct_PH_A,
                            "PH-E" = corresp_struct_PH_E,
                            "PA" = corresp_struct_PA,
                            "PAPH" = corresp_struct_PAPH)
  })
  
  dep_table_corresp <- reactive({
    table_corresp <- switch(dep_selection_categorie(),
                            "PH-A" = corresp_struct_PH_A,
                            "PH-E" = corresp_struct_PH_E,
                            "PA" = corresp_struct_PA,
                            "PAPH" = corresp_struct_PAPH)
  })
  
  base_a_utiliser <- reactive({
    inp <- switch(
      selection_categorie(),
      "PH-A" = input$detail_struct_pha,
      "PH-E" = input$detail_struct_phe, 
      "PA" = input$detail_struct_pa,
      "PAPH" = input$detail_struct_paph)
    
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
      "PA" = input$dep_detail_struct_pa,
      "PAPH" = input$dep_detail_struct_paph)
    
    if ("All" %in% inp) {
      tempo <- dep_table_corresp()$categetab
    } else {
      tabb <- dep_table_corresp()
      tempo <- tabb[tabb$categetab %in% inp,]$categetab
    }
    
    
    dep_base_a_utiliser <- reduire_base_activite(annee_de_depart, annee_de_fin, dep_base_a_utiliser, tempo)
  })
  

  liste_departements <- unique(base_finess_reduite[[2021]]$departement)

  options_affichage_reduit <- list(searching = FALSE, dom = "Bfrtip", buttons = c('copy', 'csv', 'excel','pdf'), columnDefs = list(list(visible = F, targets = 0)), "pageLength" = 105)
  # Remarque : on a conservé les rownames pour que la dernière ligne en gras marche, puis on les enlève dans les options globales(visible = FALSE)

  ########
  # VUE NATIONALE
  ########
  
  nat_heb <- reactive({
    nat_heb <- output_national(annee_de_depart, annee_de_fin, base_a_utiliser(), "hebergement", "Type d'Hébergement", correspondance = corresp_hebergement, type_compte = input$ch_places)
    dimension <- dim(nat_heb)[1]
    nat_heb <- nat_heb %>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    if (input$ch_places == "places") nat_heb <- nat_heb %>% formatStyle(
      0, target = "row",
      fontWeight = styleEqual(dimension, "bold"))
    nat_heb
  })
  
  nat_sta <- reactive({
    nat_sta <- output_national(annee_de_depart, annee_de_fin, base_a_utiliser(), "statut", "Statut de l'Hebergement", correspondance = corresp_statut, type_compte = input$ch_places)
    dimension <- dim(nat_sta)[1]
    nat_sta <- nat_sta%>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    if (input$ch_places == "places") nat_sta <- nat_sta %>% formatStyle(
      0, target = "row",
      fontWeight = styleEqual(dimension, "bold"))
    nat_sta
  })
  
  nat_str <- reactive({
    nat_str <- output_national(annee_de_depart, annee_de_fin, base_a_utiliser(), "categetab", "Catégorie de la structure",correspondance = table_corresp(), type_compte = input$ch_places)
    dimension <- dim(nat_str)[1]
    nat_str <- nat_str%>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    if (input$ch_places == "places") nat_str <- nat_str %>% formatStyle(
      0, target = "row",
      fontWeight = styleEqual(dimension, "bold"))
    nat_str
  })
  
  nat_dep <- reactive({
    nat_dep <- output_national(annee_de_depart, annee_de_fin, base_a_utiliser(), "departement", "Département", type_compte = input$ch_places)
    nat_dep <- nat_dep %>% left_join(liste_dep[,c("code_departement","nom_departement", "nom_region")], by = c("variable"="code_departement"))
    nat_dep <- nat_dep[,c("nom_region","nom_departement",seq(annee_de_depart,annee_de_fin))]
    colnames(nat_dep) <- c("Région","Département",seq(annee_de_depart,annee_de_fin))
    nat_dep <- nat_dep%>% datatable(options = options_affichage_reduit, extensions = 'Buttons') %>% formatCurrency(3:(annee_de_fin-annee_de_depart+1),currency = "", interval = 3, digits = 0, mark = " ")
    nat_dep
  })
  
  nat_ta <- reactive({
    nat_ta <- output_national(annee_de_depart, annee_de_fin, base_a_utiliser(), "ta_ph", "Type d'activité", correspondance = corresp_ta_ph, type_compte = input$ch_places)
    dimension <- dim(nat_ta)[1]
    nat_ta <- nat_ta%>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    if (input$ch_places == "places") nat_ta <- nat_ta %>% formatStyle(
      0, target = "row",
      fontWeight = styleEqual(dimension, "bold"))
    nat_ta
  })
  
  nat_disp <- reactive({
    nat_disp <- output_national(annee_de_depart, annee_de_fin, base_a_utiliser(), "disp", "Comptage des dispositifs", type_compte = input$ch_places)
    dimension <- dim(nat_disp)[1]
    nat_disp <- nat_disp%>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    if (input$ch_places == "places") nat_disp <- nat_disp %>% formatStyle(
      0, target = "row",
      fontWeight = styleEqual(dimension, "bold"))
    nat_disp
  })
  
  nat_defic <- reactive({
    nat_defic <- output_national(annee_de_depart, annee_de_fin, base_a_utiliser(), "client", "Déficiences", correspondance = corresp_defic, type_compte = input$ch_places)
    dimension <- dim(nat_defic)[1]
    nat_defic <- nat_defic%>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    if (input$ch_places == "places") nat_defic <- nat_defic %>% formatStyle(
      0, target = "row",
      fontWeight = styleEqual(dimension, "bold"))
    nat_defic
  })
  
  nat_tp <- reactive({
    nat_tp <- output_national(annee_de_depart, annee_de_fin, base_a_utiliser(), "type_pub", "Type de Public", type_compte = input$ch_places)
    dimension <- dim(nat_tp)[1]
    nat_tp <- nat_tp%>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    if (input$ch_places == "places") nat_tp <- nat_tp %>% formatStyle(
      0, target = "row",
      fontWeight = styleEqual(dimension, "bold"))
    nat_tp
  })
  
  output$vue_nat_heb <- renderDataTable({nat_heb()})
  
  output$vue_nat_sta <- renderDataTable({nat_sta()})
  
  output$vue_nat_dep <- renderDataTable({nat_dep()})
  
  output$vue_nat_str <- renderDataTable({nat_str()})
  
  output$vue_nat_ta <- renderDataTable({nat_ta()})
  
  output$vue_nat_disp <- renderDataTable({nat_disp()})
  
  output$vue_nat_defic <- renderDataTable({nat_defic()})
  output$vue_nat_tp <- renderDataTable({nat_tp()})
  
  
  ######
  # VUE PAR DEPARTEMENT
  ######
  
  dep_heb <- reactive({
    dep_heb <- output_national(annee_de_depart, annee_de_fin, dep_base_a_utiliser(), "hebergement", "Type d'Hébergement", correspondance =corresp_hebergement,  type_compte = input$ch_places_dep)
    dimension <- dim(dep_heb)[1]
    dep_heb <- dep_heb %>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    if (input$ch_places_dep == "places") dep_heb <- dep_heb %>% formatStyle(
      0, target = "row",
      fontWeight = styleEqual(dimension, "bold"))
    dep_heb
  })
  
  dep_sta <- reactive({
    dep_sta <- output_national(annee_de_depart, annee_de_fin, dep_base_a_utiliser(), "statut", "Statut de l'Hebergement", correspondance = corresp_statut,  type_compte = input$ch_places_dep)
    dimension <- dim(dep_sta)[1]
    dep_sta <- dep_sta%>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    if (input$ch_places_dep == "places") dep_sta <- dep_sta %>% formatStyle(
      0, target = "row",
      fontWeight = styleEqual(dimension, "bold"))
    dep_sta
  })
  
  dep_str <- reactive({
    dep_str <- output_national(annee_de_depart, annee_de_fin, dep_base_a_utiliser(), "categetab", "Catégorie de la structure",correspondance = dep_table_corresp(),  type_compte = input$ch_places_dep)
    dimension <- dim(dep_str)[1]
    dep_str <- dep_str%>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    if (input$ch_places_dep == "places") dep_str <- dep_str %>% formatStyle(
      0, target = "row",
      fontWeight = styleEqual(dimension, "bold"))
    dep_str
  })
  
  dep_disp <- reactive({
    dep_disp <- output_national(annee_de_depart, annee_de_fin, dep_base_a_utiliser(), "disp", "Comptage des dispositifs", type_compte = input$ch_places_dep)
    dimension <- dim(dep_disp)[1]
    dep_disp <- dep_disp%>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    if (input$ch_places_dep == "places") dep_disp <- dep_disp %>% formatStyle(
      0, target = "row",
      fontWeight = styleEqual(dimension, "bold"))
    dep_disp
  })
  
  dep_ta <- reactive({
    dep_disp <- output_national(annee_de_depart, annee_de_fin, dep_base_a_utiliser(), "ta_ph", "Type d'activité", correspondance = corresp_ta_ph, type_compte = input$ch_places_dep)
    dimension <- dim(dep_disp)[1]
    dep_disp <- dep_disp%>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    if (input$ch_places_dep == "places") dep_disp <- dep_disp %>% formatStyle(0, target = "row",fontWeight = styleEqual(dimension, "bold"))
    dep_disp
  })
  
  dep_defic <- reactive({
    dep_defic <- output_national(annee_de_depart, annee_de_fin, dep_base_a_utiliser(), "client", "Déficiences", type_compte = input$ch_places_dep, correspondance = corresp_defic)
    dimension <- dim(dep_defic)[1]
    dep_defic <- dep_defic%>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    if (input$ch_places_dep == "places") dep_defic <- dep_defic %>% formatStyle(0, target = "row",fontWeight = styleEqual(dimension, "bold"))
    dep_defic
  })
  
  dep_tp <- reactive({
    dep_tp <- output_national(annee_de_depart, annee_de_fin, dep_base_a_utiliser(), "type_pub", "Type de Public", type_compte = input$ch_places_dep)
    dimension <- dim(dep_tp)[1]
    dep_tp <- dep_tp%>% datatable(options = options_affichage_reduit,rownames = TRUE, extensions = 'Buttons') %>% formatCurrency(2:(annee_de_fin-annee_de_depart+2),currency = "", interval = 3, digits = 0, mark = " ")
    if (input$ch_places_dep == "places") { dep_tp <- dep_tp %>% formatStyle(0, target = "row",fontWeight = styleEqual(dimension, "bold")) }
    dep_tp
  })
  
  extraction <- reactive({
    if ("Tous" %in% input$Extr_categetab) {
      categories_selectionnees <- "Tous"
    } else {
      categories_selectionnees <- corresp_struct[corresp_struct$equivalent %in% input$Extr_categetab,]$categetab
    }
      
    tablo <- extraire_tableau(base_finess_reduite, input$Extr_annee, categories_selectionnees)
    tablo <- tablo %>% datatable(rownames = FALSE, extensions = 'Buttons',
                                 options = list(
                                  dom = 'Bfrtip',
                                  buttons = list(
                                     list(extend = "excel", text = "Télécharger page actuelle", filename = "Extraction_Finess_partielle",
                                        exportOptions = list(
                                            modifier = list(page = "current")
                                        )
                                     ),
                                     list(extend = "excel", text = "Télécharger résultats complets", filename = "Extraction_Finess_Complete",
                                        exportOptions = list(
                                            modifier = list(page = "all")
                                        )
                                     )
                                  )
                                )
    )

  })
  
  output$vue_dep_heb <- renderDataTable({dep_heb()})
  
  output$vue_dep_sta <- renderDataTable({dep_sta()})
  
  output$vue_dep_str <- renderDataTable({dep_str()})
  
  output$vue_dep_disp <- renderDataTable({dep_disp()})
  
  output$vue_dep_defic <- renderDataTable({dep_defic()})
  
  output$vue_dep_ta <- renderDataTable({dep_ta()})
  
  output$vue_dep_tp <- renderDataTable({dep_tp()})
  
  output$basse_extraction <- DT::renderDT(server = FALSE, { extraction() })
  
})
