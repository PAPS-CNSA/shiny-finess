#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(readr)
library(tidyverse)
library(DT)

liste_dep <- read_delim("Data/departements-france.csv", delim = ";")


# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    # Application title
    #titlePanel("Quelques éléments sur l'offre telle que présente dans FINESS"),
    navbarPage("L'Offre vue de Finess",
               tabPanel("Accueil",
                        h2("Bienvenue dans l'application de consultation des données FINESS"),
                        hr(),
                        p("Cette application vous permet de consulter, et de télécharger, les décomptes en nombre de place dans les structures d'accueil telles que comptabilisées dans le répertoire FINESS, ainsi que le nombre de structures associées."),
                        p("Les données présentées correspondent exactement aux données présentes dans FINESS ; la source à indiquer en cas de reprise est donc 'Finess'."),
                        br(),
                        p("Pour plus d'informations, merci de vous référer à la documentation qui se trouve ci-dessous, ou de contacter le Pôle Analyses et Production Statistiques."),
                        a("Documentation de l'application",href = "https://docs.google.com/document/d/1Of8moROCL2xk7zuCGUI8gIC6zr4cQ9aC/edit?usp=sharing&ouid=104010900611395673143&rtpof=true&sd=true"),
                        hr(),
                         ),
               tabPanel("Vue nationale",
                  sidebarLayout(
                    sidebarPanel(
                      fluidRow(
                        radioButtons("ch_places", label = h3("Choix du type de décompte"),
                                     choices = list("En places" = "places", "En Nombre de structures avec places installées" = "structures_places", "En nombre de structures recensées dans la base"="structures_pres"), selected = "places"),
                        radioButtons("ch_struct", label = h3("Champ de la structure envisagée"), 
                                           choices = list("Adultes Handicapés" = "PH-A", "Enfants Handicapés" = "PH-E", "Personnes Âgées" = "PA", "Etablissements et Services Multiclientèles" = "PAPH"),
                                           selected = "PH-A"),
                        hr(),

                        conditionalPanel(condition = "input.ch_struct == 'PH-A'",
                              checkboxGroupInput("detail_struct_pha", label = h3("Choix des structures à comptabiliser"), 
                                  choices = c(
                                    "Tous"="All",
                                    "198 - ESPO" = "198","246 - ESAT" = "246","249 - ESRP" = "249","252 - FHAH" = "252","253 - FPAH" = "253",
                                    "255 - MAS" = "255","379 - EEAH" = "379","382 - FVAH" = "382","395 - EATAH" = "395","437 - FAM" = "437","448 - EAM" = "448",
                                    "449 - EANM" = "449","461 - CR"= "461","464 - UEROS" = "464","445 - SAMSAH" = "445","446 - SAVS" = "446"
                                  ))
                        ),
                        conditionalPanel(condition = "input.ch_struct == 'PA'",
                                         checkboxGroupInput("detail_struct_pa", label = h3("Choix des structures à comptabiliser"), 
                                                            choices = c(
                                                              "Tous"="All","202 - RA" = "202","207 - AJA" = "207","362 - ESLD" = "362",
                                                              "381 - EEPA" = "381","500 - EHPAD" = "500","501 - EHPA perc crédit AM" = "501",
                                                              "502 - EHPA ne perc pas crédit Am" = "502"
                                                            ))
                        ),
                        conditionalPanel(condition = "input.ch_struct == 'PH-E'",
                                         checkboxGroupInput("detail_struct_phe", label = h3("Choix des structures à comptabiliser"), 
                                                            choices = c(
                                                              "Tous"="All",
                                                              "183 - IME" = "183","186 - ITEP" = "186","188 - EEAP" = "188","192 - IEM" = "192",
                                                              "194 - IDV" = "194","195 - IDA" = "195","196 - IESPESA" = "196","238 - CAFS" = "238",
                                                              "377 - EEEH" = "377","390 - EATEH" = "390","396 - FOYPH" = "396","402 - JES"= "402","182 - SESSAD" = "182",
                                                              "189 - CMPP" = "189","190 - CAMSP" = "190","221 - BAPU" = "221"
                                                            ))
                        ),
                        conditionalPanel(condition = "input.ch_struct == 'PAPH'",
                                         checkboxGroupInput("detail_struct_paph", label = h3("Choix des structures à comptabiliser"), 
                                                            choices = c(
                                                              "Tous"="All",
                                                              "209 - SPASAD" = "209","354 - SSIAD" = "354","370 - EEPH" = "370", "460 - SAAD" = "460","462 - Lieux de Vie" = "462"
                                                            ))
                        )

                      )
                    ),
                    mainPanel(
                      
                        tabsetPanel(
                            type = "tabs",
                            id = 'Tabs_Nat',
                            tabPanel("Type de Structure", value = 1,
                                       hr(),
                                     dataTableOutput("vue_nat_str")
                            ),
                              tabPanel("Type d'Hébergement", value = 2,
                                  hr(),
                                 dataTableOutput("vue_nat_heb")

                              ),
                              tabPanel("Statut", value = 3,
                                   hr(),
                                 dataTableOutput("vue_nat_sta")
                              ),
                              tabPanel("Par Département", value = 4, 
                                    hr(),
                                   dataTableOutput("vue_nat_dep")   
                              ),
                                  tabPanel("Type d'Activité", value = 5,
                                     hr(),
                                     dataTableOutput("vue_nat_ta")
                                  ),
                                  tabPanel("Type de Dispositifs", value = 6,

                                     hr(),
                                     dataTableOutput("vue_nat_disp")
                                  ),
                                  tabPanel("Déficiences", value = 7,
                                     hr(),
                                     dataTableOutput("vue_nat_defic")
                            ),
                            tabPanel("Type de Public", value = 7,
                                     hr(),
                                     dataTableOutput("vue_nat_tp")
                            )
                             )
                            
                          )
                      )),
               tabPanel("Vue par Département",
                        sidebarLayout(
                          sidebarPanel(
                            fluidRow(
                              selectInput("choix_dep","Choix du Département :", liste_dep$code_departement),
                              radioButtons("ch_places_dep", label = h3("Choix du type de décompte"),
                                           choices = list("En places" = "places", "En Nombre de structures avec places installées" = "structures_places", "En nombre de structures recensées dans la base"="structures_pres"), selected = "places"),
                              radioButtons("dep_ch_struct", label = h3("Champ de la structure envisagée"), 
                                           choices = list("Adultes Handicapés" = "PH-A", "Enfants Handicapés" = "PH-E", "Personnes Âgées" = "PA","Etablissements et Services Multiclientèles" = "PAPH"),
                                           selected = 1),
                              hr(),
                              
                              conditionalPanel(condition = "input.dep_ch_struct == 'PH-A'",
                                               checkboxGroupInput("dep_detail_struct_pha", label = h3("Choix des structures à comptabiliser"), 
                                                                  choices = c(
                                                                    "Tous"="All",
                                                                    "198 - ESPO" = "198","246 - ESAT" = "246","249 - ESRP" = "249","252 - FHAH" = "252","253 - FPAH" = "253",
                                                                    "255 - MAS" = "255","379 - EEAH" = "379","382 - FVAH" = "382","395 - EATAH" = "395","437 - FAM" = "437","448 - EAM" = "448",
                                                                    "449 - EANM" = "449","461 - CR"= "461","462 - Lieux de Vie" = "462","464 - UEROS" = "464","445 - SAMSAH" = "445","446 - SAVS" = "446"
                                                                  ))
                              ),
                              conditionalPanel(condition = "input.dep_ch_struct == 'PA'",
                                               checkboxGroupInput("dep_detail_struct_pa", label = h3("Choix des structures à comptabiliser"), 
                                                                  choices = c(
                                                                    "Tous"="All","RA" = "202","AJA" = "207",
                                                                    "381 - EEPA" = "381","500 - EHPAD" = "500","501 - EHPA perc crédit AM" = "501",
                                                                    "502 - EHPA ne perc pas crédit Am" = "502"
                                                                  ))
                              ),
                              conditionalPanel(condition = "input.dep_ch_struct == 'PH-E'",
                                               checkboxGroupInput("dep_detail_struct_phe", label = h3("Choix des structures à comptabiliser"), 
                                                                  choices = c(
                                                                    "Tous"="All",
                                                                    "183 - IME" = "183","186 - ITEP" = "186","188 - EEAP" = "188","192 - IEM" = "192",
                                                                    "194 - IDV" = "194","195 - IDA" = "195","196 - IESPESA" = "196","238 - CAFS" = "238",
                                                                    "377 - EEEH" = "377","390 - EATEH" = "390","396 - FOYPH" = "396","402 - JES"= "402","182 - SESSAD" = "182",
                                                                    "189 - CMPP" = "189","190 - CAMSP" = "190","221 - BAPU" = "221"
                                                                  ))
                              ),
                              conditionalPanel(condition = "input.dep_ch_struct == 'PAPH'",
                                               checkboxGroupInput("dep_detail_struct_paph", label = h3("Choix des structures à comptabiliser"), 
                                                                  choices = c(
                                                                    "Tous"="All",
                                                                    "209 - SPASAD" = "209","354 - SSIAD" = "354","460 - SAAD" = "460","462 - Lieux de Vie" = "462"
                                                                  ))
                              )
                              
                            )
                          ),
                          mainPanel(
                            tabsetPanel(
                              type = "tabs",
                              id = 'Tabs_Dep',
                              tabPanel("Type de Structure", value = 1,hr(),dataTableOutput("vue_dep_str")),
                              tabPanel("Type d'Hébergement", value = 2,hr(),dataTableOutput("vue_dep_heb")),
                              tabPanel("Statut", value = 3,hr(),dataTableOutput("vue_dep_sta")),
                              tabPanel("Dispositif", value = 4, hr(),dataTableOutput("vue_dep_disp")),
                              tabPanel("Déficiences", value = 5,hr(),dataTableOutput("vue_dep_defic")),
                              tabPanel("Type d'Activité", value = 6, hr(), dataTableOutput("vue_dep_ta")),
                              tabPanel("Type de public", value = 7, hr(), dataTableOutput("vue_dep_tp"))
                            )
                          )
                        )
                            )
               
               
))
)
