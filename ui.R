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
                        p("Cette application vous permet de consulter, et de télécharger, les décomptes en nombre de place dans les structures d'accueil telles que comptabilisées dans le répertoire FINESS"),
                        br(),
                        p("L'utilisation est assez simple. En haut de l'application, vous trouvez les deux dimensions proposées : nationale, ou départementale."),
                        p("Une fois votre choix effectué, tout se passe dans l'onglet à gauche. Il vous permet de choisir le champ des structures étudiées (PA, ou PH, avec une distinction adultes et enfants). Puis, une fois votre choix fait, il vous faut choisir si vous voulez toutes les structures associées (cocher alors 'tout') ou seulement certaines (les cocher dans ce cas). Attention : si l'option 'tout' est cochée, alors les autres options ne servent à rien, puisqu'on prend tout par défaut..."),
                        p("Ensuite, vous avez le choix entre les sous-onglets de droite, qui permettent de ventiler suivant différentes options"),
                        p("Les données peuvent ensuite être téléchargées de différentes façons au moyens des boutons dédiées : Excel et csv permettent un téléchargement sous cette forme, tandis que 'Copy' vous permet de copier les données du tableau dans le presse-papier, de façon à pouvoir immédiatement 'coller' dans le support de votre choix (power point, word, mail,...)"),
                        p("Elément important de cette interface : elle vise à répondre à un besoin. Si certaines dimensions vous semblent manquantes, ou si d'autres angles vous semblent pertinents, n'hésitez pas à nous en faire part afin que nous puissions la faire évoluer !")
                        ),
               tabPanel("Vue nationale",
                  sidebarLayout(
                    sidebarPanel(
                      fluidRow(
                        radioButtons("ch_places", label = h3("Choix du type de décompte"),
                                     choices = list("En places" = "places", "En Nombre de structures avec nombre de places positives" = "structures_places", "En nombre de structures recensées dans la base"="structures_pres"), selected = "places"),
                        radioButtons("ch_struct", label = h3("Champ de la structure envisagée"), 
                                           choices = list("Adultes Handicapés" = "PH-A", "Enfants Handicapés" = "PH-E", "Personnes Âgées" = "PA"),
                                           selected = "PH-A"),
                        hr(),

                        conditionalPanel(condition = "input.ch_struct == 'PH-A'",
                              checkboxGroupInput("detail_struct_pha", label = h3("Choix des structures à comptabiliser"), 
                                  choices = c(
                                    "Tous"="All",
                                    "198 - ESPO" = "198","246 - ESAT" = "246","249 - ESRP" = "249","252 - FHAH" = "252","253 - FPAH" = "253",
                                    "255 - MAS" = "255","379 - EEAH" = "379","382 - FVAH" = "382","395 - EATAH" = "395","437 - FAM" = "437","448 - EAM" = "448",
                                    "449 - EANM" = "449","461 - CR"= "461","462 - Lieux de Vie" = "462","464 - UEROS" = "464","445 - SAMSAH" = "445","446 - SAVS" = "446"
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
                        )

                      )
                    ),
                    mainPanel(
                        tabsetPanel(
                            id = 'Type de variable',
                            tabPanel("Type de Structure", 
                                     fluidRow(
                                       h3("Nombre de places par type de structure"),
                                       hr(),
                                       "Le tableau présente le nombre de places comptabilisées (places effectives), selon le type de structure sélectionnées à gauche, et par année."),
                                       hr(),
                                     dataTableOutput("vue_nat_str")
                            ),
                              tabPanel("Type d'Hébergement", 
                                 fluidRow(
                                   h3("Nombre de places par type d'hébergement"),
                                   hr(),
                                   "Le tableau présente le nombre de places comptabilisées (places effectives), selon le type d'hébergement. A noter que cette distinction peut ne pas faire sens selon le champ sélectionné."),
                                  hr(),
                                 dataTableOutput("vue_nat_heb")

                              ),
                              tabPanel("Statut", 
                                 fluidRow(
                                   h3("Nombre de places selon le statut de l'établissement"),
                                   hr(),
                                   "Le tableau présente le nombre de places comptabilisées (places effectives), selon le statut (public ou privé) de l'établissement."),
                                   hr(),
                                 dataTableOutput("vue_nat_sta")
                              ),
                              tabPanel("Par Département", 
                                 fluidRow(
                                   h3("Nombre de places par département"),
                                   hr(),
                                   "Le tableau présente le nombre de places comptabilisées dans les structures sélectionnées, ventilées par département."),
                                    hr(),
                                   dataTableOutput("vue_nat_dep")   
                              ),
                                  tabPanel("Type d'Activité", 
                                     fluidRow(
                                       h3("Nombre de places selon le type d'activités / les modalités d'accueil de l'établissement"),
                                       hr(),
                                       "Le tableau présente le nombre de places comptabilisées (places effectives), selon les modalités d'accueil de l'établissement."),
                                     hr(),
                                     dataTableOutput("vue_nat_ta")
                                  ),
                                  tabPanel("Type de Dispositifs", 
                                     fluidRow(
                                       h3("Nombre de places selon le dispositfs"),
                                       hr(),
                                       "Le tableau présente le nombre places dispositifs"),
                                     hr(),
                                     dataTableOutput("vue_nat_disp")
                            )
                             )
                            
                          )
                      )),
               tabPanel("Vue par Département",
                        sidebarLayout(
                          sidebarPanel(
                            fluidRow(
                              selectInput("choix_dep","Choix du Département :", liste_dep$code_departement),
                              radioButtons("dep_ch_struct", label = h3("Champ de la structure envisagée"), 
                                           choices = list("Adultes Handicapés" = "PH-A", "Enfants Handicapés" = "PH-E", "Personnes Âgées" = "PA"),
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
                              )
                              
                            )
                          ),
                          mainPanel(
                            tabsetPanel(
                              id = 'Type de variable',
                              tabPanel("Type de Structure", 
                                       fluidRow(
                                         h3("Nombre de places par type de structure"),
                                         hr(),
                                         "Le tableau présente le nombre de places comptabilisées (places effectives), selon le type de structure sélectionnées à gauche, et par année."),
                                       hr(),
                                       dataTableOutput("vue_dep_str")
                              ),
                              tabPanel("Type d'Hébergement", 
                                       fluidRow(
                                         h3("Nombre de places par type d'hébergement"),
                                         hr(),
                                         "Le tableau présente le nombre de places comptabilisées (places effectives), selon le type d'hébergement. A noter que cette distinction peut ne pas faire sens selon le champ sélectionné."),
                                       hr(),
                                       dataTableOutput("vue_dep_heb")
                                       
                              ),
                              tabPanel("Statut", 
                                       fluidRow(
                                         h3("Nombre de places selon le statut de l'établissement"),
                                         hr(),
                                         "Le tableau présente le nombre de places comptabilisées (places effectives), selon le statut (public ou privé) de l'établissement."),
                                       hr(),
                                       dataTableOutput("vue_dep_sta")
                              )
                            )
                          )
                        )
                            )
               
               
))
)
