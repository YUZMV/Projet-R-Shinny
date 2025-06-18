# ----- Chargement des bibliothèques nécessaires -----
library(shiny)             # Framework pour apps web interactives
library(shinydashboard)    # Pour un design type tableau de bord
library(tidyverse)         # Manipulation de données (dplyr, ggplot2, etc.)
library(plotly)            # Graphiques interactifs
library(shinyauthr)        # Authentification utilisateur
library(shinyjs)           # Gestion du DOM, afficher/masquer des éléments
library(DT)                # Tables interactives avec options avancées

# ----- Données utilisateurs -----
# Création d'une base d'utilisateurs avec noms, mots de passe chiffrés et permissions
user_base <- tibble::tibble(
  user = c("user1", "user2", "pascal"),                             # Identifiants utilisateurs
  password = purrr::map_chr(c("pass1", "pass2", "ga-dash"), sodium::password_store), # Mots de passe chiffrés
  permissions = c("admin", "standard", "admin"),                    # Permissions (non exploitées ici mais utiles)
  name = c("User One", "User Two", "Goat")                          # Noms affichés
)

# Chargement du dataset médical (chemin relatif)
heart_data <- read.csv("./data/Medicaldataset_clean.csv")

# Chargement du modèle Random Forest entraîné
rf_model <- readRDS("./models/rf_heart_failure_model.rds")

# Noms internes des visualisations avec leurs étiquettes en français pour le menu
visualizations <- c(
  "scatter_age_troponin", 
  "bar_gender_result", 
  "hist_age", 
  "scatter_ckmb_troponin"
) %>%
  set_names(c(
    "Âge vs Troponine", 
    "Risque par Sexe", 
    "Répartition des Âges", 
    "CK-MB vs Troponine"
  ))

# ----- Interface Utilisateur -----
ui <- fluidPage(
  shinyjs::useShinyjs(),                                  # Activation shinyjs pour manipuler l'affichage
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")), # Ajout CSS personnalisé
  
  # Panneau de connexion visible au départ
  div(id = "login_panel", loginUI("login")),
  
  # Contenu principal masqué tant que non connecté
  hidden(div(id = "app_content",
             dashboardPage(
               skin = "blue",                               # Thème de couleur du dashboard
               
               # En-tête avec titre et bouton logout
               dashboardHeader(title = "Tableau de bord - Crises Cardiaques",
                               tags$li(class = "dropdown", logoutUI("logout"))),
               
               # Barre latérale avec navigation
               dashboardSidebar(
                 sidebarMenu(
                   id = "tabs",                            # ID menu pour observer le choix
                   menuItem("Visualisations", tabName = "dashboard", icon = icon("chart-bar")),
                   menuItem("Filtres", tabName = "filters", icon = icon("sliders-h")),
                   menuItem("Prédiction", tabName = "prediction", icon = icon("heartbeat")) # NOUVEAU
                 )
               ),
               
               # Corps du dashboard
               dashboardBody(
                 tabItems(
                   # Onglet Visualisations
                   tabItem("dashboard",
                           fluidRow(
                             valueBoxOutput("percent_positive"),   # Pourcentage patients positifs
                             valueBoxOutput("avg_troponin"),       # Moyenne troponine
                             valueBoxOutput("avg_age")              # Âge moyen
                           ),
                           fluidRow(
                             box(title = "Visualisation principale", width = 6, status = "primary", solidHeader = TRUE,
                                 plotlyOutput("main_plot", height = "350px")),  # Graphique principal interactif
                             box(title = "Données filtrées (6 premières lignes)", width = 6, status = "info", solidHeader = TRUE,
                                 DT::dataTableOutput("data_table"), height = "400px") # Tableau des données
                           )
                   ),
                   
                   # Onglet Filtres
                   tabItem("filters",
                           box(title = "Filtres de données", width = 6, status = "info", solidHeader = TRUE,
                               selectInput("gender_filter", "Sexe", choices = c("Tous" = "all", "Femme" = 0, "Homme" = 1)),
                               sliderInput("age_range", "Tranche d'âge", 
                                           min = min(heart_data$age, na.rm = TRUE), 
                                           max = max(heart_data$age, na.rm = TRUE), 
                                           value = range(heart_data$age, na.rm = TRUE)),
                               selectInput("result_filter", "Résultat", choices = c("Tous" = "all", "Négatif" = 0, "Positif" = 1)),
                               selectInput("viz_select", "Visualisation", choices = visualizations)
                           )
                   ),
                   
                   # Onglet Prédiction (NOUVEAU)
                   tabItem("prediction",
                           box(title = "Entrer les informations du patient", width = 12, status = "warning", solidHeader = TRUE,
                               fluidRow(
                                 column(4, numericInput("pred_age", "Âge (années)", value = 60, min = 1, max = 120)),
                                 column(4, selectInput("pred_gender", "Sexe", choices = c("Femme" = 0, "Homme" = 1))),
                                 column(4, numericInput("pred_heart_rate", "Fréquence cardiaque (bpm)", value = 70, min = 30, max = 200))
                               ),
                               fluidRow(
                                 column(4, numericInput("pred_sbp", "Pression systolique (mmHg)", value = 120, min = 50, max = 250)),
                                 column(4, numericInput("pred_dbp", "Pression diastolique (mmHg)", value = 80, min = 30, max = 150)),
                                 column(4, numericInput("pred_blood_sugar", "Glycémie (mg/dL)", value = 100, min = 50, max = 500))
                               ),
                               fluidRow(
                                 column(4, numericInput("pred_ckmb", "CK-MB (ng/mL)", value = 5, min = 0, max = 100)),
                                 column(4, numericInput("pred_troponin", "Troponine (ng/mL)", value = 0.05, min = 0, max = 10)),
                                 column(4, br(), actionButton("predict_btn", "Prédire le risque", icon = icon("play"), class = "btn-danger", width = "100%"))
                               )
                           ),
                           box(title = "Résultat de la prédiction", width = 12, status = "success", solidHeader = TRUE,
                               verbatimTextOutput("prediction_result")
                           )
                   )
                 ),
                 
                 # Pied de page
                 tags$footer(class = "main-footer",
                             HTML("&copy; 2025 Dashboard Médical - Tous droits réservés"))
               )
             )
  ))
)

# ----- Serveur -----
server <- function(input, output, session) {
  
  # Serveur d'authentification
  credentials <- loginServer(
    id = "login",                    # Id du module login
    data = user_base,                # Base utilisateurs
    user_col = user,                 # Colonne identifiants
    pwd_col = password,              # Colonne mots de passe (hashés)
    sodium_hashed = TRUE,            # Indique que mots de passe sont hashés avec sodium
    log_out = reactive(logout_init()) # Réactive la déconnexion
  )
  
  # Serveur pour gérer la déconnexion
  logout_init <- logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)  # Activé seulement si connecté
  )
  
  # Observer l'état d'authentification pour afficher ou masquer la partie login / app
  observe({
    if (credentials()$user_auth) {
      shinyjs::hide("login_panel")   # Masquer panneau login
      shinyjs::show("app_content")   # Afficher dashboard
    } else {
      shinyjs::show("login_panel")   # Afficher login
      shinyjs::hide("app_content")   # Masquer dashboard
    }
  })
  
  # Données filtrées selon les inputs utilisateur et login validé
  filtered_data <- reactive({
    req(credentials()$user_auth)     # S'assure que utilisateur est connecté
    
    data <- heart_data               # Données complètes
    
    # Filtre sexe si sélection autre que "Tous"
    if (input$gender_filter != "all") data <- data %>% filter(gender == as.numeric(input$gender_filter))
    
    # Filtre tranche d'âge
    data <- data %>% filter(age >= input$age_range[1], age <= input$age_range[2])
    
    # Filtre résultat si sélection autre que "Tous"
    if (input$result_filter != "all") data <- data %>% filter(result == as.numeric(input$result_filter))
    
    data                            # Renvoie données filtrées
  })
  
  # Valeur box : pourcentage de patients positifs dans données filtrées
  output$percent_positive <- renderValueBox({
    valueBox(
      paste0(round(mean(filtered_data()$result, na.rm = TRUE) * 100, 1), "%"),  # Calcul % positifs
      "Patients à risque",
      icon = icon("heartbeat"),
      color = "red"
    )
  })
  
  # Valeur box : moyenne Troponine
  output$avg_troponin <- renderValueBox({
    valueBox(
      round(mean(filtered_data()$troponin, na.rm = TRUE), 2),  # Moyenne troponine
      "Troponine moyenne (ng/ml)",
      icon = icon("flask"),
      color = "blue"
    )
  })
  
  # Valeur box : âge moyen
  output$avg_age <- renderValueBox({
    valueBox(
      round(mean(filtered_data()$age, na.rm = TRUE), 1),       # Moyenne âge
      "Âge moyen",
      icon = icon("user"),
      color = "green"
    )
  })
  
  # Graphique principal (plotly) en fonction du choix de l'utilisateur
  output$main_plot <- renderPlotly({
    req(credentials()$user_auth)    # S'assurer que l'utilisateur est connecté
    
    # Scatter plot Age vs Troponine par résultat
    if (input$viz_select == "scatter_age_troponin") {
      plot_ly(
        data = filtered_data(),
        x = ~age, y = ~troponin, color = ~as.factor(result),
        colors = c("#4682B4", "#FF6F61"),           # Bleu et rouge clair
        type = "scatter", mode = "markers", marker = list(size = 10)
      ) %>%
        layout(title = "Âge vs Troponine par Risque de Crise Cardiaque",
               xaxis = list(title = "Âge (années)"),
               yaxis = list(title = "Troponine (ng/ml)"))
      
      # Bar chart proportion de résultats par sexe
    } else if (input$viz_select == "bar_gender_result") {
      plot_data <- filtered_data() %>%
        group_by(gender, result) %>%
        summarise(count = n(), .groups = "drop") %>%
        mutate(prop = count / sum(count) * 100)
      
      plot_ly(
        data = plot_data, x = ~gender, y = ~prop,
        color = ~as.factor(result), colors = c("#4682B4", "#FF6F61"),
        type = "bar"
      ) %>%
        layout(title = "Proportion de crises cardiaques par sexe",
               xaxis = list(title = "Sexe (0 = Femme, 1 = Homme)"),
               yaxis = list(title = "Pourcentage (%)"),
               barmode = "stack")
      
      # Histogramme des âges par sexe
    } else if (input$viz_select == "hist_age") {
      plot_ly(
        data = filtered_data(), x = ~age,
        color = ~as.factor(gender), colors = c("#4682B4", "#FF6F61"),
        type = "histogram", opacity = 0.5
      ) %>%
        layout(title = "Répartition des âges par sexe",
               xaxis = list(title = "Âge (années)"),
               yaxis = list(title = "Nombre"),
               barmode = "overlay")
      
      # Scatter plot CK-MB vs Troponine par résultat
    } else if (input$viz_select == "scatter_ckmb_troponin") {
      plot_ly(
        data = filtered_data(),
        x = ~ck_mb, y = ~troponin,
        color = ~as.factor(result),
        colors = c("#4682B4", "#FF6F61"),
        type = "scatter", mode = "markers",
        marker = list(size = 10)
      ) %>%
        layout(title = "CK-MB vs Troponine par Risque de Crise Cardiaque",
               xaxis = list(title = "CK-MB"),
               yaxis = list(title = "Troponine (ng/ml)"))
    }
  })
  
  # Tableau interactif avec les 6 premières lignes des données filtrées
  output$data_table <- DT::renderDataTable({
    req(credentials()$user_auth)    # Doit être connecté
    
    DT::datatable(
      head(filtered_data(), 6),     # 6 premières lignes
      extensions = 'Buttons',       # Boutons d'export (copy, csv, excel, pdf, print)
      options = list(
        dom = 'Bfrtip',             # Configuration DataTables (boutons + filtres)
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 6,             # Nombre de lignes affichées
        scrollX = TRUE,             # Scroll horizontal
        searching = FALSE,          # Désactivation recherche textuelle
        lengthChange = FALSE,       # Désactivation du choix du nb de lignes
        language = list(            # Traduction en français
          url = '//cdn.datatables.net/plug-ins/1.13.4/i18n/fr-FR.json'
        )
      )
    )
  })
  
  # ----- Partie PREDICTION ajoutée -----
  observeEvent(input$predict_btn, {
    new_patient <- tibble(
      age = input$pred_age,
      gender = as.numeric(input$pred_gender),
      heart_rate = input$pred_heart_rate,
      systolic_blood_pressure = input$pred_sbp,
      diastolic_blood_pressure = input$pred_dbp,
      blood_sugar = input$pred_blood_sugar,
      ck_mb = input$pred_ckmb,
      troponin = input$pred_troponin
    )
    
    prob <- predict(rf_model, new_patient, type = "prob")[,2] # Probabilité arrêt cardiaque
    pred <- predict(rf_model, new_patient)                   # Classe prédite (0 ou 1)
    
    output$prediction_result <- renderText({
      paste0("Probabilité estimée d'arrêt cardiaque : ", round(prob * 100, 2), "%\n\n",
             "Résultat : ", ifelse(pred == 1, "⚠️ Risque détecté ⚠️", "✅ Pas de risque détecté"))
    })
  })
}

# ----- Lancement de l'application -----
shinyApp(ui = ui, server = server)
