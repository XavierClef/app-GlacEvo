# Set-up ====

options(repos = c(CRAN = "https://cloud.r-project.org"))

packages <- c("shiny", "shinythemes", "bslib", "DT", "shinyglide", "scales",
              "leaflet", "tidyverse", "sf", "terra", "leaflet.extras",
              "shinyWidgets", "plotly", "cowplot", "grid", "shinydashboard",
              "viridis", "shinyjs", "leafem", "shinycssloaders")

packages_manquants <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(packages_manquants) > 0) {
  install.packages(packages_manquants)
}
lapply(packages, library, character.only = TRUE)

signed_sqrt_trans <- trans_new(
  name = "signed_sqrt",
  transform = function(x) sign(x) * sqrt(abs(x)),
  inverse = function(x) sign(x) * (x^2)
)

# UI part ====
ui <- page_navbar(
  theme = shinytheme("flatly"),
  title = HTML("<span style='padding-left: 15px;'><b><em>Glacier</em></b> <em>Evolution</em></span>"), 
  id = "nav",
  position = "fixed-top",
  
  nav_panel("Accueil",
            tags$head(
              tags$style(HTML("
      html, body {
        height: 100%;
        margin: 0;
      }
      .wrapper {
        min-height: 100%;
        display: flex;
        flex-direction: column;
      }
      .content {
        min-height: 100vh;
        flex: 1;
        padding: 15px;
      }
      .footer {
        height: 100px;
        width: 100%;
        background-color: #3f69b5;
        color: white;
        text-align: left;
        border-top: 3px solid #33418a;
      }
    "))
            ),
            
            div(class = "wrapper",
                div(class = "content",
                    style = "margin-left: 25px; margin-right: 25px;",
                    br(),
                    br(),
                    br(),
                    includeMarkdown("text_markdown/pres_app.Rmd"),
                    br(),
                    hr(),
                    div(
                      style = "margin-left: 25px; margin-right: 25px;",
                      h2("Des données issue de l'observation et de la modélisation"),
                      fluidRow(
                        style = "display: flex; align-items: stretch; min-height: 350px;",
                        column(8,
                               style = "display: flex; flex-direction: column;",
                               includeMarkdown("text_markdown/pres_data1.Rmd")
                        ),
                        column(4,
                               style = "display: flex; align-items: center;", 
                               div(
                                 style = "width: 100%;",
                                 tags$video(src = "movie_dhdt.mp4", type = "video/mp4", controls = TRUE, width = "100%"),
                                 div(
                                   style = "text-align: center; width: 100%;",
                                   em("Variation de l'altitude de surface des glaciers entre 2015 et 2020. Focus sur la région de la Mer de Glace")
                                 )
                               )
                        )
                      ),
                      hr(),
                      fluidRow(
                        style = "display: flex; align-items: stretch; min-height: 400px;", 
                        column(4,
                               style = "display: flex; align-items: center;",
                               div(
                                 style = "width: 100%;", 
                                 tags$video(src = "movie_tck.mp4", type = "video/mp4", controls = TRUE, width = "100%"),
                                 div(
                                   style = "text-align: center; width: 100%;",
                                   em("Epaisseur des glaciers en 2020. Focus sur la région de la Mer de Glace")
                                 )
                               )
                        ),
                        column(8,
                               style = "display: flex; flex-direction: column;",
                               includeMarkdown("text_markdown/pres_data2.Rmd")
                        )
                      ),
                      hr(),
                      fluidRow(
                               includeMarkdown("text_markdown/pres_data3.Rmd")
                        
                      )
                    )
                )),
            
            div(class = "footer",
                style = "display: grid; grid-template-columns: 1fr 2fr; align-items: center; ",
                div(
                  img(src = "IGE_logo-removebg-preview.png", height = "80px", alt = "IGE Logo"),
                  style = "display: flex; justify-content: right;"
                ),
                div(
                  p("Application produite par Xavier Klee"),
                  div(
                    p(
                      "Contacts : ",
                      HTML("<a href='https://www.linkedin.com/in/xavier-klee-480094272' target='_blank'><i class='fa fa-linkedin fa-lg' style='margin-right: 10px;'></i></a>
                            <a href='https://github.com/XavierClef' target='_blank'><i class='fa fa-github fa-lg'></i></a>")
                    )
                  ),
                  style = "padding-left: 20px;"
                )
            )
            
  ),
  
  nav_panel("1. Sélection des glaciers",
            useShinyjs(),
            div(class = "outer",
                tags$head(
                  includeCSS("www/styles.css")
                ),
                withSpinner(leafletOutput("map", width = "100%", height = "100vh"), color = "#164f27"),
                absolutePanel(
                  style = "overflow-y: auto; height: 90vh;",
                  id = "controls", fixed = TRUE,
                  top = 75, left = "auto", right = 20, bottom = "auto",
                  width = 400, height = "auto",
                  h2("Sélection des glaciers"),
                  hr(),
                  div(
                    class = "d-flex justify-content-center",
                    radioGroupButtons(
                      inputId = "rast_t", 
                      label = "Choix de la trajectoire à afficher :", 
                      choices = c(
                        "Trajectoire optimiste" = "data/thk0050_to.tif",
                        "Trajectoire intermédiaire" = "data/thk0050_ti.tif",
                        "Trajectoire pessimiste" = "data/thk0050_tp.tif"
                      ),
                      selected = "data/thk0050_ti.tif", 
                      direction = "vertical",  
                      justified = TRUE,
                      status = "primary"
                    )
                  ),
                  div(
                    class = "d-flex justify-content-center",
                    sliderInput("rast_year", "Année à afficher :",
                                min = 2000, max = 2050, value = 2025, sep = "",
                                width = "100%")
                  ),
                  h5("Epaisseur (m) :"),
                  plotOutput("legend_plot", height = "50px"),
                  hr(),
                  div(
                    class = "d-flex justify-content-center",
                    radioGroupButtons(
                      inputId = "select_param",
                      label = "Choix du mode de sélection :",
                      choices = c(
                        "Dessiner sur la carte" = "draw",
                        "Importation" = "upload"
                      ),
                      selected = "draw", 
                      direction = "vertical",  
                      justified = TRUE,
                      status = "primary"
                    )),
                  
                  hidden(
                    div(id = "upload_container",
                        fileInput("file_upload", "Importer un fichier",
                                  accept = c(".gpkg", ".kml", ".shp", ".geojson")),
                        p("Pour les fichiers Shapefile (.shp), assurez-vous d'inclure tous les fichiers associés (.dbf, .prj, etc.)"),
                        br()
                    )
                  ),
                  div(
                    style = "width: 100%",  # Pour s'assurer que le conteneur prend toute la largeur
                    div(
                      class = "text-center mb-3", # Centre les boutons
                      actionButton("update_polygon", "Valider la sélection", class = "btn btn-primary me-2"),  # me-2 ajoute une marge à droite
                      actionButton("clear_polygons", "Effacer la sélection", class = "btn btn-danger")
                    ),
                    br(),
                    em(icon("triangle-exclamation"),"Une grande sélection implique un temps de chargement plus long !")
                  )
                )
            )
  ),
  
  nav_panel("2. Visualisation",
            br(), 
            br(),
            br(),
            hidden(
              div(id = "warn_container",
                  class = "custom-alert",
                  style = "
    top: 50%;
    left: 100%;
    font-size: 30px;
    text-align: center;
    padding: 20px;
    color: orange;
    font-weight: bold;
  ",
                  icon("exclamation-triangle", lib = "font-awesome"),"Veuillez sélectionner au moins un glacier ou vous assurer que la géometrie du polygone de sélection est valide"
              )
            ),
            div(
              style = "margin-left: 25px; margin-right: 25px;",
              h2("Visualisation"),
              hr(),
              h3("Récapitulatif de la séléction"),
              fluidRow(
                column(4,
                       card(
                         height = "auto",
                         card_header(h3()),
                         card_body(
                           class = "p-0",
                           withSpinner(leafletOutput("mapplotRecap"), color = "#164f27")
                         )
                       )
                ),
                column(8,
                       fluidRow(
                         uiOutput("n_glac")
                       ),
                       fluidRow(
                         style = "display: flex; align-items: center; gap: 20px; margin-top: 20px;",
                         div(
                           style = "flex: 1;",
                           h4(style = "margin: 0; text-align: center;", "Pour la trajectoire optimiste :")
                         ),
                         div(
                           style = "flex: 1;",
                           uiOutput("v_loss_TO")
                         ),
                         div(
                           style = "flex: 1;",
                           uiOutput("c_loss_TO")
                         )
                       ),
                       fluidRow(
                         style = "display: flex; align-items: center; gap: 20px; margin-top: 20px;",
                         div(
                           style = "flex: 1;",
                           h4(style = "margin: 0; text-align: center;", "Pour la trajectoire intermédiaire :")
                         ),
                         div(
                           style = "flex: 1;",
                           uiOutput("v_loss_TI")
                         ),
                         div(
                           style = "flex: 1;",
                           uiOutput("c_loss_TI")
                         )
                       ),
                       fluidRow(
                         style = "display: flex; align-items: center; gap: 20px; margin-top: 20px;",
                         div(
                           style = "flex: 1;",
                           h4(style = "margin: 0; text-align: center;", "Pour la trajectoire pessimiste :")
                         ),
                         div(
                           style = "flex: 1;",
                           uiOutput("v_loss_TP")
                         ),
                         div(
                           style = "flex: 1;",
                           uiOutput("c_loss_TP")
                         )
                       )
                )
              ),
              hr(),
              h3("Visualisation graphique"),
              radioGroupButtons(
                "scenar",
                "Choix du scénario :",
                choices = c(
                  "Trajectoire optimiste" = "TO",
                  "Trajectoire intermédiaire" = "TI",
                  "Trajectoire pessimiste" = "TP"
                ),
                selected = c("Trajectoire intermédiaire" = "TI"), 
                direction = "horizontal",  
                justified = TRUE,
                status = "primary"
              ),
              radioGroupButtons(
                "var",
                "Choix de la variable à afficher :",
                choices = c(
                  "volume" = "v",
                  "Contribution annuelle des glaciers" = "c"
                ),
                selected = c("volume" = "v"), 
                direction = "horizontal",  
                justified = TRUE,
                status = "primary"
              ),
              glide(
                id = "Glide",
                next_label = "Vers la table",
                previous_label = "Vers la carte",
                screen(
                  card(
                    height = "700px",
                    style = "width: 100%;", 
                    card_header(h4(strong("Pourcentage de perte d'épaisseur entre 2000 et 2050"))),
                    card_body(
                      class = "p-1",
                      withSpinner(leafletOutput("mapplotPourcDelta", height = "650px"), color = "#164f27")
                    )
                  )
                ),
                screen(
                  card(
                    height = "700px",
                    style = "width: 100%;", 
                    card_header(h4(strong("Table de la séléction :"))),
                    card_body(
                      class = "p-1",
                      withSpinner(DTOutput("table"), color = "#164f27")
                    )
                  )
                  
                )
              ),
              hr(),
              column(1, 
                     style = "display: flex; align-items: flex-start; justify-content: center; padding-top: 150px;",
                     h2(em("Période 2000 - 2019"), 
                        style = "transform: rotate(270deg); white-space: nowrap; transform-origin: center;")
              ),
              column(11,
                     column(4,
                            card(
                              height = "550px",
                              style = "width: 100%;", 
                              card_header(h4(strong(textOutput("title_line_obs")))),
                              card_body(
                                class = "p-1",
                                plotlyOutput("plot_line_obs")
                              )
                            )
                     ),
                     column(4,
                            card(
                              height = "550px",
                              style = "width: 100%;", 
                              card_header(h4(strong(textOutput("title_stack_obs")))),
                              card_body(
                                class = "p-1",
                                plotlyOutput("plot_stack_obs")
                              )
                            )
                     ),
                     column(4,
                            card(
                              height = "550px",
                              style = "width: 100%;", 
                              card_header(h4(strong(textOutput("title_trend_obs")))),
                              card_body(
                                class = "p-1",
                                plotlyOutput("plot_trend_obs")
                              )
                            )
                     )
              ),
              column(1, 
                     style = "display: flex; align-items: flex-start; justify-content: center; padding-top: 150px;",
                     h2(em("Période 2020 - 2050"), 
                        style = "transform: rotate(270deg); white-space: nowrap; transform-origin: center;")
              ),
              column(11,
                     column(4,
                            card(
                              height = "500px",
                              style = "width: 100%;", 
                              card_body(
                                class = "p-1",
                                plotlyOutput("plot_line_mod")
                              )
                            )
                     ),
                     column(4,
                            card(
                              height = "500px",
                              style = "width: 100%;", 
                              card_body(
                                class = "p-1",
                                plotlyOutput("plot_stack_mod")
                              )
                            )
                     ),
                     column(4,
                            card(
                              height = "500px",
                              style = "width: 100%;", 
                              card_body(
                                class = "p-1",
                                plotlyOutput("plot_trend_mod")
                              )
                            )
                     )
              )
            )
  ),
  
  nav_panel("Téléchargement",
            withSpinner(div(
              style = "margin-left: 25px; margin-right: 25px;",
              fluidPage(
                br(),
                br(),
                br(),
                h1(strong("Téléchargement des données")),
                hr(),
                fluidRow(
                  p("Les données peuvent être téléchargées directement dans cette interface. 
     Il est possible de choisir entre les données brutes de tout l'arc alpin ou pour une sélection réalisée dans la section 1. 
     Les données sont disponibles pour la série temporelle 2000-2050. La partie 2020-2050, issue de la modélisation, est fournie pour toutes les trajectoires.",
                    style = "line-height: 1.5; margin-bottom: 10px; margin-left: 50px; margin-right: 50px;"),
                  
                  div(
                    style = "padding: 15px; border-radius: 6px; margin-bottom: 10px; margin-left: 50px;",
                    tags$p(tags$em("Les scénarios sont indiqués par des abréviations :"), style = "font-weight: bold; margin-bottom: 10px;"),
                    tags$p(tags$span("TO", style = "color: #0f9d58; font-weight: bold;"), " pour trajectoire optimiste", style = "margin: 5px 0;"),
                    tags$p(tags$span("TI", style = "color: #f4b400; font-weight: bold;"), " pour trajectoire intermédiaire", style = "margin: 5px 0;"),
                    tags$p(tags$span("TP", style = "color: #db4437; font-weight: bold;"), " pour trajectoire pessimiste", style = "margin: 5px 0;")
                  ),
                  column(6,
                         h3(strong("Les données brutes")),
                         leafletOutput("mapplot_raw", width = "47vw", height = "70vh"),
                         br(),
                         div(
                           radioGroupButtons(
                             inputId = 'raw', 
                             label = "Sélection du type d'export",
                             choices = setNames(
                               c('rast1', 'vec1'), 
                               c("Raster d'épaisseur", "Glaciers")
                             ),
                             selected = "rast1",
                             checkIcon = list(yes = icon("check")),
                             status = 'primary',
                             justified = TRUE
                           ),
                           hidden(
                             div(
                               id = "format_selector1",
                               radioGroupButtons(
                                 inputId = 'format_selected1', 
                                 label = "Formats de l'export :",
                                 choices = c("csv",
                                             "gpkg"),
                                 selected = "gpkg",
                                 checkIcon = list(
                                   yes = icon("check")
                                 ),
                                 status = 'primary',
                                 justified = TRUE
                               )
                             )
                           ),
                           div(style="display: flex; justify-content: center; align-items: center;",
                               downloadBttn("dl_raw", 
                                            label = 'Téléchargement des données', 
                                            style = 'fill',
                                            color = 'success',
                                            icon = icon("file-zipper")
                               )
                           ),
                           br()
                         )
                  ),
                  column(6,
                         h3(strong("Les données sélectionnées")),
                         uiOutput("dl_content")
                  )
                )
                
              )
            ), color = "#164f27")
  )
)



server <- function(input, output, session) {
  
  observe({
    if (input$select_param == "upload") {
      show("upload_container", anim = TRUE)
    } else {
      hide("upload_container", anim = TRUE)
    }
  })
  
  glac <- read_sf("data/RGI_V6_comp.gpkg") %>% 
    st_transform(4326)
  
  glac_centro <- read_sf("data/RGI_V6_comp_centro.gpkg")%>% 
    st_transform(4326)
  
  selected_thk <- reactive({
    thk_select <- rast(input$rast_t, lyrs = as.character(input$rast_year))  
    
    return(thk_select)
  })
  
  pal <- colorNumeric(palette = turbo(256), domain = c(1, 800), na.color = "transparent")
  img <- "https://ige-vis.univ-grenoble-alpes.fr/images/ige.svg"
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addRasterImage(
        selected_thk(), 
        colors = pal,  
        project = TRUE
      ) %>%
      addPolygons(data = glac, color = "red", weight = 2, fillOpacity = 0,
                  label = ~glac_nom) %>%
      setView(lng = 10.8, lat = 46.2, zoom = 8) %>% 
      addLogo(img, url = "https://www.ige-grenoble.fr/") %>% 
      addDrawToolbar(
        targetGroup = "draw",
        polygonOptions = drawPolygonOptions(repeatMode = FALSE),
        polylineOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        circleOptions = FALSE,
        editOptions = editToolbarOptions(
          selectedPathOptions = selectedPathOptions()
        )
      )
  })
  
  temp_polygons <- reactiveVal(list())
  drawn_polygon <- reactiveVal() 
  
  observeEvent(input$select_param, {
    if (input$select_param == "upload") {
      leafletProxy("map") %>%
        removeDrawToolbar() %>%
        clearGroup("draw") %>%
        clearGroup("temp_polygons")
      
      temp_polygons(list())
      drawn_polygon(NULL)
    } else {
      leafletProxy("map") %>%
        addDrawToolbar(
          targetGroup = "draw",
          polygonOptions = drawPolygonOptions(repeatMode = FALSE),
          polylineOptions = FALSE,
          markerOptions = FALSE,
          circleMarkerOptions = FALSE,
          circleOptions = FALSE,
          editOptions = editToolbarOptions(
            selectedPathOptions = selectedPathOptions()
          )
        )
    }
  })
  
  observeEvent(input$map_draw_new_feature, {
    if (input$select_param == "draw") {  # Vérifier si on est en mode dessin
      feature <- input$map_draw_new_feature
      
      if (feature$geometry$type == "Polygon") {
        coords <- lapply(feature$geometry$coordinates[[1]], function(coord) {
          c(as.numeric(coord[1]), as.numeric(coord[2]))
        })
        
        coords_matrix <- do.call(rbind, coords)
        
        if (all(is.numeric(coords_matrix)) && ncol(coords_matrix) == 2) {
          tryCatch({
            new_polygon <- st_sf(
              geometry = st_sfc(
                st_polygon(
                  list(coords_matrix)
                ),
                crs = 4326
              ) %>% 
                st_buffer(0)
            )
            
            current_polygons <- temp_polygons()
            temp_polygons(c(current_polygons, list(new_polygon)))
            
          }, error = function(e) {
            warning("Erreur lors de la création du polygone SF: ", e$message)
          })
        }
      }
    }
  })
  
  observeEvent(input$update_polygon, {
    if (input$select_param == "draw") {
      polygons <- temp_polygons()
      if (length(polygons) > 0) {
        tryCatch({
          geometries <- lapply(polygons, function(p) st_geometry(p)[[1]])
          
          multipolygon <- st_sf(
            geometry = st_sfc(
              st_multipolygon(
                lapply(geometries, function(g) list(st_coordinates(g)[,1:2]))
              ),
              crs = 4326
            ) %>% 
              st_buffer(0)
          )
          
          drawn_polygon(multipolygon)
          
        }, error = function(e) {
          warning("Erreur lors de la création du multipolygone: ", e$message)
        })
      }
    }
  })
  
  observeEvent(input$clear_polygons, {
    if (input$select_param == "draw") {  # Vérifier si on est en mode dessin
      temp_polygons(list())  
      drawn_polygon(NULL)    
      
      leafletProxy("map") %>%
        clearGroup("draw") %>%
        clearGroup("temp_polygons") 
    }
  })
  
  observe({
    if (isTRUE(input$select_param == "draw")) {
      polygons <- temp_polygons()
      if (length(polygons) > 0) {
        leafletProxy("map") %>%
          clearGroup("temp_polygons") %>%
          addPolygons(
            data = do.call(rbind, polygons),
            group = "temp_polygons",
            color = "darkgreen",
            weight = 2,
            opacity = 0.8,
            fillOpacity = 0.3
          )
      }
    } else {
      # Check if a file has been uploaded
      if (!is.null(input$file_upload)) {
        polygons <- st_read(input$file_upload) %>% 
          st_transform(4326)
        if (length(polygons) > 0) {
          leafletProxy("map") %>%
            clearGroup("temp_polygons") %>%
            addPolygons(
              data = polygons,
              group = "temp_polygons",
              color = "darkgreen",
              weight = 2,
              opacity = 0.8,
              fillOpacity = 0.3
            )
        }
      }
    }
  })
  
  output$legend_plot <- renderPlot({
    df <- data.frame(y = seq(1, 800, length.out = 100), x = 1)
    
    plot <- ggplot(df, aes(x = y, y = 1, fill = y)) +
      geom_tile() +
      scale_fill_viridis(option = "turbo", limits = c(1, 800), oob = scales::squish, 
                         guide = guide_colorbar(
                           barwidth = 16, barheight = 2,  
                           label.theme = element_text(size = 16)  
                         )) + 
      theme(legend.position = "bottom",
            legend.title = element_blank())
    
    g <- ggplotGrob(plot)
    legend <- g$grobs[grepl("guide-box", sapply(g$grobs, function(x) x$name))][[1]]
    
    grid.newpage()
    grid.draw(legend)
  })
  
  df_spatial <- reactive({
    req(drawn_polygon())  
    
    glac_inter <- st_intersection(glac_centro, drawn_polygon())
    glac_filtered <- glac %>%
      filter(glac_nom %in% glac_inter$glac_nom)
    
    return(glac_filtered)
  })
  
  df_plot <- reactive({
    req(df_spatial())
    
    df <- df_spatial() %>% 
      st_set_geometry(NULL) %>%
      pivot_longer(cols = starts_with(c("v.", "c.")), names_to = "variable", values_to = "value") %>%
      mutate(
        type = sub("^([vc])\\..*", "\\1", variable),
        year = sub("^([vc])\\.(\\d{4}).*", "\\2", variable),
        scenario = sub("^([vc])\\.(\\d{4})_(.*)$", "\\3", variable)
      ) %>%
      select(-variable) %>% 
      mutate(year = as.numeric(year)) %>% 
      mutate(across(everything(), function(x){replace(x, which(x<0), 0)}))
    
    return(df)
  })
  
  observe({
    if (length(drawn_polygon()) == 0) {
      show("warn_container")
    } else {
      hide("warn_container")
    }
  })
  
  output$mapplotRecap <- renderLeaflet({
    req(df_spatial(), drawn_polygon())
    
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$CartoDB.VoyagerOnlyLabels) %>% 
      addPolygons(data = drawn_polygon(),
                  fillOpacity = 0,
                  weight = 2,
                  color = "black") %>% 
      addPolygons(data = df_spatial(),  
                  fillColor = "#2f69b5",
                  fillOpacity = 1,
                  color = "#3854a6",
                  stroke = TRUE,
                  weight = 1,
                  label = ~glac_nom) 
  })
  
  output$n_glac <- renderUI({
    if (length(drawn_polygon()) == 0) {
      value_box(
        style = 'background-color: #2f69b5!important; color: white;',
        title = HTML("<span style='font-size: 15px;'>Nombre de glaciers</span>"),
        value = "0",
        showcase = icon("mountain"),
      )
    } else {
      req(df_spatial())
      value_box(
        style = 'background-color: #2f69b5!important; color: white;',
        title = HTML("<span style='font-size: 15px;'>Nombre de glaciers</span>"),
        value = as.character(nrow(df_spatial())),
        showcase = icon("mountain"),
      )
    }
  })
  
  output$v_loss_TO <- renderUI({
    if (length(drawn_polygon()) == 0) {
      value_box(
        height = "100px",
        style = 'background-color: darkred!important; color: white;',
        title = HTML("<span style='font-size: 15px;'>volume perdu entre 2000 et 2050</span>"),
        value = "0 %",
        showcase = icon("cube"),
      )
    } else {
      req(df_plot())
      
      df <- df_plot() %>% 
        filter(type== "v"& scenario == "TO")
      
      v_2000 <- df %>% 
        filter(year == 2000)
      
      v_2050 <- df %>% 
        filter(year == 2050)
      
      volume_loss <- round(((sum(v_2000$value) - sum(v_2050$value)) / sum(v_2000$value) * 100), digits = 0)
      
      value_box(
        height = "100px",
        style = 'background-color: darkred!important; color: white;',
        title = HTML("<span style='font-size: 15px;'>volume perdu entre 2000 et 2050</span>"),
        value = paste0(volume_loss, " %"),
        showcase = icon("cube"),
      )
    }
  })
  
  output$c_loss_TO <- renderUI({
    if (length(drawn_polygon()) == 0) {
      value_box(
        height = "100px",
        style = 'background-color: orange!important; color: white;',
        title = HTML("<span style='font-size: 15px;'>Contribution moyenne annuelle de la variation de volume glaciaire</span>"),
        value = "0 ls",
        showcase = icon("faucet-drip"),
      )
    } else {
      req(df_plot())
      
      df <- df_plot() %>% 
        filter(type== "c"& scenario == "TO")
      
      c_2001 <- df %>% 
        filter(year == 2000)
      
      c_2050 <- df %>% 
        filter(year == 2050)
      
      c_loss <- round(sum(c_2001$value) - sum(c_2050$value), digits = 0)
      
      value_box(
        height = "100px",
        style = 'background-color: orange!important; color: white;',
        title = HTML("<span style='font-size: 15px;'>Contribution moyenne annuelle de la variation de volume glaciaire</span>"),
        value = paste0(c_loss, " l/s"),
        showcase = icon("faucet-drip"),
      )
    }
  })
  
  output$v_loss_TI <- renderUI({
    if (length(drawn_polygon()) == 0) {
      value_box(
        height = "100px",
        style = 'background-color: darkred!important; color: white;',
        title = HTML("<span style='font-size: 15px;'>volume perdu entre 2020 et 2050</span>"),
        value = "0 %",
        showcase = icon("cube"),
      )
    } else {
      req(df_plot())
      
      df <- df_plot() %>% 
        filter(type == "v" & scenario == "TI")
      
      v_2000 <- df %>% 
        filter(year == 2000)
      
      v_2050 <- df %>% 
        filter(year == 2050)
      
      volume_loss <- round(((sum(v_2000$value) - sum(v_2050$value)) / sum(v_2000$value) * 100), digits = 0)
      
      value_box(
        height = "100px",
        style = 'background-color: darkred!important; color: white;',
        title = HTML("<span style='font-size: 15px;'>volume perdu entre 2000 et 2050</span>"),
        value = paste0(volume_loss, " %"),
        showcase = icon("cube"),
      )
    }
  })
  
  output$c_loss_TI <- renderUI({
    if (length(drawn_polygon()) == 0) {
      value_box(
        height = "100px",
        style = 'background-color: orange!important; color: white;',
        title = HTML("<span style='font-size: 15px;'>Contribution moyenne annuelle de la variation de volume glaciaire</span>"),
        value = "0 ls",
        showcase = icon("faucet-drip"),
      )
    } else {
      req(df_plot())
      
      df <- df_plot() %>% 
        filter(type== "c"& scenario == "TI")
      
      c_2001 <- df %>% 
        filter(year == 2000)
      
      c_2050 <- df %>% 
        filter(year == 2050)
      
      c_loss <- round(sum(c_2001$value) - sum(c_2050$value), digits = 0)
      
      value_box(
        height = "100px",
        style = 'background-color: orange!important; color: white;',
        title = HTML("<span style='font-size: 15px;'>Contribution moyenne annuelle de la variation de volume glaciaire</span>"),
        value = paste0(c_loss, " l/s"),
        showcase = icon("faucet-drip"),
      )
    }
  })
  
  output$v_loss_TP <- renderUI({
    if (length(drawn_polygon()) == 0) {
      value_box(
        height = "100px",
        style = 'background-color: darkred!important; color: white;',
        title = HTML("<span style='font-size: 15px;'>volume perdu entre 2000 et 2050</span>"),
        value = "0 %",
        showcase = icon("cube"),
      )
    } else {
      req(df_plot())
      
      df <- df_plot() %>% 
        filter(type== "v"& scenario == "TP")
      
      v_2000 <- df %>% 
        filter(year == 2000)
      
      v_2050 <- df %>% 
        filter(year == 2050)
      
      volume_loss <- round(((sum(v_2000$value) - sum(v_2050$value)) / sum(v_2000$value) * 100), digits = 0)
      
      value_box(
        height = "100px",
        style = 'background-color: darkred!important; color: white;',
        title = HTML("<span style='font-size: 15px;'>volume perdu entre 2000 et 2050</span>"),
        value = paste0(volume_loss, " %"),
        showcase = icon("cube"),
      )
    }
  })
  
  output$c_loss_TP <- renderUI({
    if (length(drawn_polygon()) == 0) {
      value_box(
        height = "100px",
        style = 'background-color: orange!important; color: white;',
        title = HTML("<span style='font-size: 15px;'>Contribution moyenne annuelle de la variation de volume glaciaire</span>"),
        value = "0 ls",
        showcase = icon("faucet-drip"),
      )
    } else {
      req(df_plot())
      
      df <- df_plot() %>% 
        filter(type== "c"& scenario == "TP")
      
      c_2001 <- df %>% 
        filter(year == 2000)
      
      c_2050 <- df %>% 
        filter(year == 2050)
      
      c_loss <- round(sum(c_2001$value) - sum(c_2050$value), digits = 0)
      
      value_box(
        height = "100px",
        style = 'background-color: orange!important; color: white;',
        title = HTML("<span style='font-size: 15px;'>Contribution moyenne annuelle de la variation de volume glaciaire</span>"),
        value = paste0(c_loss, " l/s"),
        showcase = icon("faucet-drip"),
      )
    }
  })
  
  output$table <- renderDT({
    req(df_spatial())
    
    df <- df_spatial() %>%
      st_drop_geometry() %>%
      select(c(glac_nom, starts_with(as.character(input$var)))) %>%
      select(c(glac_nom, ends_with(input$scenar))) %>% 
      mutate(across(where(is.numeric), round, digits= 2)) %>% 
      rename(Glacier = glac_nom)
    
    DT::datatable(df,
                  selection = 'none',
                  extensions = 'Scroller',
                  options = list(
                    scroller = TRUE,
                    scrollX = TRUE,
                    scrollY = 500))
  })
  
  suffix <- reactive({
    switch(input$scenar,
           "TO" = "to",
           "TI" = "ti",
           "TP" = "tp")
  })
  
  output$mapplotPourcDelta <- renderLeaflet({
    req(df_spatial(), suffix())
    
    suffix <- suffix()
    
    thk_path <- paste0("~/IGE_app/IGE_shinyapp/data/thkdelta_",suffix,".tif")
    
    thk_delta <- rast(thk_path)
    thk_delta <- mask(thk_delta, df_spatial())
    
    thk_delta[thk_delta < 0] <- 0
    legend_values <- list(
      min = 0,
      max = 100
    )
    
    pal <- colorNumeric(palette = turbo(100),
                        domain = c(0, 100),
                        na.color = "transparent")
    
    bbox_list <- as.list(st_bbox(df_spatial()))
    center_lng <- (bbox_list$xmin + bbox_list$xmax) / 2
    center_lat <- (bbox_list$ymin + bbox_list$ymax) / 2
    
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>% 
      addRasterImage(thk_delta,
                     colors = pal,
                     group = "Delta",
                     layerId = "delta") %>% 
      addPolygons(data = df_spatial(),  
                  fillColor = NA,
                  fillOpacity = 0,
                  color = "lightgray",
                  stroke = TRUE,
                  weight = 1,
                  label = ~glac_nom) %>% 
      addLegend(position = "bottomright",
                pal = pal,
                values = unlist(legend_values),
                title = paste0("Pourcentage de perte (%)"),
                opacity = 1) %>%
      addFullscreenControl() %>%
      fitBounds(
        lng1 = st_bbox(df_spatial())[["xmin"]],
        lat1 = st_bbox(df_spatial())[["ymin"]],
        lng2 = st_bbox(df_spatial())[["xmax"]],
        lat2 = st_bbox(df_spatial())[["ymax"]]
      )
  })
  
  output$title_line_obs <- renderText({
    if (input$var == "v") {
      "Evolution du volume par glacier"
    } else {
      "Evolution de la contribution moyenne annuelle par glacier"
    }
  })
  
  output$plot_line_obs <- renderPlotly({
    req(df_plot())
    
    df <- df_plot() %>% 
      filter(type == as.character(input$var),
             scenario == as.character(input$scenar)) %>% 
      filter(year %in% if (input$var == "v") 2000:2019 else 2001:2019)
    
    plot <- df %>%
      ggplot(aes(text = glac_nom,  
                 x = year, 
                 y = value,
                 color = glac_nom,
                 group = glac_nom)) +
      geom_point(shape = 3) +
      geom_line() +
      labs(
        x = "Année",
        y = if(input$var == "v") {
          "Volume (m3)"
        } else {
          "Contribution annuelle des glaciers (l/s)"
        }
      ) +
      theme_classic() +
      theme(
        legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_x_continuous(n.breaks = 11) +
      scale_colour_viridis(discrete = TRUE, option = "H")
    
    ggplotly(plot, tooltip = c("text", "x", "y")) %>%
      layout(showlegend = FALSE)%>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                        "select2d", "lasso2d", 
                                        "pan2d", "resetScale2d",
                                        "hoverCompareCartesian"))
  })
  
  output$title_stack_obs <- renderText({
    if (input$var == "v") {
      "Evolution cumulée du volume"
    } else {
      "Evolution de la contribution annuelle cumulée"
    }
  })
  
  output$plot_stack_obs <- renderPlotly({
    req(df_plot())
    
    df <- df_plot() %>% 
      filter(type == as.character(input$var)
             & scenario == as.character(input$scenar))  %>% 
      filter(year %in% if (input$var == "v") 2000:2019 else 2001:2019)
    
    plot <- df %>%
      ggplot(aes(text = glac_nom,  
                 x = year, 
                 y = value,
                 fill = glac_nom,
                 group = glac_nom)) +
      geom_bar(position = "stack", stat = "identity") +
      labs(
        x = "Année",
        y = if(input$var == "v") {
          "Volume (m3)"
        } else {
          "Contribution annuelle des glaciers (l/s)"
        }
      ) +
      theme_classic() +
      theme(
        legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_x_continuous(n.breaks = 11) +
      scale_fill_viridis(discrete = TRUE, option = "H")
    
    ggplotly(plot, tooltip = c("text", "x", "y")) %>%
      layout(showlegend = FALSE)%>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                        "select2d", "lasso2d", 
                                        "pan2d", "resetScale2d",
                                        "hoverCompareCartesian"))
  })
  
  output$title_trend_obs <- renderText({
    if (input$var == "v") {
      "Evolution du volume annuel par glacier"
    } else {
      "Evolution de la contribution moyenne annuelle par glacier"
    }
  })
  
  output$plot_trend_obs <- renderPlotly({
    req(df_plot())
    
    df_reg <- df_plot() %>% 
      filter(type == input$var) %>% 
      filter(year %in% if (input$var == "v") 2000:2019 else 2001:2019) %>% 
      group_by(glac_nom) %>% 
      summarise(trend = coef(lm(value ~ year))[2], .groups = "drop")
    
    df_mean <- df_reg %>% 
      group_by(glac_nom) %>% 
      summarise(mean_trend = mean(trend))
    
    plot <- df_reg %>% 
      ggplot(aes(x = glac_nom, 
                 y = trend,
                 color = glac_nom,
                 text = glac_nom, 
                 group = glac_nom)) +
      geom_hline(data = df_reg, aes(yintercept = mean(trend)), linetype = 'dashed', color = 'black') +
      geom_jitter() +
      labs(
        x = "",
        y = if(input$var == "v") {
          "Taux d'évolution annuel moyen du volume (m3/an)"
        } else {
          "Taux d'évolution de la contribution annuelle ((l/s)/an)"
        }
      ) +
      theme_classic() +
      theme(
        legend.position = 'none',
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      scale_y_continuous(trans = signed_sqrt_trans) +
      scale_colour_viridis(discrete = TRUE, option = "H")
    
    ggplotly(plot, tooltip = c("text", "y")) %>%
      layout(showlegend = FALSE) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                        "select2d", "lasso2d", 
                                        "pan2d", "resetScale2d",
                                        "hoverCompareCartesian"))
  })
  
  output$plot_line_mod <- renderPlotly({
    req(df_plot())
    
    df <- df_plot() %>% 
      filter(type == as.character(input$var),
             scenario == as.character(input$scenar)) %>% 
      filter(year %in% if (input$var == "v") 2020:2050 else 2021:2050)
    
    plot <- df %>%
      ggplot(aes(text = glac_nom,  
                 x = year, 
                 y = value,
                 color = glac_nom,
                 group = glac_nom)) +
      geom_point(shape = 3) +
      geom_line() +
      labs(
        x = "Année",
        y = if(input$var == "v") {
          "Volume (m3)"
        } else {
          "Contribution annuelle des glaciers (l/s)"
        }
      ) +
      theme_classic() +
      theme(
        legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_x_continuous(n.breaks = 11) +
      scale_colour_viridis(discrete = TRUE, option = "H")
    
    ggplotly(plot, tooltip = c("text", "x", "y")) %>%
      layout(showlegend = FALSE)%>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                        "select2d", "lasso2d", 
                                        "pan2d", "resetScale2d",
                                        "hoverCompareCartesian"))
  })
  
  output$plot_stack_mod <- renderPlotly({
    req(df_plot())
    
    df <- df_plot() %>% 
      filter(type == as.character(input$var)
             & scenario == as.character(input$scenar))  %>% 
      filter(year %in% if (input$var == "v") 2020:2050 else 2021:2050)
    
    plot <- df %>%
      ggplot(aes(text = glac_nom,  
                 x = year, 
                 y = value,
                 fill = glac_nom,
                 group = glac_nom)) +
      geom_bar(position = "stack", stat = "identity") +
      labs(
        x = "Année",
        y = if(input$var == "v") {
          "Volume (m3)"
        } else {
          "Contribution annuelle des glaciers (l/s)"
        }
      ) +
      theme_classic() +
      theme(
        legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_x_continuous(n.breaks = 11) +
      scale_fill_viridis(discrete = TRUE, option = "H")
    
    ggplotly(plot, tooltip = c("text", "x", "y")) %>%
      layout(showlegend = FALSE)%>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                        "select2d", "lasso2d", 
                                        "pan2d", "resetScale2d",
                                        "hoverCompareCartesian"))
  })
  
  output$plot_trend_mod <- renderPlotly({
    req(df_plot())
    
    df_reg <- df_plot() %>% 
      filter(type == input$var) %>% 
      filter(year %in% if (input$var == "v") 2020:2050 else 2021:2050) %>% 
      group_by(glac_nom) %>% 
      summarise(trend = coef(lm(value ~ year))[2], .groups = "drop")
    
    df_mean <- df_reg %>% 
      group_by(glac_nom) %>% 
      summarise(mean_trend = mean(trend))
    
    plot <- df_reg %>% 
      ggplot(aes(x = glac_nom, 
                 y = trend,
                 color = glac_nom,
                 text = glac_nom, 
                 group = glac_nom)) +
      geom_hline(data = df_reg, aes(yintercept = mean(trend)), linetype = 'dashed', color = 'black') +
      geom_jitter() +
      labs(
        x = "",
        y = if(input$var == "v") {
          "Taux d'évolution annuel moyen du volume (m3/an)"
        } else {
          "Taux d'évolution de la contribution annuelle ((l/s)/an)"
        }
      ) +
      theme_classic() +
      theme(
        legend.position = 'none',
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      scale_y_continuous(trans = signed_sqrt_trans) +
      scale_colour_viridis(discrete = TRUE, option = "H")
    
    ggplotly(plot, tooltip = c("text", "y")) %>%
      layout(showlegend = FALSE) %>% 
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                        "select2d", "lasso2d", 
                                        "pan2d", "resetScale2d",
                                        "hoverCompareCartesian"))
  })
  
  output$mapplot_raw <- renderLeaflet({
    leaflet(glac) %>%
      addTiles() %>%
      addRasterImage(
        selected_thk(), 
        colors = pal,  
        project = TRUE
      ) %>%
      addPolygons(data = glac, color = "red", weight = 2, fillOpacity = 0,
                  label = ~glac_nom) 
  })
  
  observe({
    if (length(drawn_polygon()) == 0) {
      show("dl_container")
    } else {
      hide("dl_container")
    }
  })
  
  output$mapplot_select <- renderLeaflet({
    req(df_spatial(), drawn_polygon(), selected_thk())
    
    thk_clip <- crop(selected_thk(), df_spatial())
    
    leaflet() %>%
      addTiles() %>%
      addRasterImage(
        thk_clip, 
        colors = pal,  
        project = TRUE
      ) %>%
      addPolygons(data = drawn_polygon(),
                  fillOpacity = 0,
                  weight = 2,
                  color = "black") %>% 
      addPolygons(data = df_spatial(),
                  color = "red", 
                  weight = 2, 
                  fillOpacity = 0,
                  label = ~glac_nom) 
  })
  
  observe({
    if ("vec1" %in% input$raw) {
      show("format_selector1", anim = TRUE, time = 0.5)
    } else {
      hide("format_selector1", anim = TRUE, time = 0.5)
    }
  })
  
  observe({
    if ("vec2" %in% input$select) {
      show("format_selector2", anim = TRUE, time = 0.5)
    } else {
      hide("format_selector2", anim = TRUE, time = 0.5)
    }
  })
  
  output$dl_content <- renderUI({
    if(length(drawn_polygon()) == 0) {
      div(id = "dl_container",
          class = "custom-alert",
          style = "display: flex; 
                justify-content: center; 
                align-items: center; 
                margin-top: 150px;
                text-align: center; 
                font-size: 30px; 
                padding: 20px; 
                color: orange; 
                font-weight: bold;",
          "Veuillez sélectionner au moins un glacier ou vous assurer que la géometrie du polygone de sélection est valide"
      )
    } else {
      div(
        leafletOutput("mapplot_select", width = "47vw", height = "70vh"),
        br(),
        div(
          radioGroupButtons(
            inputId = 'select', 
            label = "Sélection du type d'export",
            choices = setNames(
              c('rast2', 'vec2'), 
              c("Raster d'épaisseur", "Glaciers")
            ),
            selected = "rast2",
            checkIcon = list(yes = icon("check")),
            status = 'primary',
            justified = TRUE
          ),
          hidden(
            div(
              id = "format_selector2",
              radioGroupButtons(
                inputId = 'format_selected2', 
                label = "Formats de l'export :",
                choices = c("csv",
                            "gpkg"),
                selected = "gpkg",
                checkIcon = list(
                  yes = icon("check")
                ),
                status = 'primary',
                justified = TRUE
              )
            )
          ),
          div(style="display: flex; justify-content: center; align-items: center;",
              downloadBttn("dl_select", 
                           label = 'Téléchargement des données', 
                           style = 'fill',
                           color = 'success',
                           icon = icon("file-zipper")
              )
          ),
          br()
        )
      )
    }
  })
  
  output$dl_raw <- downloadHandler(
    
    filename = function() {
      selected_format <- input$format_selected1
      if ("rast1" %in% input$raw) {
        return(paste0("glac_raw_tck_", Sys.Date(), ".tif"))
      }
      if ("vec1" %in% input$raw) {
        if (selected_format == "csv") {
          return(paste0("glac_raw_", Sys.Date(), ".csv"))
        }
        if (selected_format == "gpkg") {
          return(paste0("glac_raw_", Sys.Date(), ".gpkg"))
        }
      }
    },
    content = function(file) {
      temp_dir <- gsub(":", "-", tempdir())
      
      if ("rast1" %in% input$raw) {
        
        tckSO <- rast("data/thk0050_to.tif")
        names(tckSO) <- paste0("TO_", 2000:2050)
        
        tckSR <- rast("data/thk0050_ti.tif")
        names(tckSR) <- paste0("TI_", 2000:2050)
        
        tckSP <- rast("data/thk0050_tp.tif")
        names(tckSP) <- paste0("TP_", 2000:2050)
        
        tck_comp <- c(tckSO, tckSR, tckSP)
        
        writeRaster(tck_comp, file, filetype = "GTiff", overwrite = TRUE)
        
      } else if ("vec1" %in% input$raw) {
        if (input$format_selected1 == "csv") {
          modified_data <- as.data.frame(glac) %>% 
            select(-c(geom))
          write.csv(modified_data, file, row.names = FALSE)
          
        } else if (input$format_selected1 == "gpkg") {
          st_write(glac, file, driver = "GPKG", quiet = TRUE)
        }
      }
    }
  )
  
  output$dl_select <- downloadHandler(
    filename = function() {
      selected_format <- input$format_selected2
      if ("rast2" %in% input$select) {
        return(paste0("glac_tck_", Sys.Date(), ".tif"))
      }
      if ("vec2" %in% input$select) {
        if (selected_format == "csv") {
          return(paste0("glac_", Sys.Date(), ".csv"))
        }
        if (selected_format == "gpkg") {
          return(paste0("glac_", Sys.Date(), ".gpkg"))
        }
      }
    },
    content = function(file) {
      temp_dir <- gsub(":", "-", tempdir())
      if ("rast2" %in% input$select) {
        
        tckSO <- rast("data/thk0050_to.tif")
        names(tckSO) <- paste0("TO_", 2000:2050)
        
        tckSR <- rast("data/thk0050_ti.tif")
        names(tckSR) <- paste0("TI_", 2000:2050)
        
        tckSP <- rast("data/thk0050_tp.tif")
        names(tckSP) <- paste0("TP_", 2000:2050)
        
        tck_comp <- c(tckSO, tckSR, tckSP)
        tck_comp <- crop(tck_comp, df_spatial())
        
        writeRaster(tck_comp, file, filetype = "GTiff", overwrite = TRUE)
      } else if ("vec2" %in% input$select) {
        if (input$format_selected2 == "csv") {
          modified_data <- as.data.frame(df_spatial()) %>%
            select(-c(geom)) 
          write.csv(modified_data, file, row.names = FALSE)
        } else if (input$format_selected2 == "gpkg") {
          modified_data <- as.data.frame(df_spatial()) 
          st_write(modified_data, file, driver = "GPKG", quiet = TRUE)
        }
      } 
    }
  )
  
}

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))

