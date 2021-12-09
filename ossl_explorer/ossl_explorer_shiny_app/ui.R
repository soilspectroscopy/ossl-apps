#
# This is the user-interface definition of a Shiny web application. 
#


remove_leaf_features <- tags$script(HTML(
    "
Shiny.addCustomMessageHandler(
  'removeleaflet',
  function(x){
    console.log('deleting',x)
    // get leaflet map
    var map = HTMLWidgets.find('#' + x.elid).getMap();
    // remove
    map.removeLayer(map._layers[x.layerid])
  })
"
))



# Define UI for application 
ui <- shinyUI(
    tagList(
      busy_start_up(
        # loader = tags$img(
        #   src = "https://jeroen.github.io/images/banana.gif",
        #   width = 100
        # ),
        loader = spin_kit(spin = "circle", color = "white", style = "width:70px; height:70px;"),
        # loader = spin_epic("orbit", color = "#62B331"), # withSpinner(color="#62B331"), #
        text = div(
          strong(h2("Loading data...")),
          img(src = "logos/OSSL_White_1.png", style = "align: center; height:50px;"),
          p("Follow us on ", a(href = "https://github.com/soilspectroscopy", target="_blank", img(src = "github.png", style = "align: center; height:50px;")))
          ),
        # timeout = 1500,
        mode = "auto",
        color = "white",
        background = "#4E9522"
      ),
      tags$head(tags$script(type="text/javascript", src = "nav_engine.js")),
        tags$script(HTML(
            "document.body.style.backgroundColor = 'sapphire';"
        )),
        tags$script(HTML(
            "document.body.style.fontFamily = 'Verdana';"
        )),
        tags$head(tags$link(rel="shortcut icon", href="logos/Design_01_200.png")),
        tags$head(HTML("<title>OSSL Explorer</title>")),
        tags$head(tags$script(type="text/javascript", src = "code.js")),
        
        useShinyjs(),
        shinyjs::extendShinyjs(text = "shinyjs.polygon_click = function(){
                      var e = document.createEvent('Event');
                      e.initEvent('click', true, true);
                      var cb = document.getElementsByClassName('leaflet-draw-draw-polygon');
                      return !cb[0].dispatchEvent(e);
                      }", functions = c("polygon_click")),
        shinyjs::extendShinyjs(text = "shinyjs.rectangle_click = function(){
                      var e = document.createEvent('Event');
                      e.initEvent('click', true, true);
                      var cb = document.getElementsByClassName('leaflet-draw-draw-rectangle');
                      return !cb[0].dispatchEvent(e);
                      }", functions = c("rectangle_click")),
        remove_leaf_features,
        tags$head(
            tags$style(HTML("
                           div.leaflet-draw-toolbar{
                            visibility: hidden !important;
                            }"))
        ),
        tags$script(HTML("
                                       var openTab = function(tabName){
                                       $('a', $('.navbar')).each(function() {
                                       if(this.getAttribute('data-value') == tabName) {
                                       this.click()
                                       };
                                       });
                                       }")),
        tags$footer(title="",  align = "center", style = "
                      position:fixed;
                      bottom:0;
                      width:100%;
                      height:70px; /* Height of the footer */
                      color: black;
                      padding: 0px;
                      background-color: rgba(255, 255, 255, 0.5);
                      z-index: 1000;
                      display: inline-block;
                      font-size: 12px !important;
                    ",
                    tags$a(div(#p("Powered by: "),
                      a(href = "https://www.woodwellclimate.org/", target="_blank", img(src = "woodwell_climate_logo.png", style = "align: left; height:70px;")), 
                      a(href = "https://www.ufl.edu/", target="_blank", img(src = "uflorida_logo.png", style = "align: left; height:70px;")), 
                      a(href = "https://opengeohub.org/", target="_blank", img(src = "opengeohub_logo.png", style = "align: left; height:70px;")),
                      #a("USDA National Institute of Food and Agriculture Award " , style = "text-align: right; height:70px; text-decoration: none; color: #4E9522;, font-size: 12px !important;"), 
                      #a(href = "https://nifa.usda.gov/press-release/nifa-invests-over-7-million-big-data-artificial-intelligence-and-other", target="_blank", "# 2020-67021-32467", style = "text-align: right; height:70px;  color: #4E9522; font-size: 12px;"),
                               
                               style="height:70px;  text-align: left;  float:left;  align: left; padding: 0px; padding-left: 20px; bottom:0; display: inline-block;"),
                      div(
                        a("USDA National Institute of Food and Agriculture Award " , style = "text-align: right; align: right; height:70px; text-decoration: none; color: black; font-size: 12px !important;"), 
                        a(href = "https://nifa.usda.gov/press-release/nifa-invests-over-7-million-big-data-artificial-intelligence-and-other", target="_blank", "# 2020-67021-32467", style = "text-align: right; align: right; height:70px;  color: black; font-weight: bold; font-size: 12px;"),
                        
                        style="height:70px;  text-align: right; float:right; align: right; padding: 0px; padding-right: 20px; bottom:0; display: inline-block; line-height: 70px")

                             ),

                           
                    
                    #tags$a(href="link to github",icon("github"))
        ),
        navbarPage(fluid = TRUE,
               id = "inTabset",
               title = a(href = "", span(img(src = "logos/OSSL_White_1.png", style = "align: center; height:30px;"), title ="Reload the app"), style = "cursor: pointer; text-decoration: none; color:white;"), # samo ovo kad se stavi radi reload aplikacije
                 # a(onclick = "openTab('Geography')", # za otvaranje prvog taba klikom na title
                 #         href = NULL,
                 #         "OSSL Explorer",
                 #         style = "cursor: pointer; text-decoration: none; color:white;"),
               selected ="Geography", 
               #theme = shinytheme("united"),
               tabPanel(title = span("Geography", title = "Browse by location"), value = "Geography", 
                        #bsTooltip("Geography", title ="Browse by location", trigger = "hover", placement	= "right"),
                        div(class="outer",
                            tags$head(
                                # Include custom CSS
                                includeCSS("styles.css")
                            ),
                            #top: 15% !important;
                            #position: fixed !important;
                            tags$head(tags$style(
                                HTML('div.leaflet-control-search {
                                left: 50% !important;
                                position-top: 2px !important;
                                margin-top: -50px !important;
                                position: absolute;
                                }')
                            )),
                        leafletOutput("map", width="100%", height="100%"), # leafglOutput
                        absolutePanel(
                                      id = "controls", 
                                      class = "panel panel-default", 
                                      fixed = TRUE,
                                      draggable = TRUE, 
                                      top = 300, left = 60, right = "auto", bottom = "auto",
                                      width = 330, height = "auto",
                                      h4("Data selection"),
                                         #actionButton("openTweet",label = "", icon = shiny::icon("twitter"),
                                        #              style = "background-color: rgba(255, 255, 255, 0.5); color:black; border:rgba(255, 255, 255, 0.5); float:right;"),
                                         # actionButton("openLand",label = "", icon = shiny::icon("info-circle"),
                                         #              style = "background-color: rgba(255, 255, 255, 0.5); color:black; border:rgba(255, 255, 255, 0.5); float:right;")),
                                      p(""),
                                      bsCollapse(id = "collapseExample",
                                                 bsCollapsePanel("Geospatial",
                                                                 #"Please use the button to draw and to remove a polygon from the map.",
                                                                 p(""),
                                                                 actionButton(inputId = "draw_poly", label= "Draw", class = "btn-info shiny-bound-input", icon = shiny::icon("draw-polygon")),
                                                                 actionButton(inputId = "draw_rectangle", label= "Draw", class = "btn-info shiny-bound-input", icon = shiny::icon("square")),
                                                                 actionButton(inputId = "del_poly", label= "Clear", class = "btn-info shiny-bound-input", icon = shiny::icon("eraser")),
                                                                 hr(),
                                                                 selectizeInput(
                                                                     inputId = 'nation',
                                                                     label = "Nation: ",
                                                                     choices = factor(sp_bounds_0$NAME_0), 
                                                                     multiple = F,
                                                                     options = list(
                                                                         placeholder = 'Please select a Nation',
                                                                         onInitialize = I('function() { this.setValue(""); }')
                                                                 )),
                                                                 uiOutput("state.dynamicui"),
                                                                 style = "success"),
                                                 bsCollapsePanel("Attributes",
                                                                 selectizeInput(
                                                                     inputId = 'soil_order',
                                                                     label = "Soil texture: ",
                                                                     choices = unique(soilsite.data$layer.texture_usda_c),
                                                                     multiple = T,
                                                                     options = list(
                                                                         placeholder = 'Please select a Soil texture',
                                                                         onInitialize = I('function() { this.setValue(""); }')
                                                                 )),
                                                                 numericRangeInput(
                                                                     inputId = "depth_range",
                                                                     label = "Depth range [cm]: ",
                                                                     value = c(min(soilsite.data$layer.lower.depth_usda_cm, na.rm = T), max(soilsite.data$layer.lower.depth_usda_cm, na.rm = T)), # c(0, 1783), #soilsite.data$layer.lower.depth_usda_cm,
                                                                     separator = " to "
                                                                 ),
                                                                 style = "success"),
                                                 bsCollapsePanel("Dataset",
                                                                 
                                                                 selectizeInput(
                                                                   inputId = 'library_site',
                                                                   label = "Source dataset: ",
                                                                   choices = unique(soilsite.data$dataset.title_utf8_txt), #dataset.code_ascii_c
                                                                   multiple = T,
                                                                   options = list(
                                                                     placeholder = 'Please select a Source dataset',
                                                                     onInitialize = I('function() { this.setValue(""); }')
                                                                   )),
                                                                 style = "success")
                                      ), # bsCollapse
                                      p(""),
                                      actionButton(inputId = "jumpToSoil", label= "Soil properties", class = "btn-info btn-block", icon = shiny::icon("arrow-circle-right")),
                                      verbatimTextOutput("id1"),verbatimTextOutput("id2"),verbatimTextOutput("id3"),verbatimTextOutput("id4")
                                    
                        ), #absolutePanel Select by
                        
                        absolutePanel(
                          id = "controls2", 
                          class = "panel panel-default", 
                          fixed = TRUE,
                          draggable = TRUE, 
                          top = 65, left = 60, right = "auto", bottom = "auto",
                          width = 450, height = "auto",
                          
                          bsCollapse(id = "collapseExample3", open = "Discover The Map", 
                                     bsCollapsePanel("Discover The Map",
                                                     div(style="display:inline-block",
                                                         selectizeInput(inputId = "search_OSM_text",
                                                                        label = NULL,
                                                                        multiple = F,
                                                                        choices = NULL,
                                                                        # selected = NULL,
                                                                        options = list(
                                                                           placeholder = 'Search for a geographic or administrative area', create = TRUE#,
                                                                                                                                                         #createOnBlur = TRUE#,
                                                                          # onInitialize = I('function() { this.setValue(""); }')
                                                                        ))),
                                                         # textInput(inputId = "search_OSM_text",
                                                         #           label=NULL,
                                                         #           value = NULL,
                                                         #           placeholder = 'Search for a geographic or administrative area'),
                                                     
                                                       #div(style="display:inline-block",textInput(inputId = "search_OSM", placeholder = "Search for a geographic or administrative area", label = "" )),
                                                       # div(style="display:inline-block",textInput(inputId = "results_OSM", placeholder = "Search for a geographic or administrative area", label = "" )),
                                                       div(style="display:inline-block",actionButton(inputId = "search_OSM", label= "", class = "btn-info shiny-bound-input", icon = shiny::icon("search"))),
                                                       div(style="display:inline-block",actionButton(inputId = "remove_OSM", label= "", class = "btn-info shiny-bound-input", icon = shiny::icon("times-circle"))),
                                                       # div(style="display:inline-block",
                                                       # hidden(
                                                       #   
                                                       # )
                                                     hidden(
                                                       p(id = "no_locs", "No locations found.", style = "color:red")
                                                      ),
                                                     style = "success")
                                    
                          ),
                          div(style="display:inline-block; align:center;",a(img(src="databases_.png", style = "width:30px; height:40px;")), strong("OSSL: explore, download, share and extend..."))
                        ), #absolutePanel Search
                        

                        absolutePanel(id = "controls", 
                                      class = "panel panel-default", 
                                      fixed = TRUE,
                                      draggable = TRUE, 
                                      top = "80%", left = 20, right = "auto", bottom = "auto",
                                      width = 330, 
                                      p(""),
                                      h4("# of geolocated samples: ",textOutput(outputId = "no_pts", inline = TRUE)),
                                      h4("# of MIR spectra: ", textOutput(outputId = "no_mir", inline = TRUE)),
                                      h4("# of VNIR spectra: ", textOutput(outputId = "no_vnir", inline = TRUE))
                        ),  #absolutePanel Numbers
                        
                        absolutePanel(id = "controls", 
                                      class = "panel panel-default", 
                                      fixed = TRUE,
                                      draggable = TRUE, 
                                      top = "80%", right = 20, left = "auto", bottom = "auto",
                                      width = 330, 
                                      p(""),
                                      p(""),
                                      actionButton(inputId = "clearS", label= "Clear selection", class = "btn-danger btn-block", icon = shiny::icon("redo")),
                                      p(""),
                                      downloadButton(outputId = "downloadData", label = "Download now", class = "btn-info btn-block", icon = shiny::icon("download")),
                                      #p(""),
                                      #actionButton(inputId = "buttonSH", label = "Show / hide feature info", class = "btn-info btn-block", icon = shiny::icon("eye-slash"))
                                      
                        ),  #absolutePanel Download and clear
                        hidden( 
                            div(id = "conditionalPanel",
                                fluidRow(
                                    absolutePanel(id = "controls", 
                                                  class = "panel panel-default", 
                                                  fixed = TRUE,
                                                  draggable = TRUE, 
                                                  top = "20%", left = "auto", right = 370, bottom = "auto",
                                                  width = 400, 
                                                  p(""),
                                                  selectInput(inputId = "selectPointInfo",
                                                              label=NULL,
                                                              choices = NULL,
                                                              selected=NULL,
                                                              multiple = FALSE),
                                                  bsCollapse(id = "collapseExample1", open = "Location properties",
                                                             bsCollapsePanel("Location properties",
                                                                             #h5("Click on the Marker on the Map: "),
                                                                             tableOutput('point_table'),
                                                                             # h5("ID: ",textOutput(outputId = "pr1", inline = TRUE)),
                                                                             # h5("Soil texture: ", textOutput(outputId = "pr2", inline = TRUE)),
                                                                             # h5("Upper depth [cm]: ", textOutput(outputId = "pr3", inline = TRUE)),
                                                                             # h5("Lower depth [cm]: ", textOutput(outputId = "pr4", inline = TRUE)),
                                                                             # h5("Address: ", textOutput(outputId = "pr5", inline = TRUE)),
                                                                             style = "success"),
                                                             bsCollapsePanel("VISNIR spectra profile",
                                                                             h5("VISNIR spectra profile: "), 
                                                                             plotlyOutput("pl.vis"), 
                                                                             style = "success"),
                                                             bsCollapsePanel("MIR spectra profile",
                                                                             h5("MIR spectra profile: "), 
                                                                             plotlyOutput("pl.mir"), 
                                                                             style = "success")
                                                  ) # bsCollapse
                                    )  #absolutePanel Point Info
                        ))),
                        shinyjs::useShinyjs(),
                        #shinyjs::show(
                          div(
                            id = "landingpage",
                            fluidRow(
                              absolutePanel(id = "controls1", 
                                                           class = "panel panel-default", 
                                                           fixed = TRUE,
                                                           draggable = FALSE, 
                                                           top = "20%", left = "50%", right = "auto", bottom = "auto",
                                                           width = 700, height = "auto",# 550,
                                                           p(""),
                                            actionButton(inputId = "closePanel", label= "", class = "btn-info", icon = shiny::icon("close"),
                                                                        style = "background-color: rgba(255, 255, 255, 0.5); color:black; border:rgba(255, 255, 255, 0.5); float:right;"),
                                            
                                            p(""),              
                                            fluidRow(
                                            #img(src = "neonftir.png", style = "")
                                            # div(a(href = 'https://soilspectroscopy.org/', target="_blank", img(src = "logos/OSSL.png", style = "background-color: #4E9522; align: center;")), style="text-align: center;"),
                                            div(img(src = "logos/OSSL.png", style = "align: center; height:50px;"), style="text-align: center;"),
                                            # h4("Open Soil Spectral Library (OSSL)", style = "text-align: center; color: #4E9522; font-size: 30px;"),
                                            h4("Open Soil Spectral Library", style = "text-align: center; color: black; font-size: 30px;"),
                                            #h4("and", style = "text-align: center; color: #4E9522; font-size: 30px;"),
                                            #h4("OSSL Explorer!", style = "text-align: center; color: #4E9522; font-size: 40px;"),
                                            # br(),
                                            div(class = "containerS",style = "align: center; height: auto;",
                                            # br(),
                                            # br(),
                                            div(strong(
                                              p(a(href = 'https://soilspectroscopy.org/', target="_blank", 'SoilSpec4GG'), " is pleased to release the first beta version of the OSSL explorer and calibration engine. These applications are a work in progress and we have provided these services with the hope of receiving feedback that will enable us to improve the functioning and utility of services."),
                                              
                                              div(a(href = 'https://soilspectroscopy.org/', target="_blank", 'SoilSpec4GG'), " is a USDA-funded Food and Agriculture Cyberinformatics Tools Coordinated Innovation Network (USDA National Institute of Food and Agriculture Award  ", a(href="https://nifa.usda.gov/press-release/nifa-invests-over-7-million-big-data-artificial-intelligence-and-other", target="_blank", "#2020-67021-32467", style = "display: inline-block; margin-right: -4px; margin-left: -4px;"),
                                                  "). This project brings together soil scientists, spectroscopists, informaticians, data scientists and software engineers to overcome some of the current bottlenecks preventing wider and more efficient use of soil spectroscopy. It includes a series of working groups to address topics including calibration transfer, model choice, outreach & demonstration, and use of spectroscopy to inform global carbon cycle modeling.", style = "display: inline-block; margin-right: -4px;"),
                                              p(""),
                                              p("For a brief introduction to the project, please visit the ", a(href = "https://soilspectroscopy.org/", target="_blank", "https://soilspectroscopy.org/.")),
                                              p("Need help using OSSL Explorer or have feedback? Email ", a(href = "mailto: soilspec4gg@woodwellclimate.org", "soilspec4gg@woodwellclimate.org.")),
                                              
                                              style = "text-align: left;"
                                              
                                            ))
                                            )
                                            #a(img(src = "neonftir.png", style = "width:800px; height:300px; text-align: center; align: center;"),  style = "width:800px; height:300px; text-align: center; align: center;")
                                             
                                            ),
                                            br(),
                                            div(a(href = 'https://soilspectroscopy.org/', target="_blank", img(src = "logos/Design_03.png", style = "align: center; height:50px;")), style="text-align: center;")
                                            )  # absolutePanel
                                    ) # fluidRow
                            ), # div
                 
                      
                        
                        div(
                          id = "tweeterpage",
                          fluidRow(
                            absolutePanel(id = "controls3", 
                                          class = "panel panel-default", 
                                          fixed = TRUE,
                                          draggable = TRUE, 
                                          top = "20%", left = "auto", right = 20, bottom = "auto",
                                          width = "330px", height = "50%",
                                          p(""),
                                          actionButton(inputId = "closePanelTweet", label= "", class = "btn-info", icon = shiny::icon("close"),
                                                       style = "background-color: rgba(255, 255, 255, 0.5); color:black; border:rgba(255, 255, 255, 0.5); float:right;"),
                                          
                                          p(""),              
                                          fluidRow(
                                            column(width=12,
                                                   tags$head(tags$script('!function(d,s,id){var js,fjs=d.getElementsByTagName(s)    [0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");')),
                                                   
                                                   column(width=12, #box(width=NULL, height=NULL,
                                                    
                                                          a("Tweets by @soilspec", class="twitter-timeline", 
                                                            href="https://twitter.com/soilspec", style = "overflow-y:scroll")
                                                                       
                                                   #)
                                                   )
                                          )
                                          )
                            )  # absolutePanel
                          ) # fluidRow
                        ) # div tweeterpage
                        
               ) #div
                        
               ),
               tabPanel(div(span("Soil properties", title = "Browse by soil property")), value = "Soil properties", 
                        #bsTooltip("Soil properties", title ="Browse by soil property", trigger = "hover", placement	= "right"),
                        
                            tags$head(
                                # Include custom CSS
                                includeCSS("styles.css")
                            ),
                        fluidRow(
                            column(width = 3,
                                   bsCollapse(id = "collapseExample2", open = "Soil properties", 
                                              
                                              bsCollapsePanel("Soil properties",
                                                              h5("Has spectral data: "),
                                                              span(class="mycheckbox", 
                                                                   checkboxInput(inputId = 'check.mir',
                                                                                 label = "MIR",
                                                                                 value = T),
                                                                   checkboxInput(inputId = 'check.vis',
                                                                                 label = "VISNIR",
                                                                                 value = T)
                                                                   ),
                                                              h5("Data selection based on soil properties range: "),
                                                              materialSwitch(inputId = "switchselection", label = "", status = "success", value = FALSE),
                                                              #hr(),
                                                              bsCollapse(id = "collapseExample3", open = "Soil property #1: ",
                                                                         bsCollapsePanel("Soil property #1: ",
                                                                                         selectizeInput(
                                                                                           'var1', '', choices = factor(soillab.df$full_name),
                                                                                           selected =  factor(soillab.df$full_name)[27]
                                                                                          
                                                                                           # options = list(
                                                                                           #   placeholder = 'Please select a soil property below',
                                                                                           #   onInitialize = I('function() { this.setValue(""); }')
                                                                                           # )
                                                                                         ),
                                                                                         numericRangeInput(
                                                                                           inputId = "range_var1",
                                                                                           label = "Range: ",
                                                                                           value = c(min(soillab.data[, soillab.df$full_name[27]], na.rm = T), max(soillab.data[, soillab.df$full_name[27]], na.rm = T)),
                                                                                           separator = " to "
                                                                                         ),
                                                                           style = "info"),
                                                                         bsCollapsePanel("Soil property #2: ",
                                                                                         selectizeInput(
                                                                                           'var2', '', choices = factor(soillab.df$full_name),
                                                                                           selected =  factor(soillab.df$full_name)[13]
                                                                                           # options = list(
                                                                                           #   placeholder = 'Please select a soil property below',
                                                                                           #   onInitialize = I('function() { this.setValue(""); }')
                                                                                           # )
                                                                                         ),
                                                                                         numericRangeInput(
                                                                                           inputId = "range_var2",
                                                                                           label = "Range: ",
                                                                                           value = c(min(soillab.data[, soillab.df$full_name[13]], na.rm = T), max(soillab.data[, soillab.df$full_name[13]], na.rm = T)),
                                                                                           separator = " to "
                                                                                         ),
                                                                           style = "info"),
                                                                         bsCollapsePanel("Soil property #3: ",
                                                                                         selectizeInput(
                                                                                           'var3', '', choices = factor(soillab.df$full_name),
                                                                                           selected =  factor(soillab.df$full_name)[19]
                                                                                           # options = list(
                                                                                           #   placeholder = 'Please select a soil property below',
                                                                                           #   onInitialize = I('function() { this.setValue(""); }')
                                                                                           # )
                                                                                         ),
                                                                                         numericRangeInput(
                                                                                           inputId = "range_var3",
                                                                                           label = "Range: ",
                                                                                           value = c(min(soillab.data[, soillab.df$full_name[19]], na.rm = T), max(soillab.data[, soillab.df$full_name[19]], na.rm = T)),
                                                                                           separator = " to "
                                                                                         ),
                                                                           style = "info"),
                                                                         bsCollapsePanel("Soil property #4: ",
                                                                                         selectizeInput(
                                                                                           'var4', '', choices = factor(soillab.df$full_name),
                                                                                           selected =  factor(soillab.df$full_name)[3]
                                                                                           # options = list(
                                                                                           #   placeholder = 'Please select a soil property below',
                                                                                           #   onInitialize = I('function() { this.setValue(""); }')
                                                                                           # )
                                                                                         ),
                                                                                         numericRangeInput(
                                                                                           inputId = "range_var4",
                                                                                           label = "Range: ",
                                                                                           value = c(min(soillab.data[, soillab.df$full_name[3]], na.rm = T), max(soillab.data[, soillab.df$full_name[3]], na.rm = T)),
                                                                                           separator = " to "
                                                                                         ),
                                                                            style = "info")
                                                                         ),
                                                              style = "success"
                                                              ) # bsCollapsePanel
                                              ), # bsCollapse

                                       ), # column inputs 
                                
                                column(width = 9,
                                       fluidRow(
                                           column(width = 6,
                                                  plotlyOutput(outputId = "var_plot1", height = 300) %>% withSpinner(color="#62B331")),
                                           column(width = 6,
                                                  plotlyOutput(outputId = "var_plot2", height = 300) %>% withSpinner(color="#62B331")
                                                  )
                                           
                                       ),
                                       p(""),
                                       fluidRow(
                                           column(width = 6,
                                                  plotlyOutput(outputId = "var_plot3", height = 300) %>% withSpinner(color="#62B331")),
                                           column(width = 6,
                                                  plotlyOutput(outputId = "var_plot4", height = 300) %>% withSpinner(color="#62B331")
                                           )
                                       )
                                       #fluidRow(column(12,div(style = "height:70px;")))
                                       ) # column plots
                            ), # fluidRow 
                        br(),
                        br(),
                        br(),
                        br(),
                            
                            absolutePanel(id = "controls", 
                                          class = "panel panel-default", 
                                          fixed = TRUE,
                                          draggable = TRUE, 
                                          top = "80%", left = 20, right = "auto", bottom = "auto",
                                          width = 330, 
                                          p(""),
                                          h4("# of geolocated samples: ",textOutput(outputId = "no_pts1", inline = TRUE)),
                                          h4("# of MIR spectra: ", textOutput(outputId = "no_mir1", inline = TRUE)),
                                          h4("# of VNIR spectra: ", textOutput(outputId = "no_vnir1", inline = TRUE))
                            ),  #absolutePanel Numbers
                            
                            absolutePanel(id = "controls", 
                                          class = "panel panel-default", 
                                          fixed = TRUE,
                                          draggable = TRUE, 
                                          top = "80%", right = 20, left = "auto", bottom = "auto",
                                          width = 330, 
                                          p(""),
                                          p(""),
                                          actionButton(inputId = "clearS1", label= "Clear selection", class = "btn-danger btn-block", icon = shiny::icon("redo")),
                                          p(""),
                                          downloadButton(outputId = "downloadData1", label = "Download now", class = "btn-info btn-block", icon = shiny::icon("download")) 
                                          
                            )  #absolutePanel Download and clear
               ),
               tabPanel(span("About", title = "About OSSL"), 
                        p(""),
                        setBackgroundImage(src = "bare_soil_theme3.jpg"),
                        fluidRow(
                          column(9, align = "center-left", style = "background-color: rgba(255, 255, 255, 0.90); margin-left:10px; dispaly:inline-block;",
                                 p(""),
                                 p("The OSSL is only possible through the hard work of many organizations that have produced and provided open high quality soil data. Data-driven advances in soil science are reliant upon these soil collection efforts. We are extremely grateful to the ",
                                 a(href="https://www.nrcs.usda.gov/wps/portal/nrcs/main/soils/research/", target="_blank", "USDA NRCS National Soil Survey Center – Kellogg Soil Survey Laboratory"), ", ",
                                 a(href="https://www.worldagroforestry.org/", target="_blank", "ICRAF-World Agroforestry"), ", ",
                                 a(href="https://www.isric.org/", target="_blank", "ISRIC-World Soil Information"), ", the ",
                                 a(href="http://africasoils.net/services/data/soil-databases/", target="_blank", "Africa Soil Information Service"), " (AfSIS), funded by the Bill and Melinda Gates Foundation, the ",
                                 a(href="https://esdac.jrc.ec.europa.eu/", target="_blank", "European Soil Data Centre"), ", the ",
                                 a(href="https://www.neonscience.org/", target="_blank", "National Ecological Observatory Network"), " (NEON), and ",
                                 a(href="https://sae.ethz.ch/", target="_blank", "ETH Zurich"), " for producing and providing high quality data that the OSSL can build upon."),
                                 p(""),
                                 strong("Data Use Policy and Guidelines"),
                                 p(""),
                                 strong("License"),
                                 p(""),
                                 p('If not specified otherwise, all data and results of data mining (models, visualizations and similar) available via the Open Soil Spectral Library (OSSL) are licensed under ', a(href = 'https://creativecommons.org/licenses/by/4.0/', target="_blank", 'CC-BY-4', .noWS = "outside"), '.', .noWS = c("after-begin", "before-end")), 
                                 p(strong("Design and functionality by: "), a(href = 'https://gilab.rs/', target="_blank", img(src="logo-1-1.svg"))), 
                                 p(""),
                                 strong("Soil Spectroscopy for Global Good"),
                                 p(""),
                                 p(a(href = 'https://soilspectroscopy.org/about/', 'SoilSpec4GG', target="_blank"), " is a USDA-funded Food and Agriculture Cyberinformatics Tools Coordinated Innovation Network (USDA National Institute of Food and Agriculture", a(href="https://nifa.usda.gov/press-release/nifa-invests-over-7-million-big-data-artificial-intelligence-and-other", target="_blank", "Award #2020-67021-32467"),
                                   "). This project brings together soil scientists, spectroscopists, informaticians, data scientists and software engineers to overcome some of the current bottlenecks preventing wider and more efficient use of soil spectroscopy."),
                                 p(""),
                                 strong("Database citation"),
                                 p("Any publications using these data should cite the references listed ", a(href="https://soilspectroscopy.github.io/ossl-manual/index.html#acknowledgments", target="_blank", "here"), "."),
                                 p(""),
                                 p("In addition, this database should be referenced as well (DOI representing all versions). If the data has changed since original publication, arising publications should cite the specific version used, ideally with a DOI associated with that version. Authors may contact ",
                                   a(href="https://www.woodwellclimate.org/staff/jonathan-sanderman/", target="_blank", "Jonathan Sanderman"), " (Woodwell Climate Research), ",
                                   a(href="https://www.essie.ufl.edu/people/name/kathe-todd-brown/", target="_blank", "Katherine Todd-Brown"), "and/or ",
                                   a(href="https://www.opengeohub.org/people/tom-hengl", target="_blank", "Tom Hengl"), " (OpenGeoHub foundation) to generate a release and associated DOI that matches the database version used."),
                                 p("In publications, please include some form of the following acknowledgement: “The Open Soil Spectral Library and associated services were developed with funding from USDA NIFA Award #2020-67021-32467.”"),
                                 p(""),
                                 strong("Citation"),
                                 p("Publication describing the production and uses of the OSSL is pending. For technical documentation please refer to: "),
                                 tags$ul(
                                   tags$li(p("Woodwell Climate Research Center, University of Florida, OpenGeoHub and contributors (2021). ", strong('"Open Soil Spectral Library: technical manual "'), ", ",
                                   a(href="https://soilspectroscopy.github.io/ossl-manual/", target="_blank", "https://soilspectroscopy.github.io/ossl-manual/"), "."),
                                 )),
                                 p(""),
                                 strong("DISCLAIMER"),
                                 p("Last updated December 5, 2021"),
                                 p("The information provided by Open Soil Spectral Library (“we”, “us”, or “our”) on explorer.soilspectroscopy.org or engine.soilspectroscopy.org (the “Site”) is for general informational purposes only. All information on the Site and generated by the Site is provided in good faith; however, we make no representation or warranty of any kind, express or implied, regarding the accuracy, adequacy, validity, reliability, availability or completeness of any information on the Site. UNDER NO CIRCUMSTANCE SHALL WE HAVE ANY LIABILITY TO YOU FOR ANY LOSS OR DAMAGE OF ANY KIND INCURRED AS A RESULT OF THE USE OF THE SITE OR RELIANCE ON ANY INFORMATION PROVIDED. YOUR USE OF THE SITE AND YOUR RELIANCE ON ANY INFORMATION ON THE SITE OR GENERATED BY THE SITE IS SOLELY AT YOUR OWN RISK."),
                                 p(""),
                                 strong("PLEASE CONTRIBUTE"),
                                 p("The Open Soil Spectral Library is a work in progress. This first release is intended as a sounding board for feedback. The database and estimation services will continue to improve."),
                                 p("Please contribute to this project and help us make", strong("better tools for measuring and monitoring our soils and land!")),
                                 p("This is how you can contribute:"),
                                 HTML("<ol>
                                      <li>Donate your SSL data so it can be included to the OSSL. Note: we accept only correctly formatted, quality controlled soil spectral scans which come with reference values of soil properties (wet-chemistry).</li>
                                      <li>Help develop open source tools that use OSSL for both public needs and business purposes.</li>
                                      <li>Report problems and help us debug and resolve issues. Help us make better soil data for a healthier planet!</li>
                                      </ol>"),
                                 
                                 p(""),
                                 strong("Contacts"),
                                 p(a(href="https://www.woodwellclimate.org/staff/jonathan-sanderman/", target="_blank", "Jonathan Sanderman,"), "Woodwell Climate Research"),
                                 p(a(href="https://soilspectroscopy.org/about/", target="_blank", "https://soilspectroscopy.org/about/")),
                                 p(""),
                                 ),
                          
                          column(width=2,
                                 align = "center-right", 
                                 # htmlOutput("frame"),
                                 # tags$script(type="text/javascript", src = "revolvermaps.js")
                                 HTML('<a href="https://www.revolvermaps.com/livestats/5k5ggejrbhf/" target="_blank"><img src="//rf.revolvermaps.com/h/m/a/0/ff0000/128/0/5k5ggejrbhf.png" width="256" height="128" alt="Map" style="border:0;""></a>')
                                 # column(width=12, #box(width=NULL, height=NULL,
                                 #        
                                 #        a("Tweets by @soilspec", class="twitter-timeline", 
                                 #          href="https://twitter.com/soilspec", style = "overflow-y:scroll")
                                 #        
                                 #        #)
                                 # )
                          
                        )
                   
                        ),
                        br(),
                        br(),
                        br(),
                        br()
               )#,
               # nav_item(a(href="https://engine.soilspectroscopy.org/", "OSSL engine"))
               # tabPanel(title=HTML('<a href="https://engine.soilspectroscopy.org/"  target="_blank">', span("OSSL engine", title = "Go to the OSSL engine"), '</a>'))
               # tabPanel(span("OSSL engine", title = "Go to the OSSL engine"))
               
        ) # navbarPage,
        # a(href="https://engine.soilspectroscopy.org/", target="_blank", span("OSSL engine", title = "Go to the OSSL engine"))
    )# tagList
) # shinyUI







