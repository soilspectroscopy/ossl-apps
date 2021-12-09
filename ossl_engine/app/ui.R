# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Define user interface
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"
jscode_upload_msg <- "
Shiny.addCustomMessageHandler('upload_msg', function(msg) {
  var target = $('#input_csv_progress').children()[0];
  target.innerHTML = msg;
});
"

auth0_ui( # shinyUI
  
  tagList(
    busy_start_up(
      loader = spin_kit(spin = "circle", color = "white", style = "width:70px; height:70px;"),
      text = div(
        strong(h2("Loading data...")),
        img(src = "logos/OSSL_White_1.png", style = "align: center; height:50px;"),
        p("Follow us on ", a(href = "https://github.com/soilspectroscopy", target="_blank", img(src = "github.png", style = "align: center; height:50px;")))
      ),
      mode = "auto",
      color = "white",
      background = "#4E9522"
    ),
    tags$head(tags$script(type="text/javascript", src = "nav_engine.js")),
    tags$script(htmlwidgets::JS("setTimeout(function(){history.pushState({}, 'Page Title', window.location.pathname);},2000);")),
    tags$script(HTML(
      "document.body.style.backgroundColor = 'sapphire';"
    )),
    tags$script(HTML(
      "document.body.style.fontFamily = 'Open Sans';"
    )),
    tags$script(HTML(jscode_upload_msg)),
    
    tags$head(tags$link(rel="shortcut icon", href="logos/Design_01_200.png")),
    tags$head(HTML("<title>OSSL Estimator</title>")),
    
    tags$head(tags$script(type="text/javascript", src = "code.js")),
    tags$style("@import url(https://use.fontawesome.com/releases/v5.10.0/css/all.css);"),
    useShinyjs(),
    extendShinyjs(text = jsResetCode, functions = "reset"),
    tags$head(
      includeCSS("styles.css") # Include custom CSS
    ),
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
                tags$a(div(
                  a(href = "https://www.woodwellclimate.org/", target="_blank", img(src = "woodwell_climate_logo.png", style = "align: left; height:70px;")), 
                  a(href = "https://www.ufl.edu/", target="_blank", img(src = "uflorida_logo.png", style = "align: left; height:70px;")), 
                  a(href = "https://opengeohub.org/", target="_blank", img(src = "opengeohub_logo.png", style = "align: left; height:70px;")),
                  style="height:70px;  text-align: left;  float:left;  align: left; padding: 0px; padding-left: 20px; bottom:0; display: inline-block;"),
                  div(
                    a("USDA National Institute of Food and Agriculture Award " , style = "text-align: right; align: right; height:70px; text-decoration: none; color: black; font-size: 12px !important;"), 
                    a(href = "https://nifa.usda.gov/press-release/nifa-invests-over-7-million-big-data-artificial-intelligence-and-other", target="_blank", "# 2020-67021-32467", style = "text-align: right; align: right; height:70px;  color: black; font-weight: bold; font-size: 12px;"),
                    
                    style="height:70px;  text-align: right; float:right; align: right; padding: 0px; padding-right: 20px; bottom:0; display: inline-block; line-height: 70px")
                ),
    ), # tags$footer
    
    navbarPage(fluid = TRUE,
               id = "inTabset",
               title = a(href = "", span(img(src = "logos/OSSL_White_1.png", style = "align: center; height:30px;"), title ="Reload the app"), style = "cursor: pointer; text-decoration: none; color:white;"), 
               selected ="Estimation service", 
               # setBackgroundImage(src = "image_free3.jpg"),
               navbarMenu("", icon = icon("user-circle"),
                          tabPanel(div(span("User profile", title = "User profile"), style = "display: inline-block;"), value = "User profile", icon = icon("users"),
                                   div(
                                     id = "login-reg_user",
                                     style = "width: 700px; max-width: 100%; margin: 0 auto;",
                                     
                                     div(
                                       class = "well",
                                       h4(class = "text-center", "User informations: ", style = "color: #4E9522;"),
                                       div(style = "text-align: center;",
                                           a(href = 'https://soilspectroscopy.org/', target="_blank", img(src = "logos/Design_03.png", style = "align: center; height:50px; ")),
                                       ),
                                       p(""),
                                       hr(),
                                       htmlOutput("user_image"),
                                       p(""),
                                       
                                       h5(icon("user"), "User Name: ", textOutput(outputId = "user_text", inline = TRUE),
                                          tags$head(tags$style("#user_text{color:  #4E9522;font-size: 14px;}"))),
                                       p(""),
                                       
                                       h5(icon("id-card"), "User ID: ", textOutput(outputId = "iduser_text", inline = TRUE),
                                          tags$head(tags$style("#iduser_text{color:  #4E9522;font-size: 14px;}"))),
                                       p(""),
                                       
                                       h5(icon("id-card"), "User First Name: ", textOutput(outputId = "fuser_text", inline = TRUE),
                                          tags$head(tags$style("#fuser_text{color:  #4E9522;font-size: 14px;}"))),
                                       p(""),
                                       
                                       h5(icon("id-card"), "User Last Name: ", textOutput(outputId = "luser_text", inline = TRUE),
                                          tags$head(tags$style("#luser_text{color:  #4E9522;font-size: 14px;}"))),
                                       p(""),
                                       
                                       h5(icon("id-badge"), "User Nickname: ", textOutput(outputId = "nickuser_text", inline = TRUE),
                                          tags$head(tags$style("#nickuser_text{color:  #4E9522;font-size: 14px;}"))),
                                       p(""),
                                       
                                       h5(icon("clock"), "Profile Updated: ", textOutput(outputId = "updated_text", inline = TRUE),
                                          tags$head(tags$style("#updated_text{color:  #4E9522;font-size: 14px;}"))),
                                       p("")
                                       
                                     ),
                                     p("")
                                     
                                   ) # div
                                   
                                   
                                   
                          ), # tabPanel User profile
                          
                          tabPanel(div(span("User data panel", title = "User data panel"), style = "display: inline-block;"), value = "User data panel", icon = icon("database"),
                                   div(
                                     id = "reg_user",
                                     style = "width: 60%; max-width: 100%; margin: 0 auto;",
                                     
                                     div(
                                       class = "well",
                                       fluidRow(
                                         column(width = 5,
                                                p(""),
                                                h4(class = "text-center", strong("USER DATA PANEL"), style = "color: #4E9522;"),
                                                div(style = "text-align: center;",
                                                    a(href = 'https://soilspectroscopy.org/', target="_blank", img(src = "logos/Design_03.png", style = "align: center; height:50px; ")),
                                                ),
                                                p(""),
                                                actionButton(inputId = "back_to_estimation", label= "", class = "btn-info", icon = shiny::icon("long-arrow-alt-left")),
                                                p(""),
                                                hr(),
                                                htmlOutput("user_image1"),
                                                p(""),
                                                
                                                h5(icon("user"), "User Name: ", textOutput(outputId = "user_text1", inline = TRUE),
                                                   tags$head(tags$style("#user_text1{color:  #4E9522;font-size: 14px;}"))),
                                                p(""),
                                                
                                                h5(icon("id-card"), "User ID: ", textOutput(outputId = "iduser_text1", inline = TRUE),
                                                   tags$head(tags$style("#iduser_text1{color:  #4E9522;font-size: 14px;}"))),
                                                p(""),
                                                hr(),
                                                p("")
                                         ),
                                         column(width = 1),
                                         column(width = 6,
                                                p(""),
                                                h4(class = "text-left", strong("Available datasets in database: "), style = "color: #4E9522;"),
                                                DT::dataTableOutput("datasets") %>% withSpinner(color="#62B331"),
                                                p(""),
                                                hr(),
                                                actionButton(inputId = "load_from_panel", label= "Load selected dataset", class = "btn-success btn-block", icon = shiny::icon("cloud-upload-alt")),
                                                p(""),
                                                hr(),
                                                actionButton(inputId = "delete_from_panel", label= "Remove selected dataset", class = "btn-danger btn-block", icon = shiny::icon("trash-alt")),
                                                p("")
                                         )
                                       )
                                       
                                       # h4(class = "text-center", strong("USER DATA PANEL"), style = "color: #4E9522;"),
                                       # div(style = "text-align: center;",
                                       #     a(href = 'https://soilspectroscopy.org/', target="_blank", img(src = "logos/Design_03.png", style = "align: center; height:50px; ")),
                                       # ),
                                       # p(""),
                                       # actionButton(inputId = "back_to_estimation", label= "", class = "btn-info", icon = shiny::icon("long-arrow-alt-left")),
                                       # p(""),
                                       # hr(),
                                       # htmlOutput("user_image1"),
                                       # p(""),
                                       # 
                                       # h5(icon("user"), "User Name: ", textOutput(outputId = "user_text1", inline = TRUE),
                                       #    tags$head(tags$style("#user_text1{color:  #4E9522;font-size: 14px;}"))),
                                       # p(""),
                                       # 
                                       # h5(icon("id-card"), "User ID: ", textOutput(outputId = "iduser_text1", inline = TRUE),
                                       #    tags$head(tags$style("#iduser_text1{color:  #4E9522;font-size: 14px;}"))),
                                       # p(""),
                                       # 
                                       # hr(),
                                       # h4(class = "text-left", strong("Available datasets in database: "), style = "color: #4E9522;"),
                                       # DT::dataTableOutput("datasets") %>% withSpinner(color="#62B331"),
                                       # p(""),
                                       # hr(),
                                       # actionButton(inputId = "load_from_panel", label= "Load selected dataset", class = "btn-success btn-block", icon = shiny::icon("cloud-upload-alt")),
                                       # p(""),
                                       # hr(),
                                       # actionButton(inputId = "delete_from_panel", label= "Remove selected dataset", class = "btn-danger btn-block", icon = shiny::icon("trash-alt"))
                                     ) # div
                                   ) # div
                          ), # tabPanel User data panel
                          
                          
                          tabPanel(list(
                            # a(actionButton(inputId = "blogout", label= "Logout", class = "btn-danger btn-block", icon = shiny::icon("sign-out-alt")))
                            a(logoutButton(label = "Logout", style="background:#db4040; color:white;"), style="text-align: center;")
                          )) # tabPanel Logout
                          
               ),
               # tabPanel(title = span("Login", title = "Login and authentication"), value = "Login", 
               #          div(
               #            id = "login-basic", 
               #            style = "width: 500px; max-width: 100%; margin: 0 auto;",
               #            
               #            div(
               #              class = "well",
               #              h4(class = "text-center", "Welcome to OSSL Estimator service!", style = "color: #4E9522;"),
               #              div(style = "text-align: center;",
               #              a(href = 'https://soilspectroscopy.org/', target="_blank", img(src = "logos/Design_03.png", style = "align: center; height:50px; ")),
               #              ),
               #              p(""),
               #              h4(class = "text-center", "Please login: ", style = "color: #4E9522;"),
               #              
               #              
               #              textInput(
               #                inputId     = "ti_user_name_basic", 
               #                label       = tagList(icon("user"), 
               #                                      "User Name or E-mail address"),
               #                placeholder = "Enter user name or e-mail address"
               #              ),
               #              
               #              passwordInput(
               #                inputId     = "ti_password_basic", 
               #                label       = tagList(icon("unlock-alt"), 
               #                                      "Password"), 
               #                placeholder = "Enter password"
               #              ), 
               #              p(""),
               #              div(
               #                class = "text-center-left",
               #                textOutput(outputId = "pass_text"),
               #                tags$head(tags$style("#pass_text{color: red;
               #                   font-size: 12px;
               #                   }"
               #                )
               #                )
               #              
               #                ),
               #              p(""),
               #              div(
               #                class = "text-center",
               #                actionButton(
               #                  inputId = "ab_login_button_basic", 
               #                  label = "Log in",
               #                  class = "btn-success"
               #                )
               #              ),
               #              p(""),
               #              hr(),
               #              div(class = "text-center",
               #                  h4("New User?", 
               #                     actionButton(inputId = "ab_new_user", 
               #                                  label = "Click for registration",
               #                                  class = "btn-info"
               #                     )
               #                    , style = "color: #4E9522;")
               #              
               #              ),
               #              p(""),
               #              hr(),
               #              div(class = "text-center",
               #                  h4("Developer? Read our", a(href="https://soilspectroscopy.github.io/ossl-manual/reference-soil-spectroscopy-models.html#worked-out-examples", target="_blank", "API documentation"), style = "color: #4E9522;")
               #                
               #              )
               #            )
               #          )
               #          
               #          
               #          
               #          ), # tabPanel Login
               # 
               # 
               #   tabPanel(title = span("Registration", title = "New User Registration"), value = "Registration",
               #            div(
               #              id = "login-reg",
               #              style = "width: 500px; max-width: 100%; margin: 0 auto;",
               # 
               #              div(
               #                class = "well",
               #                h4(class = "text-center", "Registration form: ", style = "color: #4E9522;"),
               #                div(style = "text-align: center;",
               #                    a(href = 'https://soilspectroscopy.org/', target="_blank", img(src = "logos/Design_03.png", style = "align: center; height:50px; ")),
               #                ),
               #                p(""),
               # 
               # 
               # 
               #                textInput(
               #                  inputId     = "new_user_email",
               #                  label       = tagList(icon("user"),
               #                                        "E-mail address"),
               #                  placeholder = "Enter e-mail address"
               #                ),
               #                textInput(
               #                  inputId     = "new_user_first",
               #                  label       = tagList(icon("user"),
               #                                        "First name"),
               #                  placeholder = "Enter first name"
               #                ),
               #                textInput(
               #                  inputId     = "new_user_last",
               #                  label       = tagList(icon("user"),
               #                                        "Last name"),
               #                  placeholder = "Enter last name"
               #                ),
               #                passwordInput(
               #                  inputId     = "new_password_basic",
               #                  label       = tagList(icon("unlock-alt"),
               #                                        "Password"),
               #                  placeholder = "Enter password"
               #                ),
               #                passwordInput(
               #                  inputId     = "new_password_basic_rematch",
               #                  label       = tagList(icon("unlock-alt"),
               #                                        "Confirm Password"),
               #                  placeholder = "Confirm password"
               #                ),
               #                p(""),
               #                div(
               #                  class = "text-center-left",     # za ovo je ostalo da se napravi upozorenje ako ikada bude zatrebalo
               #                  textOutput(outputId = "pass_text_rematch"),
               #                  tags$head(tags$style("#pass_text_rematch{color: red;
               #                   font-size: 12px;
               #                   }"
               #                  )
               #                  )
               # 
               #                ),
               #                p(""),
               #                div(
               #                  class = "text-center",
               #                  actionButton(
               #                    inputId = "ab_submit",
               #                    label = "Submit",
               #                    class = "btn-success"
               #                  )
               #                )
               # 
               #              )
               #            )
               # 
               # 
               # 
               #   ), # tabPanel NewUser


               tabPanel(title = span("Estimation service", title = "Discover the Estimation service"), value = "Estimation service",
                        
                        fluidRow(
                          column(width = 2,
                                 
                              #hidden( 
                              div(id = "conditionalPanel0", class = "divClassSideBar",
                                  actionButton(inputId = "to_user_panel", label= "User data panel", class = "btn-info btn-block", icon = shiny::icon("database")),
                                  p(""),
                                  
                                  tags$div(style="float:right;",title="Informations before data upload.", actionButton("openInfoPanel", label = "", icon = shiny::icon("info-circle"),
                                              style = "background-color: rgba(255, 255, 255, 0.5); color:black; border:rgba(255, 255, 255, 0.5); float:right;")),
                                 p(""),
                                 h4("Spectra type: ", style = "color: #4E9522; dispaly:inline-block; font-size: 16px; padding-bottom:5px;", 
                                 prettyRadioButtons(
                                   inputId = "radioSpectra",
                                   label = "", # Spectra type: 
                                   choices = c("mir", "visnir", "visnir.mir"),
                                   shape = "round",
                                   status = "success",
                                   fill = TRUE,
                                   inline = TRUE), inline = TRUE),
                                 p(""),
                                 hr(),
                                 h4("Metadata: ", style = "color: #4E9522; font-size: 16px;"),
                                 # textInputIcon(inputId = "id_man", label = "Manufacturer: ", icon = icon("microchip"), size = "sm", placeholder = "The manufacturer of instrument"),
                                 # textInputIcon(inputId = "id_model", label = "Model: ", icon = icon("microchip"), size = "sm", placeholder = "The model of instrument"), # project-diagram
                                 selectizeInput(inputId = "id_model", 
                                                label = "Manufacturer: " , 
                                                multiple = F,
                                                choices = unique(inst$Instruments), 
                                                options = list(
                                                  create = TRUE,
                                                  placeholder = 'Instrument manufacturer/model',
                                                  onInitialize = I('function() { this.setValue(""); }')
                                                )),
                                 # textInputIcon(inputId = "id_year", label = "Year: ", icon = icon("calendar"), size = "sm", placeholder = "Year of data sampling"),
                                 # textInputIcon(inputId = "id_depth", label = "Depth [cm]: ", icon = icon("mountain"), size = "sm", placeholder = "Upper depth [cm]"),
                                 # textInputIcon(inputId = "id_lat", label = "Latitude [dd]: ", icon = icon("globe"), size = "sm", placeholder = "Latitude WGS84 [dd]"),
                                 # textInputIcon(inputId = "id_long", label = "Longitude [dd]: ", icon = icon("globe"), size = "sm", placeholder = "Longitude WGS84 [dd]"),
                                 
                                 # textInputIcon(inputId = "id_light", label = "Light source: ", icon = icon("sun"), size = "sm", placeholder = "Light source"),
                                 # textInputIcon(inputId = "id_back", label = "Background: ", icon = icon("sticky-note"), size = "sm", placeholder = "Background"),
                                 # textInputIcon(inputId = "id_sample", label = "Sample prep: ", icon = icon("creative-commons-sampling"), size = "sm", placeholder = "Sample preparation"),
                                 p(""),
                                 hr(),
                                 h4("Data upload: ", style = "color: #4E9522; font-size: 16px;"),
                                 splitLayout(cellWidths = c("90%", "10%"),
                                             uiOutput(outputId = "file_upload", inline = TRUE), 
                                             actionButton(inputId = "reset_file_upload", label= "", class = "btn-danger shiny-bound-input", icon = shiny::icon("times-circle"), style='padding:4px; font-size:80%; margin-top:5px')),
                                 #fileInput(inputId = "input_csv", label = NULL, multiple = FALSE, accept = c(".csv"), width = "100%", buttonLabel = "Browse: ", placeholder = "Select .csv file"),
                                 actionButton(inputId = "upload_input_csv", label= "Upload data now", class = "btn-success btn-block", icon = shiny::icon("file-import")),
                                 p(""),
                                 p(a(href = "https://github.com/soilspectroscopy/ossl-models/", target="_blank", "See examples."))
                                 )), # )
                          column(width = 4,
                                 shinyjs::useShinyjs(),
                                   #hidden( 
                                   div(id = "conditionalPanel1",
                                       fluidRow(
                                         absolutePanel(id = "controls", 
                                                       class = "panel panel-default", 
                                                       fixed = TRUE,
                                                       draggable = FALSE, 
                                                       top = 70, left = "auto", right = "auto", bottom = "auto",
                                                       width = 420, height = 300,
                                                       actionButton(inputId = "closePanelInfo", label= "", class = "btn-info", icon = shiny::icon("close"),
                                                                    style = "background-color: rgba(255, 255, 255, 0.5); color:black; border:rgba(255, 255, 255, 0.5); float:right;"),
                                                       p(""),
                                                       h4("Informations before data upload!", style = "color: red; font-size: 16px;"),
                                                       p("Please upload the file in .csv file type. Data must must be in rows, with columns reperesenting MIR or VISNIR spectra wavelengths."),
                                                       p(""),
                                                       div(style = "text-align: center;",
                                                           a(img(src = "data_example1.JPG", style = "align: center; height:100px; "), title = "Example of input data."),
                                                       ),
                                                       p(""),
                                                       p("Click on the button 'Upload data now' to upload the data or another to drop the uploaded data."),
                                                       
                                         )  #absolutePanel before Data upload
                                       )), # )
                                 
                                 # hidden( 
                                 #   div(id = "conditionalPanel2",
                                 #       fluidRow(
                                 #         absolutePanel(id = "controls", 
                                 #                       class = "panel panel-default", 
                                 #                       fixed = TRUE,
                                 #                       draggable = FALSE, 
                                 #                       top = 70, left = "auto", right = "auto", bottom = "auto",
                                 #                       width = 850, height = 100, 
                                 #                       p(""),
                                 #                       p(h4("Data successfully uploaded!", style = "color: #4E9522; font-size: 16px; display: inline-block;"),
                                 #                       
                                 #                       a(href = 'https://soilspectroscopy.org/', target="_blank", img(src = "logos/Design_03.png", style = "align: center; height:50px; margin-left:20px;")))
                                 #         )  #absolutePanel after Data upload
                                 #       ))),
                                 
                                 hidden( 
                                   div(id = "conditionalPanel4",
                                       fluidRow(
                                         absolutePanel(id = "controls0", 
                                                       class = "panel panel-default", 
                                                       fixed = TRUE,
                                                       draggable = FALSE, 
                                                       top = 70, left = "auto", right = "auto", bottom = "auto",
                                                       width = 850, height = 708, 
                                                       p(""),
                                                       h4("Data preparation: ", style = "color: #4E9522; font-size: 16px;"),
                                                       div(style="display:inline-block", 
                                                           prettyCheckbox(inputId = "check.del", label = "Remove first column", value = TRUE, status = "danger", shape = "square", inline = TRUE, thick = TRUE, animation = "pulse")
                                                           ),
                                                       # div(style="display:inline-block",
                                                       #     textInput(inputId = "spec_pref", label = "", placeholder = "Specify column name prefix.")
                                                       #     ),
                                                       div(style="display:inline-block",
                                                           numericInput(inputId = "spec_n_rows", label = "Number of rows to process: ", max = 100, min = 1, value = NA, step = 1)
                                                           ),
                                                       hr(),
                                                       DT::dataTableOutput("data_row_preview"),
                                                       p(""),
                                                       plotlyOutput(outputId = "plot_data", height = 300, width = 820) %>% withSpinner(color="#62B331")
                                         )  #absolutePanel after Data upload
                                       ))),
                                 
                                 
                                 #hidden(
                                   div(id = "landingpage",
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
                                                       div(img(src = "logos/OSSL.png", style = "align: center; height:50px;"), style="text-align: center;"),
                                                       h4("Open Soil Spectral Library", style = "text-align: center; color: black; font-size: 30px;"),
                                                       div(class = "containerS",style = "align: center; height: auto;",
                                                           div(strong(
                                                             p("SoilSpec4GG is pleased to release the first beta version of the OSSL explorer and calibration engine. These applications are a work in progress and we have provided these services with the hope of receiving feedback that will enable us to improve the functioning and utility of services."),
                                                             
                                                             div(a(href = 'https://soilspectroscopy.org/', target="_blank", 'SoilSpec4GG'), " is a USDA-funded Food and Agriculture Cyberinformatics Tools Coordinated Innovation Network (USDA National Institute of Food and Agriculture Award  ", a(href="https://nifa.usda.gov/press-release/nifa-invests-over-7-million-big-data-artificial-intelligence-and-other", target="_blank", "#2020-67021-32467", style = "display: inline-block; margin-right: -4px;"),
                                                               "). This project brings together soil scientists, spectroscopists, informaticians, data scientists and software engineers to overcome some of the current bottlenecks preventing wider and more efficient use of soil spectroscopy. It includes a series of working groups to address topics including calibration transfer, model choice, outreach & demonstration, and use of spectroscopy to inform global carbon cycle modeling.", style = "display: inline-block; margin-right: -4px;"),
                                                             p(""),
                                                             p("For a brief introduction to the project, please visit the ", a(href = "https://soilspectroscopy.org/", target="_blank", "https://soilspectroscopy.org/.")),
                                                             p("Need help using OSSL Explorer or have feedback? Email ", a(href = "mailto: soilspec4gg@woodwellclimate.org", "soilspec4gg@woodwellclimate.org.")),
                                                             
                                                             style = "text-align: left;"
                                                             
                                                             ))
                                                       )
                                                     ),
                                                     br(),
                                                     div(a(href = 'https://soilspectroscopy.org/', target="_blank", img(src = "logos/Design_03.png", style = "align: center; height:50px;")), style="text-align: center;")
                                       )  # absolutePanel
                                     ) # fluidRow
                                   ), # div
                                    
                                 #)
                                 
                                 hidden(
                                   div(id = "conditionalPanel8",
                                       fluidRow(
                                         absolutePanel(id = "controls4", 
                                                       class = "panel panel-default", 
                                                       fixed = TRUE,
                                                       draggable = FALSE, 
                                                       top = "15%", left = "25%", right = "auto", bottom = "auto",
                                                       width = 700, height = 460,
                                                       actionButton(inputId = "closePanelInfoGeo", label= "", class = "btn-info", icon = shiny::icon("close"),
                                                                    style = "background-color: rgba(255, 255, 255, 0.5); color:black; border:rgba(255, 255, 255, 0.5); float:right;"),
                                                       p(""),
                                                       h4("Geolocation datatable", style = "color: coral; font-size: 16px;"),
                                                       p(strong("Data must have columns id, smp_id, lay_depth_to_top, lay_depth_to_bottom, longitude.std.decimal.degrees, latitude.std.decimal.degrees;")),
                                                       p(""),
                                                       DT::dataTableOutput("table_geo")
                                                       
                                                       
                                         )  #absolutePanel before Data upload
                                       ))
                                 ),
                                 
                                 hidden(
                                   div(id = "conditionalPanel9",
                                       fluidRow(
                                         absolutePanel(id = "controls4", 
                                                       class = "panel panel-default", 
                                                       fixed = TRUE,
                                                       draggable = FALSE, 
                                                       top = "10%", left = "25%", right = "auto", bottom = "auto",
                                                       width = 750, height = 725,
                                                       actionButton(inputId = "closePanelInfoModel", label= "", class = "btn-info", icon = shiny::icon("close"),
                                                                    style = "background-color: rgba(255, 255, 255, 0.5); color:black; border:rgba(255, 255, 255, 0.5); float:right;"),
                                                       p(""),
                                                       h5("Model description: ", style = "color: #4E9522;"),
                                                       tableOutput('desc_table'),
                                                       hr(),
                                                       h5("Model evaluation: ", style = "color: #4E9522;"),
                                                       tableOutput('eval_table'),
                                                       hr(),
                                                       h5("Plot: ", style = "color: #4E9522;"),
                                                       #htmlOutput("model_image")
                                                       div (align = "center" , style = "text-align: center;",
                                                       imageOutput("model_image")) # , width = "100%", height = "100%"
                                                       
                                         )  #absolutePanel InfoModel
                                       ))
                                 )
                                 

                                 ),
                          column(width = 5,
                                 shinyjs::useShinyjs(),
                                 
                                 # hidden(
                                   div(id = "tweeterpage",
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
                                   ), # div tweeterpage
                                 # ), # hidden
                                 
                                 hidden( 
                                   div(id = "conditionalPanel3",
                                       fluidRow(
                                         absolutePanel(id = "controls", 
                                                       class = "panel panel-default", 
                                                       fixed = TRUE,
                                                       draggable = FALSE, 
                                                       top = 70, left = "auto", right = 15, bottom = "auto",
                                                       width = 450, height = "auto", 
                                                       p(""),
                                                       # p("Either select soil properties and OSSL will use all available models or you can select specific models from model registry or calculate all available properties with most appropriate model selection."),
                                                       p("Please select soil properties and OSSL will filter all available models. Alse selection is based on specifed spectra type for dataset uppon data upload. Selected model will be used for prediction of specifed variable.", style = "font-size: 11px;"),
                                                       hr(),
                                                       
                                                                   
                                                                   
                                                       
                                                       div(style="display:inline-block",
                                                           splitLayout(cellWidths = c("65%", "65%"),
                                                           prettyCheckbox(inputId = "check.geo", label = "Use geolocation", value = FALSE, status = "info", shape = "square", inline = TRUE, thick = TRUE, animation = "pulse"),
                                                           prettyRadioButtons(
                                                             inputId = "radioLib",
                                                             label = "Library: ", 
                                                             choices = c("kssl", "ossl"),
                                                             shape = "round",
                                                             status = "success",
                                                             fill = TRUE,
                                                             inline = TRUE))
                                                       ),
                                                       uiOutput(outputId = "file_upload_geo", inline = TRUE),
                                                       # h4("Select properties for prediction: ", style = "color: #4E9522; font-size: 16px;"),
                                                       # prettyCheckboxGroup(
                                                       #                    inputId = "checkgroup_properties",
                                                       #                    label = NULL,
                                                       #                    thick = TRUE,
                                                       #                    choices = c("Organic carbon", "Total nitrogen", "Total sulfur", "Sand", "Silt", "Clay"),
                                                       #                    animation = "pulse",
                                                       #                    status = "success", 
                                                       #                    bigger = TRUE),
                                                       h4("Select target variable: ", style = "color: #4E9522; font-size: 16px;"),
                                                       selectizeInput(inputId = "var_select", 
                                                                      label = "" , 
                                                                      multiple = F,
                                                                      choices = unique(soil_model_df$variable_name), #unique(df_desc_yml$var_name), 
                                                                      options = list(
                                                                        placeholder = 'Choose one of the properties for prediction',
                                                                        onInitialize = I('function() { this.setValue(""); }')
                                                                      )),
                                                       hr(),
                                                       h4("And select specific model: ", style = "color: #4E9522; font-size: 16px;"),
                                                       div(style="display: inline-block; vertical-align:top;",           
                                                       selectizeInput(inputId = "model_select", 
                                                                      label = "" , 
                                                                      multiple = F,
                                                                      choices = unique(soil_model_df$description),#paste(df_desc_yml$name,": ", df_desc_yml$desc, sep = ""), 
                                                                      options = list(
                                                                        placeholder = 'Choose one of the specific ML models',
                                                                        onInitialize = I('function() { this.setValue(""); }')
                                                                      ))),
                                                       actionButton(inputId = "openPanelInfoModel", label= "View", class = "btn-info", icon = shiny::icon("file-alt"),
                                                                    style = "display: inline-block; background-color: rgba(255, 255, 255, 0.5); color:black; border:rgba(255, 255, 255, 0.5); margin-top: 20px;"),
                                                       actionButton(inputId = "openModelLink", label= "", class = "btn-info", icon = shiny::icon("info"),
                                                                    style = "display: inline-block; background-color: rgba(255, 255, 255, 0.5); color:black; border:rgba(255, 255, 255, 0.5); margin-top: 20px;"),
                                                       uiOutput("ui_open_tab"),
                                                       # div(style="display: inline-block; margin-top:5px;", title = "Selected model description and evaluation metrics.", 
                                                       # dropdown(
                                                       #   h5("Model description: ", style = "color: #4E9522;"),
                                                       #   tableOutput('desc_table'),
                                                       #   hr(),
                                                       #   h5("Model evaluation: ", style = "color: #4E9522;"),
                                                       #   tableOutput('eval_table'),
                                                       #   
                                                       #   style = "unite", icon = icon("sticky-note"),
                                                       #   status = "success", width = "250px",
                                                       #   size = "md", up = FALSE,
                                                       #   #tooltip = tooltipOptions(title = "Selected model description.", placement = "top"),
                                                       #   animate = animateOptions(
                                                       #     enter = animations$fading_entrances$fadeInUpBig,
                                                       #     exit = animations$fading_exits$fadeOutDownBig#, 
                                                       #     #duration = 5
                                                       #   )
                                                       # )),
                                                       hr(),
                                                       actionButton(inputId = "run_models", label= "Run models now", class = "btn-success btn-block", icon = shiny::icon("r-project")),
                                                       hr(),
                                                       p("")
                                                       #h4("Or estimate all available properties: ", style = "color: #4E9522; font-size: 16px;"),
                                                       #actionButton(inputId = "run_all_prop", label= "Estimate all properties", class = "btn-danger btn-block", icon = shiny::icon("r-project"))
                                         )  #absolutePanel after Data upload
                                       ))),
                                 
                                 
                                 
                                 
                                 )
                          
                        ) # fluidRow
                        

                        
                        ), # tabPanel Estimation service
               
               tabPanel(div(span("Model performance", title = "Discover the Model performance")), value = "Model performance",
                        
                        fluidRow(
                          column(width = 6,
                              hidden(
                                 div(id = "conditionalPanel5", class = "divClassSideBar", 
                                 p(""),
                                 h4("Prediction results for specified model: ", style = "color: #4E9522; font-size: 16px;"),
                                 hr(),
                                 tableOutput('ml_res_table') %>% withSpinner(color="#62B331")
                                 ))
                              ), # column
                          
                          column(width = 6,
                                 hidden( 
                                   div(id = "conditionalPanel6",
                                       fluidRow(
                                         absolutePanel(id = "controls6_0", 
                                                       class = "panel panel-default", 
                                                       fixed = TRUE,
                                                       draggable = FALSE, 
                                                       top = 70, left = "auto", right = 15, bottom = "auto",
                                                       width = "45%", height = "auto", 
                                                       p(""),
                                                       h4("PCA plot for specified model: ", style = "color: #4E9522; font-size: 16px;"),
                                                       plotlyOutput(outputId = "plot_pca_final", height = 500, width = 600) %>% withSpinner(color="#62B331")
                                         )  #absolutePanel after Data upload
                                       ),
                                       hidden( 
                                         fluidRow(
                                           absolutePanel(id = "controls6_1", 
                                                         class = "panel panel-default", 
                                                         fixed = TRUE,
                                                         draggable = FALSE, 
                                                         top = 70, left = "auto", right = 15, bottom = "auto",
                                                         width = "45%", height = "auto", 
                                                         p(""),
                                                         h4("PCA plot for specified model: ", style = "color: #4E9522; font-size: 16px;"),
                                                         plotlyOutput(outputId = "plot_pca_final_1", height = 500, width = 600) %>% withSpinner(color="#62B331")
                                           )  #absolutePanel after Data upload
                                         ))
                                       )),
                                 
                                 
                              ) # column
                          
                          
                          ), # fluidRow
                        hidden(
                          div(id = "conditionalPanel7",

                        absolutePanel(id = "controls", 
                                      class = "panel panel-default", 
                                      fixed = TRUE,
                                      draggable = TRUE, 
                                      top = "80%", right = 20, left = "auto", bottom = "auto",
                                      width = 330, 
                                      p(""),
                                      downloadButton(outputId = "downloadData_csv", label = "Download now", class = "btn-info btn-block", icon = shiny::icon("download")),
                                      
                        )  #absolutePanel Download and clear
                        ))
                        
                        ), # tabPanel Model performance
               
               tabPanel(div(span("About", title = "About OSSL")), value = "About",
                        p(""),
                        setBackgroundImage(src = "image_free3.jpg"),
                        fluidRow(
                          column(9, align = "center-left", style = "background-color: rgba(255, 255, 255, 0.90); margin-left:10px; dispaly:inline-block;", # font-size: 14px;",
                                 p(""),
                                 strong("Background"),
                                 p(""),
                                 p("The Open Soil Spectral Library (OSSL) calibration service is developed and maintained within the",
                                   a(href = "https://soilspectroscopy.org/", target="_blank", "Soil Spectroscopy for Global Good project."), 
                                   "This is a not-for-profit Open Source, Open Data project aiming at enabling easier access to soil calibration services, data and tools."),
                                 p(""),
                                 strong("Data access and license"),
                                 p(""),
                                 p("If you do not wish to register for login to this service, we recommend you use the API or the", 
                                   a(href = "https://soilspectroscopy.github.io/ossl-manual/", target="_blank", "R documentation"),
                                   "that explains how to run calibration for large datasets. A copy of the data and models is hosted via S3 (Simple Storage Service) service and which is publicly available without restrictions."),
                                 p("If not otherwise specified the data produced and shared via Open Soil Spectral Library and tools are available under the",
                                   a(href = "https://creativecommons.org/licenses/by/4.0/", target="_blank",  "CC-BY license;"),
                                   "software is available via the",
                                   a(href = "https://opensource.org/licenses/MIT", target="_blank",  "MIT license."),
                                   "To request support please open an issue via",
                                   a(href = "https://github.com/soilspectroscopy/", target="_blank",  "https://github.com/soilspectroscopy/.")),
                                   p(strong("Design and functionality by: "), a(href = 'https://gilab.rs/', target="_blank", img(src="logo-1-1.svg"))), 
                                 p(""),
                                 strong("Citation"),
                                 p("If you make use of the OSSL, please mention hashtags #SoilSpec4GG, #soilspectroscopy and #OpenData, and of course keep us in the loop!"),
                                 p("In publications, please include some form of the following acknowledgement: “The Open Soil Spectral Library and associated services were developed with funding from USDA NIFA Award #2020-67021-32467.”"),
                                 p("Current version of the OSSL service and data:"),
                                 HTML("<ul><li>OSSL service v1.0-1,</li><li>OSSL database v1.0.</li></ul>"),
                                 p(""),
                                 strong("DISCLAIMER"),
                                 p(""),
                                 p("Last updated December 5, 2021"),
                                 p("The information provided by Open Soil Spectral Library (“we”, “us”, or “our”) on explorer.soilspectroscopy.org or engine.soilspectroscopy.org (the “Site”) is for general informational purposes only. All information on the Site and generated by the Site is provided in good faith; however, we make no representation or warranty of any kind, express or implied, regarding the accuracy, adequacy, validity, reliability, availability or completeness of any information on the Site. UNDER NO CIRCUMSTANCE SHALL WE HAVE ANY LIABILITY TO YOU FOR ANY LOSS OR DAMAGE OF ANY KIND INCURRED AS A RESULT OF THE USE OF THE SITE OR RELIANCE ON ANY INFORMATION PROVIDED. YOUR USE OF THE SITE AND YOUR RELIANCE ON ANY INFORMATION ON THE SITE OR GENERATED BY THE SITE IS SOLELY AT YOUR OWN RISK."),
                                 p(""),
                                 strong("PLEASE CONTRIBUTE"),
                                 p(""),
                                 p("The Open Soil Spectral Library is a work in progress. This first release is intended as a sounding board for feedback. The database and estimation services will continue to improve."),
                                 p("Please contribute to this project and help us make", strong("better tools for measuring and monitoring our soils and land!")),
                                 p("This is how you can contribute:"),
                                 HTML("<ol>
                                 <li>Donate your SSL data so it can be included to the OSSL. Note: we accept only correctly formatted, quality controlled soil spectral scans which come with reference values of soil properties (wet-chemistry).</li>
                                 <li>Help develop open source tools that use OSSL for both public needs and business purposes.</li> 
                                 <li>Report problems and help us debug and resolve issues. Help us make better soil data for a healthier planet!</li> 
                                      </ol>"),

                          ),
                          column(width=2,
                                 align = "center-right", 
                                 HTML('<a href="https://www.revolvermaps.com/livestats/5hnq0hual82/" target="_blank"><img src="//rf.revolvermaps.com/h/m/a/0/ff0000/128/0/5hnq0hual82.png" width="256" height="128" alt="Map" style="border:0;"></a>')
                                 # <a href="https://www.revolvermaps.com/livestats/5k5ggejrbhf/" target="_blank"><img src="//rf.revolvermaps.com/h/m/a/0/ff0000/128/0/5k5ggejrbhf.png" width="256" height="128" alt="Map" style="border:0;""></a>')
                          )
                        ),
                        br(),
                        br(),
                        br(),
                        br()
                        ) # tabPanel About
               

               
               ) # navbarPage
    ),# tagList
  info = a0_info
) # shinyUI