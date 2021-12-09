# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Define server logic
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
destroyX = function(es) {
  f = es
  for (col in c(1:ncol(f))){ #for each column in dataframe
    if (startsWith(colnames(f)[col], "X") == TRUE)  { #if starts with 'X' ..
      colnames(f)[col] <- substr(colnames(f)[col], 2, 100) #get rid of it
    }
  }
  assign(deparse(substitute(es)), f, inherits = TRUE) #assign corrected data to original name
}

auth0_server(function(input, output, session) { # shinyServer
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Custom log in 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # login_pass <- eventReactive(input$ab_login_button_basic,{
  #   req(!is.na(input$ti_user_name_basic))
  #   req(!is.na(input$ti_password_basic))
  #   pass_user <- FALSE
  #   
  #   if(input$ti_user_name_basic %in% user && input$ti_password_basic %in% pass){
  #     pass_user <- TRUE
  #   }
  #   return(pass_user)
  # })
  
  # observe({
  #   req(!is.na(input$ti_user_name_basic))
  #   req(!is.na(input$ti_password_basic))
  #   if(login_pass() == FALSE){
  #     output$pass_text <- renderText({
  #       "Wrong User Name or Password, please enter the correct values!"
  #     })
  #   }
  # })
  
  
  # observeEvent(input$ab_new_user,{
  #   showTab(inputId = "inTabset", target = "Registration", session = session)
  #   updateTabsetPanel(session = session,
  #                     "inTabset",
  #                     selected = "Registration")
  # 
  # })
  
  # observeEvent(input$ab_submit,{
  #   hideTab(inputId = "inTabset", target = "Registration", session = session)
  #   updateTabsetPanel(session = session,
  #                     "inTabset",
  #                     selected = "Login")
  # })
  
  
  # observeEvent(input$ab_login_button_basic, {
  #   req(!is.na(input$ti_user_name_basic))
  #   req(!is.na(input$ti_password_basic))
  #   req(login_pass() == TRUE)
  #   
  #   shinyjs::show("conditionalPanel0")
  #   shinyjs::show("conditionalPanel1")
  #   updateTabsetPanel(session, 
  #                     "inTabset",
  #                     selected = "Estimation service")
  #   hideTab(inputId = "inTabset", target = "Login")
  #   hideTab(inputId = "inTabset", target = "Registration", session = session)
  #   # show welocome landing page and tweeter page
  #   shinyjs::show("landingpage")
  #   shinyjs::show("tweeterpage")
  #   
  # })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # User info panel
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$user_text <- renderText({
    session$userData$auth0_info$name
  })
  
  output$user_text1 <- renderText({
    session$userData$auth0_info$name
  })
  
  output$nickuser_text <- renderText({
    session$userData$auth0_info$nickname
  })
  
  output$iduser_text <- renderText({
    session$userData$auth0_info$sub
  })
  
  output$iduser_text1 <- renderText({
    session$userData$auth0_info$sub
  })
  
  output$fuser_text <- renderText({
    session$userData$auth0_info$given_name
  })
  
  output$luser_text <- renderText({
    session$userData$auth0_info$family_name
  })
  
  output$updated_text <- renderText({
    session$userData$auth0_info$updated_at
  })
  
  # output$user_image <- renderImage({
  #   session$userData$auth0_info$picture
  # })
  output$user_image <- renderText({
    src = session$userData$auth0_info$picture
    c('<div align = "center-right" style = "text-align: right;"> <img src="',src,'" alt="User picture" style="border-radius: 50%;"> </img> </div>')
  })
  
  output$user_image1 <- renderText({
    src = session$userData$auth0_info$picture
    c('<div align = "center-right" style = "text-align: right;"> <img src="',src,'" alt="User picture" style="border-radius: 50%;"> </img> </div>')
  })
  
  observe({
    shiny::withProgress(
      message = "Collecting user informations, please wait...",
      value = 0,
      { 
        shiny::incProgress(4/10)
        Sys.sleep(1)
        shiny::incProgress(10/10)
        Sys.sleep(0.5)
        
        print(session$userData$auth0_info)
        
        
      }
    )
    
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Panels
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # landing page close
  
  observeEvent(input$closePanel,{
    shinyjs::hide("landingpage")
  })
  observeEvent(input$openLand,{
    shinyjs::show("landingpage")
  })
  
  # tweeter page close
  
  observeEvent(input$closePanelTweet,{
    shinyjs::hide("tweeterpage")
  })
  observeEvent(input$openTweet,{
    shinyjs::show("tweeterpage")
  })
  
  # info panel close
  
  observeEvent(input$closePanelInfo,{
    shinyjs::hide("conditionalPanel1")
  })
  observeEvent(input$openInfoPanel,{
    req(is.na(filesName$file_name))
    shinyjs::show("conditionalPanel1")
  })
  
  observe({
    req(input$input_csv)
    session$sendCustomMessage("upload_msg", "Loading complete")
  })
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read input file and panels
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$file_upload <- renderUI({
    input$reset_file_upload # Create a dependency with the reset button
    input$load_from_panel
    # input$upload_input_csv
    filesName$file_name <- NA
    NamesFiles$file_name <- NA
    
    fileInput(inputId = "input_csv", label = NULL, multiple = FALSE, accept = c(".csv"), width = "100%", buttonLabel = "Browse: ", placeholder = "Select .csv file")
  })
  
  # panel with info before and after data upload - show and close
  
  filesName <- reactiveValues(file_name = NA)
  
  observeEvent(input$input_csv, { 
    shinyjs::hide("conditionalPanel4")
    shinyjs::show("conditionalPanel1")
  })
  # observeEvent(input$upload_input_csv,{
  #   shinyjs::hide("conditionalPanel1")
  # })
  
  observeEvent(input$upload_input_csv, { # input$input_csv
    
    req(input$input_csv)
    filesName$file_name <- input$input_csv$name
    print(filesName$file_name)
    if(!is.na(filesName$file_name)){
      shinyjs::hide("conditionalPanel1")
      updatePrettyRadioButtons(session = session, 
                               inputId = "radioSpectra", 
                               label = "",  
                               choices = c("mir", "visnir", "visnir.mir"),
                               inline = TRUE)
      
      updateSelectizeInput(session = session, 
                           inputId = "id_model", 
                           label = "Manufacturer: " ,
                           choices =  unique(inst$Instruments),
                           options = list(
                             create = TRUE,
                             placeholder = 'Instrument manufacturer/model',
                             onInitialize = I('function() { this.setValue(""); }')
                           ))
      # updateTextInputIcon(session = session, inputId = "id_year", label = "Year: ", placeholder = "Year of data sampling", value = "")
      # 
      # updateTextInputIcon(session = session, inputId = "id_depth", label = "Depth [cm]: ", placeholder = "Upper depth [cm]", value = "")
      # 
      # updateTextInputIcon(session = session, inputId = "id_lat", label = "Latitude [dd]: ", placeholder = "Latitude WGS84 [dd]", value = "")
      # 
      # updateTextInputIcon(session = session, inputId = "id_long", label = "Longitude [dd]: ",  placeholder = "Longitude WGS84 [dd]", value = "")
      updateNumericInput(session = session,
                         inputId = "spec_n_rows",
                         label = "Number of rows to process: ",
                         max = 100,
                         min = 1,
                         value = 5,
                         step = 1)
      NamesFiles$file_name <- NA
      # shinyjs::show("conditionalPanel2")
      # inFile <- input$input_csv
      # 
      # if (is.null(inFile))
      #   return(NULL)
      
      # napraviti sa inFile$size < 20*1024^2 a ovamo povecati
      
      # if(dim(row_data_csv)[1] > 1050){
      #   show_alert(
      #     title = "Upload too large (20MB), please refer to documentation!",
      #     text = tags$div(
      #       a(href = 'https://soilspectroscopy.org/', target="_blank", img(src = "logos/Design_03.png", style = "align: center; height:50px;"))
      #     ),
      #     html = TRUE,
      #     width = "40%",
      #     type = "danger"
      #   )
      #   return(NULL)
      # } else{
      
      show_alert(
        title = "Data successfully uploaded!",
        text = tags$div(
          a(href = 'https://soilspectroscopy.org/', target="_blank", img(src = "logos/Design_03.png", style = "align: center; height:50px;"))
        ),
        html = TRUE,
        width = "40%",
        type = "success"
      )
      
      shinyjs::show("conditionalPanel3")
      shinyjs::show("conditionalPanel4")
      shinyjs::show("conditionalPanel5")
      shinyjs::show("conditionalPanel6")
      shinyjs::show("conditionalPanel7")
      shinyjs::hide("tweeterpage")
      shinyjs::hide("landingpage")
    } else{
      return()
    }
  })
  

  
  # observeEvent(input$upload_input_csv,{
  #   # req(!is.na(filesName$file_name))
  #   if(is.na(filesName$file_name)){
  #     shinyjs::show("conditionalPanel1")
  #   } else{
  #     # shinyjs::show("conditionalPanel2")
  #     # inFile <- input$input_csv
  #     # 
  #     # if (is.null(inFile))
  #     #   return(NULL)
  #     
  #     # napraviti sa inFile$size < 20*1024^2 a ovamo povecati
  #     
  #     # if(dim(row_data_csv)[1] > 1050){
  #     #   show_alert(
  #     #     title = "Upload too large (20MB), please refer to documentation!",
  #     #     text = tags$div(
  #     #       a(href = 'https://soilspectroscopy.org/', target="_blank", img(src = "logos/Design_03.png", style = "align: center; height:50px;"))
  #     #     ),
  #     #     html = TRUE,
  #     #     width = "40%",
  #     #     type = "danger"
  #     #   )
  #     #   return(NULL)
  #     # } else{
  #     
  #     show_alert(
  #       title = "Data successfully uploaded!",
  #       text = tags$div(
  #         a(href = 'https://soilspectroscopy.org/', target="_blank", img(src = "logos/Design_03.png", style = "align: center; height:50px;"))
  #       ),
  #       html = TRUE,
  #       width = "40%",
  #       type = "success"
  #     )
  #     
  #     shinyjs::show("conditionalPanel3")
  #     shinyjs::show("conditionalPanel4")
  #     shinyjs::show("conditionalPanel5")
  #     shinyjs::show("conditionalPanel6")
  #     shinyjs::show("conditionalPanel7")
  #     shinyjs::hide("tweeterpage")
  #     shinyjs::hide("landingpage")
  #     #}
  #   }
  # })
  
  observeEvent(input$reset_file_upload,{
    shinyjs::show("conditionalPanel1")
    # shinyjs::hide("conditionalPanel2")
    shinyjs::hide("conditionalPanel3")
    shinyjs::hide("conditionalPanel4")
    shinyjs::hide("conditionalPanel5")
    shinyjs::hide("conditionalPanel6")
    shinyjs::hide("conditionalPanel7")
    shinyjs::show("landingpage")
    shinyjs::show("tweeterpage")
    filesName$file_name <- NA
    
    updatePrettyRadioButtons(session = session, 
                             inputId = "radioSpectra", 
                             label = "",  
                             choices = c("mir", "visnir", "visnir.mir"),
                             inline = TRUE)
    
    updateSelectizeInput(session = session, 
                         inputId = "id_model", 
                         label = "Manufacturer: " ,
                         choices =  unique(inst$Instruments),
                         options = list(
                           create = TRUE,
                           placeholder = 'Instrument manufacturer/model',
                           onInitialize = I('function() { this.setValue(""); }')
                         ))
    
    # updateTextInputIcon(session = session, inputId = "id_year", label = "Year: ", placeholder = "Year of data sampling", value = "")
    # 
    # updateTextInputIcon(session = session, inputId = "id_depth", label = "Depth [cm]: ", placeholder = "Upper depth [cm]", value = "")
    # 
    # updateTextInputIcon(session = session, inputId = "id_lat", label = "Latitude [dd]: ", placeholder = "Latitude WGS84 [dd]", value = "")
    # 
    # updateTextInputIcon(session = session, inputId = "id_long", label = "Longitude [dd]: ",  placeholder = "Longitude WGS84 [dd]", value = "")
    # 
    
    
    
  })
  
  # button to tab user panel
  observeEvent(input$to_user_panel, {
    updateTabsetPanel(session, 
                      "inTabset",
                      selected = "User data panel")
  })
  
  # button to new tab after model run
  observeEvent(input$run_models, {
    updateTabsetPanel(session, 
                      "inTabset",
                      selected = "Model performance")
  })
  
  # back_to_estimation panel
  observeEvent(input$back_to_estimation, {
    updateTabsetPanel(session, 
                      "inTabset",
                      selected = "Estimation service")
  })
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read csv
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  data_csv0 <- eventReactive(input$upload_input_csv,{
    req(input$input_csv)
    inFile <- input$input_csv
    NamesFiles$file_name <- NA
    if (is.null(inFile))
      return(NULL)
    
    row_data_csv <- read.csv(inFile$datapath, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    # if(dim(row_data_csv)[1] > 1050){
    #   return(NULL)
    # }else{
    row_data_csv
    #}
  })
  # observe({
  #   print(data_csv0())
  # })
  
  # observeEvent(input$upload_input_csv,{
  #   updatePrettyRadioButtons(session = session, 
  #                            inputId = "radioSpectra", 
  #                            label = "",  
  #                            choices = c("mir", "visnir", "visnir.mir"),
  #                            inline = TRUE)
  #   
  #   updateSelectizeInput(session = session, 
  #                        inputId = "id_model", 
  #                        label = "Manufacturer: " ,
  #                        choices =  unique(inst$Instruments),
  #                        options = list(
  #                          create = TRUE,
  #                          placeholder = 'Instrument manufacturer/model',
  #                          onInitialize = I('function() { this.setValue(""); }')
  #                        ))
  #   
  #   # updateTextInputIcon(session = session, inputId = "id_year", label = "Year: ", placeholder = "Year of data sampling", value = "")
  #   # 
  #   # updateTextInputIcon(session = session, inputId = "id_depth", label = "Depth [cm]: ", placeholder = "Upper depth [cm]", value = "")
  #   # 
  #   # updateTextInputIcon(session = session, inputId = "id_lat", label = "Latitude [dd]: ", placeholder = "Latitude WGS84 [dd]", value = "")
  #   # 
  #   # updateTextInputIcon(session = session, inputId = "id_long", label = "Longitude [dd]: ",  placeholder = "Longitude WGS84 [dd]", value = "")
  #   
  # })
  # 
  # 
  # data_csv <- reactive({
  #   req(data_csv0())
  #   req(!is.na(input$spec_n_rows))
  #   row_data_csv <- data_csv0() %>% 
  #     as.data.frame() %>%
  #     dplyr::slice(1:input$spec_n_rows)
  #   row_data_csv
  # })
  
  data_csv <- reactive({
    req(data_csv0())
    req(!is.na(input$spec_n_rows))
    row_data_csv <- data_csv0() %>% 
      as.data.frame() %>%
      dplyr::slice(1:input$spec_n_rows)
    row_data_csv
    
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # User data panel
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  observe({
    name_f <- session$userData$auth0_info$sub
    # name_f <- "Petar Bursac"
    f_names <- list.dirs(path = paste(data.dir, "/database/", sep=""), full.names = FALSE)
    if(!dir.exists(paste(data.dir, "/database/", name_f, sep = ""))){
      dir.create(path = paste(data.dir, "/database/", name_f, sep = ""))
      print("dir created!")
    }
    
  })
  
  NamesFiles <- reactiveValues(file_name = NA) # ili tirger dugme refresh
  
  # loaded_data_initial = # !!!
  # loaded_data_initial <- reactiveVal(NA)
  loaded_data_initial <- reactiveValues(load = NA)
  
  
  observeEvent(data_csv0(),{ # data_csv0() input$upload_input_csv
    name_f <- session$userData$auth0_info$sub
    f_list <- list.files(path = paste(data.dir, "/database/", name_f, sep = ""))
    name_file0 <-  filesName$file_name
    name_file <- filesName$file_name %>% gsub("\\..*","",.)

    if((!name_file0 %in% f_list) == TRUE){
      meta_df <- data.frame(Spectra = if(input$radioSpectra == "mir"){
        "mir"
      } else if(input$radioSpectra == "visnir") {
        "visnir"
      } else{
        "visnir.mir"
      },
      Instrument = input$id_model
      # Year = input$id_year,
      # Depth = input$id_depth,
      # Lat = input$id_lat, 
      # Long = input$id_long
      
      )
      
      print("aaa")
      res_list <- list(data = data_csv0(), meta = meta_df, time = Sys.time())
      saveRDS(object = res_list, file = paste(data.dir, "/database/", name_f, "/", name_file, ".rds", sep = ""))
      NamesFiles$file_name <- TRUE
      loaded_data_initial$load <- res_list
    } else {
      return()
    }
    
  })
  
  
  observeEvent(loaded_data_initial$load,{
    datas <- loaded_data_initial$load # !!!
    updatePrettyRadioButtons(session = session, 
                             inputId = "radioSpectra", 
                             label = "",  
                             choices = c("mir", "visnir", "visnir.mir"),
                             selected = as.character(datas$meta$Spectra),
                             inline = TRUE
    )
    
    
    
    updateSelectizeInput(session = session, 
                         inputId = "id_model", 
                         label = "Manufacturer: " ,
                         choices =  unique(inst$Instruments),
                         selected = as.character(datas$meta$Instrument)
    )
  
  }, ignoreInit = TRUE)
  
  
  # delete_from_panel (ostalo je da kada se izbrise dataset koji je ucitan da se sve resetuje...)
  observeEvent(input$delete_from_panel,{
    NamesFiles$file_name <- NA
    name_f <- session$userData$auth0_info$sub
    f_list <- list.files(path = paste(data.dir, "/database/", name_f, sep = ""))
    f_list0 <- f_list %>% gsub("\\..*","",.)
    selected <- input$datasets_rows_selected
    f_list0 %<>% as.data.frame() %>% dplyr::rename(Dataset_name = ".")
    namesSel <- f_list0$Dataset_name[selected]
    file_name <- paste(data.dir, "/database/", name_f,"/", namesSel,".rds", sep = "")
    #Check its existence
    if (file.exists(file_name)) {
      #Delete file if it exists
      file.remove(file_name)
      NamesFiles$file_name <- TRUE
    }
    
  })
  
  
  df_names <- eventReactive(NamesFiles$file_name, {
    print("lll")
    name_f <- session$userData$auth0_info$sub
    f_list <- list.files(path = paste(data.dir, "/database/", name_f, sep = ""))
    f_list0 <- f_list %>% gsub("\\..*","",.)
    
    f_list0 %<>% as.data.frame() %>% dplyr::rename(Dataset_name = ".")
    f_list0
  })
  
  
  output$datasets <- DT::renderDataTable({
    DT::datatable(
      df_names(),
      escape=F,
      extensions = list('Buttons'),
      options = list("pageLength" = 5, dom = 'Bfrtip', buttons = I('colvis'),
                     deferRender = TRUE),
      selection = list(mode = 'single')
    )
    
  })
  
  selectedData <- eventReactive(input$load_from_panel,{
    selected <- input$datasets_rows_selected
    namesSel <- df_names()$Dataset_name[selected]
    namesSel
  })
  
  
  # button to tab after selected dataset in panel
  observeEvent(input$load_from_panel, {
    updateTabsetPanel(session, 
                      "inTabset",
                      selected = "Estimation service")
  })
  
  data_loaded <- eventReactive(input$load_from_panel,{ # ovde dodati da ima uvek !!!
    req(input$datasets_rows_selected)
    name_f <- session$userData$auth0_info$sub
    f_name <- selectedData()
    if (identical(f_name,character(0)) | is.na(f_name)){
      data <- NA
    } else {
      data <- readRDS(file = paste(data.dir, "/database/", name_f, "/", f_name, ".rds", sep = ""))
    }
    loaded_data_initial$load <- data
    data
  })
  
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Render data
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  observeEvent(input$load_from_panel,{
    # req(selectedData())
    
    show_alert(
      title = "Data successfully uploaded!",
      text = tags$div(
        a(href = 'https://soilspectroscopy.org/', target="_blank", img(src = "logos/Design_03.png", style = "align: center; height:50px;"))
      ),
      html = TRUE,
      width = "40%",
      type = "success"
    )
    
    shinyjs::show("conditionalPanel3")
    shinyjs::show("conditionalPanel4")
    shinyjs::show("conditionalPanel5")
    shinyjs::show("conditionalPanel6")
    shinyjs::show("conditionalPanel7")
    shinyjs::hide("tweeterpage")
    shinyjs::hide("landingpage")
    
  })
  
  observeEvent(input$load_from_panel,{
    datas <- data_loaded() # !!!
    updatePrettyRadioButtons(session = session, 
                             inputId = "radioSpectra", 
                             label = "",  
                             choices = c("mir", "visnir", "visnir.mir"),
                             selected = as.character(datas$meta$Spectra),
                             inline = TRUE
    )
    
    
    
    updateSelectizeInput(session = session, 
                         inputId = "id_model", 
                         label = "Manufacturer: " ,
                         choices =  unique(inst$Instruments),
                         selected = as.character(datas$meta$Instrument)
    )
    
    updateNumericInput(session = session,
                       inputId = "spec_n_rows",
                       label = "Number of rows to process: ",
                       max = 100,
                       min = 1,
                       value = 5,
                       step = 1)
    
    # updateTextInputIcon(session = session, inputId = "id_year", label = "Year: ", icon = icon("calendar"), value = as.character(datas$meta$Year, placeholder = "Year of data sampling"))
    # 
    # updateTextInputIcon(session = session, inputId = "id_depth", label = "Depth [cm]: ", icon = icon("mountain"), value = as.character(datas$meta$Depth), placeholder = "Upper depth [cm]")
    # 
    # updateTextInputIcon(session = session, inputId = "id_lat", label = "Latitude [dd]: ", icon = icon("globe"), value = as.character(datas$meta$Lat), placeholder = "Latitude WGS84 [dd]")
    # 
    # updateTextInputIcon(session = session, inputId = "id_long", label = "Longitude [dd]: ", icon = icon("globe"), value = as.character(datas$meta$Long), placeholder = "Longitude WGS84 [dd]")
    # 
    
    # print(c(as.character(datas$meta$Year), as.character(datas$meta$Instrument), as.character(datas$meta$Spectra), as.character(datas$meta$Depth), as.character(datas$meta$Lat), as.character(datas$meta$Long)))
  })
  
  
  data_final0 <- reactive({
    print("pera")
    if(is.na(filesName$file_name)){
      print("pera1")
      data_f <- data_loaded()$data %>% 
        as.data.frame() 
      print("pera2")
    } else{
      print("pera3")
      data_f <- data_csv()
    }
    print("pera4")
    data_f
  })

  
  
  data_final <- reactive({
    print("pera5")
    req(!is.na(input$spec_n_rows))
    print(input$spec_n_rows)
    data_f <- data_final0() %>%
      as.data.frame() %>%
      dplyr::slice(1:input$spec_n_rows)
    data_f
  })
  
  
  output$data_row_preview <- DT::renderDataTable({
    
    DT::datatable(
      destroyX(data_final()),
      filter = 'none',
      extensions = c('Scroller'),
      options = list(scrollY = 120,
                     scrollX = 600,
                     deferRender = TRUE,
                     scroller = TRUE,
                     ordering = FALSE,
                     # paging = TRUE,
                     # pageLength = 25,
                     # buttons = list('excel',
                     #                list(extend = 'colvis', targets = 0, visible = FALSE)),
                     dom = 'lrtip', #  lBfrtip # Learn more on dom here: https://datatables.net/reference/option/dom
                     fixedColumns = TRUE), 
      rownames = FALSE) %>%
      formatRound(columns=names(destroyX(data_final()))[2:length(names(destroyX(data_final())))], digits=3)
  })
  
  
  output$plot_data <- renderPlotly({
    req(!is.na(input$spec_n_rows))
    if(input$check.del == TRUE){
      row_data_csv <- data_final() %>%
        dplyr::select(-1) %>%
        arrange()
    } else {
      row_data_csv <- data_final() %>% 
        arrange()
    }
    
    df <- ml.predict.preprocessing(newdata = row_data_csv, spec.prefix = "X", no_rows = input$spec_n_rows,  rm.fcol = input$check.del) 
    df1 <- df %>% melt() 
    df1 %<>% mutate(id = rep(1:input$spec_n_rows, length(names(df))))
    
    # df <- data.frame(
    #   x = rep(as.numeric(gsub( "X", '', names(row_data_csv))), input$spec_n_rows), # input$spec_pref
    #   y = unlist(row_data_csv[1:input$spec_n_rows, ]),
    #   id = rep(1:input$spec_n_rows, each = length(names(row_data_csv)))
    # )
    
    # if mir or vnir!!!
    
    plot_ly(df1, x = ~variable, y = ~value, color = ~factor(id)) %>% # rev
      add_lines() %>%
      # layout(xaxis = list(title = 'Wavenumber [cm-1]', showspikes = TRUE), 
      #        yaxis = list(title = 'Absorbance', showspikes = TRUE))  %>% 
      layout(xaxis = list(title = 'Wavelength [nm]', showspikes = TRUE), 
             yaxis = list(title = 'Reflectance', showspikes = TRUE))  %>% 
      layout(autosize = TRUE,hovermode = 'compare', margin = list(
        l = 0, r = 0, b = 0, t = 0, pad = 0
      ))
  })
  
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Models and prediction
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  observeEvent(data_final(),{
    spec <- loaded_data_initial$load$meta$Spectra

    filter_soil <- soil_model_df %>% dplyr::filter(spectra_type == spec)# input$radioSpectra)
    updateSelectizeInput(inputId = "var_select", 
                         session = session,
                         label = "" ,
                         choices = unique(filter_soil$variable_name),
                         options = list(
                           placeholder = 'Choose one of the properties for prediction',
                           onInitialize = I('function() { this.setValue(""); }')
                         ))
    
    
  }) # ,ignoreInit = TRUE)
  
  toListen <- reactive({
    list(input$var_select, input$radioLib, input$check.geo, loaded_data_initial$load) # , input$radioSpectra
  })
  
  observeEvent(toListen(), {
    req(input$var_select)

    if(input$check.geo == TRUE){
      geo_s = "ll"
    } else{
      geo_s = "na"
    }
    
    spec <- loaded_data_initial$load$meta$Spectra
    
    filter_soil <- soil_model_df %>% dplyr::filter(spectra_type == spec & variable_name == input$var_select & subset == input$radioLib & geo == geo_s)
    
    updateSelectizeInput(inputId = "model_select", 
                         session = session,
                         label = "" ,
                         choices = unique(filter_soil$description),
                         options = list(
                           placeholder = 'Choose one of the specific ML models',
                           onInitialize = I('function() { this.setValue(""); }')
                         ))
    
    
  }, ignoreInit = TRUE)
  
  output$file_upload_geo <- renderUI({
    req(input$check.geo == TRUE)
    tagList(
      actionButton(inputId = "openPanelInfoGeo", label= "View table", class = "btn-info", icon = shiny::icon("file-alt"),
                   style = "background-color: rgba(255, 255, 255, 0.5); color:black; border:rgba(255, 255, 255, 0.5); float:right;"),
      fileInput(inputId = "input_csv_geo", label = "Upload geolocation data", multiple = FALSE, accept = c(".csv"), width = "70%", buttonLabel = "Browse: ", placeholder = "Select .csv file")
      
    )
  })
  
  geo_table <- eventReactive(input$input_csv_geo,{
    inFile <- input$input_csv_geo
    if (is.null(inFile))
      return(NULL)
    row_data_csv <- read.csv(inFile$datapath, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    row_data_csv %<>% as.data.frame() %>% dplyr::rename(id = "X")
    row_data_csv
  })
  
  geo_table_raw <- eventReactive(input$input_csv_geo,{
    inFile <- input$input_csv_geo
    if (is.null(inFile))
      return(NULL)
    row_data_csv <- read.csv(inFile$datapath, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    row_data_csv %<>% as.data.frame() 
    row_data_csv
  })
  
  
  output$table_geo <- DT::renderDataTable({
    DT::datatable(
      geo_table(),
      escape=F,
      filter = 'none',
      extensions = c('Scroller'),
      options = list(scrollY = 220,
                     scrollX = 600,
                     "pageLength" = 5, dom = 'Bfrtip', buttons = I('colvis'),
                     deferRender = TRUE, fixedColumns = TRUE)
    )
    
  })
  
  observeEvent(input$input_csv_geo,{
    shinyjs::show("conditionalPanel8")
  }, ignoreInit = TRUE)
  
  observeEvent(input$closePanelInfoGeo,{
    shinyjs::hide("conditionalPanel8")
  }, ignoreInit = TRUE)
  
  observeEvent(input$openPanelInfoGeo,{
    shinyjs::show("conditionalPanel8")
  }, ignoreInit = TRUE)
  
  
  
  # Model description
  
  observeEvent(input$closePanelInfoModel,{
    shinyjs::hide("conditionalPanel9")
  }, ignoreInit = TRUE)
  
  observeEvent(input$openPanelInfoModel,{
    req(input$model_select)
    shinyjs::show("conditionalPanel9")
  }, ignoreInit = TRUE)
  
  
  
  filtered_model <- eventReactive(input$model_select,{
    req(input$model_select)
    if(input$check.geo == TRUE){
      geo_s = "ll"
    } else{
      geo_s = "na"
    }
    id_m <- input$model_select
    spec <- loaded_data_initial$load$meta$Spectra
    
    t_desc <- soil_model_df %>% dplyr::filter(description == id_m & spectra_type == spec & variable_name == input$var_select & subset == input$radioLib & geo == geo_s)
    t_desc
  })
  
  
  
  
  
  output$desc_table <- reactive({
    req(input$model_select)
    t_desc <- filtered_model()
    
    t_desc %<>% dplyr::select(model, variable_name, description)
    t_desc %>%
      kable(align = "c") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)
  })
  
  output$eval_table <- reactive({
    req(input$model_select)
    t_eval <- filtered_model()
    
    t_eval %<>% dplyr::select(R_square, RSME, N)
    t_eval %>%
      kable(digits = 2, align = "c") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)
    
  })
  
  output$model_image <- renderImage({
    req(input$model_select)
    t_eval <- filtered_model()
    filename <- paste(data.dir, "/ossl_models/", t_eval$variable, "/ap.", t_eval$model,".png", sep = "")
    shiny::validate(
      shiny::need(file.exists(filename), "No image available for chosen model!"
      )
    )
    #if (file.exists(filename)) {
    list(src = filename, alt = "Model plot", width = 400, height = 400)
    #} else{
    #  stop("No image available for chosen model!")
    #}
    
  }, deleteFile = FALSE)
  
  # observeEvent(input$openModelLink,{
  #   req(input$model_select)
  #   t_eval <- filtered_model()
  #   filename <- t_eval$variable_url
  #   onclick("openModelLink", runjs("window.open(filename', '_blank')"))
  #   
  # })
  butt <- reactiveVal(FALSE)
  
  observeEvent(input$openModelLink,{
    butt(TRUE) 
  })
  
  
  output$ui_open_tab <- renderUI({
    req(butt() == TRUE)
    t_eval <- filtered_model()
    filename <- t_eval$variable_url
    tags$script(paste0("window.open('", filename, "', '_blank')"))
  })
  
  observeEvent(input$model_select,{
    butt(FALSE)
  })
  
  # # MODEL 
  # model_in <- reactive({
  #   
  #   
  # })
  
  
  
  # output$model_image <- renderText({
  #   req(input$model_select)
  #   t_eval <- filtered_model()
  # 
  #   filename <- paste("soilspectroscopy/ossl_models/", t_eval$variable, "/ap.", t_eval$model,".png", sep = "")
  #   src = filename
  #   c('<div align = "center" style = "text-align: center;"> <img src="',src,'" alt="Model plot" style="width:250px; height:250px;"> </img> </div>')
  # })
  
  
  # 
  # observe({
  #   req(input$model_select)
  #   updateSelectizeInput(inputId = "var_select", session = session, 
  #                        label = "" ,
  #                        choices = unique(df_desc_yml$var_name), 
  #                        options = list(
  #                          placeholder = 'Choose one of the properties for prediction',
  #                          onInitialize = I('function() { this.setValue(""); }')
  #                        ))
  # })
  # 
  # observe({
  #   req(input$var_select)
  #   updateSelectizeInput(inputId = "model_select", session = session, 
  #                        label = "" ,
  #                        choices = paste(df_desc_yml$name,": ", df_desc_yml$desc, sep = ""), 
  #                        options = list(
  #                          placeholder = 'Choose one of the specific ML models',
  #                          onInitialize = I('function() { this.setValue(""); }')
  #                        ))
  #   
  # })
  # 
  # data_prepro <- eventReactive(input$run_models,{
  #   req(!is.na(input$spec_n_rows))
  #   data_prep <- ml.predict.preprocessing(newdata = data_final(), spec.prefix = "X", no_rows = input$spec_n_rows, rm.fcol = input$check.del) # input$spec_pref
  #   return(data_prep)
  # })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prediction results for specified model
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  # ml_model_result <- reactive({
  #   req(input$model_select)
  #   id_m <- input$model_select %>% gsub(":.*","",.)
  #   mres <- ml.predict(models = models, modelId = id_m, newData = data_prepro())
  #   return(mres)
  # })
  
  ml_model_result <- eventReactive(input$run_models,{

    req(input$model_select)
    t_model <- filtered_model()
    raw.data <- data_final()

    if (t_model$pca.model.mir != "" & t_model$pca.model.visnir != ""){
      shinyjs::show("controls6_1")
    }

    
    ossl.model <-  readRDS(paste(data.dir, "/ossl_models/", t_model$variable, "/", t_model$model, sep = ""))
    
    if(input$check.geo == TRUE){
      geo_s = "ll"
    } else{
      geo_s = "na"
    }
    
    spec <- loaded_data_initial$load$meta$Spectra

    if(spec == "mir"){
      if(geo_s == "na"){
        if(input$radioLib == "kssl"){
          ossl.pca.mir <- readRDS(paste(data.dir, "/ossl_models/pca.ossl/mpca_mir_kssl_v1.rds", sep=""))
        } else{
          ossl.pca.mir <- readRDS(paste(data.dir, "/ossl_models/pca.ossl/mpca_mir_ossl_v1.rds", sep=""))
          
        }
        
        mres <- predict.ossl(t.var= t_model$variable, mir.raw=raw.data, ossl.model=ossl.model, ossl.pca.mir=ossl.pca.mir, ylim=c(0,100))
        
      } else{
        
        locs <- geo_table_raw()
        lon = locs$longitude.std.decimal.degrees
        lat = locs$latitude.std.decimal.degrees
        hzn_depth = locs$lay_depth_to_top + (locs$lay_depth_to_bottom - locs$lay_depth_to_top)/2
        
        
        if(input$radioLib == "kssl"){
          ossl.pca.mir <- readRDS(paste(data.dir, "/ossl_models/pca.ossl/mpca_mir_kssl_v1.rds"), sep="")
        } else{
          ossl.pca.mir <- readRDS(paste(data.dir, "/ossl_models/pca.ossl/mpca_mir_ossl_v1.rds"), sep="")
          
        }
        
        mres <- predict.ossl(t.var=t_model$variable, mir.raw=raw.data, ossl.model=ossl.model, ylim=c(0,100),
                             ossl.pca.mir=ossl.pca.mir, geo.type="ll", lon=lon, lat=lat, hzn_depth=hzn_depth, cog.dir = paste(data.dir, "/layers1km/", sep=""))
        
      }
      
      
    } else if (spec == "visnir"){
      
      if(geo_s == "na"){
        ossl.pca.visnir = readRDS(paste(data.dir, "/ossl_models/pca.ossl/mpca_visnir_ossl_v1.rds", sep=""))
        mres <- predict.ossl(t.var= t_model$variable, visnir.raw=raw.data, ossl.model=ossl.model, ossl.pca.visnir = ossl.pca.visnir, ylim=c(0,100), spc.type = "visnir")
        
      } else{
        
        locs <- geo_table_raw()
        lon = locs$longitude.std.decimal.degrees
        lat = locs$latitude.std.decimal.degrees
        hzn_depth = locs$lay_depth_to_top + (locs$lay_depth_to_bottom - locs$lay_depth_to_top)/2
        
        
        ossl.pca.visnir = readRDS(paste(data.dir, "/ossl_models/pca.ossl/mpca_visnir_ossl_v1.rds", sep=""))
        mres <- predict.ossl(t.var=t_model$variable, visnir.raw=raw.data, ossl.model=ossl.model, ylim=c(0,100),
                             ossl.pca.visnir = ossl.pca.visnir, geo.type="ll", lon=lon, lat=lat, hzn_depth=hzn_depth, cog.dir = paste(data.dir, "/layers1km/", sep=""))
      }
      
      
    } else{
      return()
    }
    
    
    return(mres)
  })
  
  
  
  output$ml_res_table <- reactive({
    req(input$model_select)
    ml_model_result()$pred %>% as.data.frame() %>% mutate(id = 1:nrow(.)) %>% select(id, everything()) %>%
      kable(align = "c") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # PCA plot
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  # MIR or VNIR
  output$plot_pca_final <- renderPlotly({
    req(ml_model_result())
    
    pca_model_plot <- readRDS(paste(data.dir, filtered_model()$pca_plot_rds, sep=""))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (filtered_model()$spectra_type == "visnir.mir") {
      pca_model_plot <- pca_model_plot$mir # samo MIR
    }
    if (filtered_model()$spectra_type == "mir" | filtered_model()$spectra_type == "visnir.mir") {
      pc1 <- ml_model_result()$x$mir.PC1
      pc2 <- ml_model_result()$x$mir.PC2
    } else {
      pc1 <- ml_model_result()$x$visnir.PC1
      pc2 <- ml_model_result()$x$visnir.PC2
    }
    print(names(ml_model_result()))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # aaa = data.frame(PC1 = c(5,10,15,20), PC2 = c(5,10,15,20))
    aaa = data.frame(PC1 = pc1, PC2 = pc2)
    
    # Predpostavljam ovako nesto
    plot_final <- pca_model_plot +
      geom_point(data=aaa, aes(x=PC1, y = PC2), pch=17, colour = "black")# +
      # coord_fixed()
    
    # Ova funkcija konverrtuje u plotly, nazivi osa i title mogu da se podese u ggplotu
    plotly::ggplotly(plot_final)
    
  })
  
  # VNIR - when both MIR and VNIR are present
  output$plot_pca_final_1 <- renderPlotly({
    req(ml_model_result())
    
    pca_model_plot <- readRDS(paste(data.dir, filtered_model()$pca_plot_rds, sep=""))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (filtered_model()$spectra_type == "visnir.mir") {
      pca_model_plot <- pca_model_plot$vnir # samo VISNIR
    } else {return()}
    pc1 <- ml_model_result()$x$visnir.PC1
    pc2 <- ml_model_result()$x$visnir.PC2
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    aaa = data.frame(PC1 = pc1, PC2 = pc2)
    
    # Predpostavljam ovako nesto
    plot_final <- pca_model_plot +
      geom_point(data=aaa, aes(x=PC1, y = PC2), pch=17, colour = "black") #+
      # coord_fixed()
    
    # Ova funkcija konverrtuje u plotly, nazivi osa i title mogu da se podese u ggplotu
    plotly::ggplotly(plot_final)

  })
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Download prediction results
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # txt_data <- eventReactive(input$run_models,{
  #   t_model <- filtered_model()
  #   txt_data <- read_lines(paste("soilspectroscopy/ossl_models/", t_model$variable,"/", str_replace(t_model$model, ".rds", "_resultsFit.txt"), sep = ""))
  #   txt_data
  # })
  
  output$downloadData_csv <- downloadHandler(
    filename = "prediction_results.zip",
    content = function( file){
      
      shiny::withProgress(
        message = "Preparing data download",
        value = 0,
        {
          shiny::incProgress(4/10)
          Sys.sleep(1)
          # Set temporary working directory
          # owd <- setwd( tempdir())
          # on.exit( setwd( owd))
          tmp_path <- tempdir()
          shiny::incProgress(6/10)
          Sys.sleep(1)
          
          
          # Save the data
          sub_data <- ml_model_result()$pred %>% as.data.frame() %>% mutate(id = 1:nrow(.)) %>% select(id, everything())
          write.csv(sub_data, paste(tmp_path, "/prediction_results.csv", sep=""), row.names = FALSE)
          
          # 1
          # txt_data_raw <- txt_data()
          # fileConn <- file("model_metadata.txt")
          # write_lines(txt_data_raw, fileConn)
          # close(fileConn)
          
          # 2
          t_model <- filtered_model()
          # GET(t_model$model_summary, "model_summary.txt")
          # http://s3.us-east-1.wasabisys.com/soilspectroscopy/ossl_models/log..oc_usda.calc_wpct/mir_mlr..eml_kssl_na_v1_resultsFit.txt
          
          
          # Zip them up
          # Zip them up
          zip( file, c( paste(tmp_path, "/prediction_results.csv", sep=""),
                        paste(data.dir, "/ossl_models/", t_model$variable, "/", strsplit(t_model$model, ".rds")[1], "_resultsFit.txt", sep = "")),
               extras = '-j') # da ne pise celu putanju u zip, samo fajlove
          shiny::incProgress(10/10)
          
        } 
      )
      
    })
  
  
  # observeEvent(input$blogout,{
  #   shinyjs::js$reset()
  # })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Revolver map
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # output$frame <- renderUI({
  #   tags$iframe(src="//ra.revolvermaps.com/w/6/a/a2.php?i=57z5yqmd25p&m=0&c=ff0000&cr1=ffffff&l=33&s=180", height=300, width=240, style = "background-color: #fff0; border-color: #ccc0; float: right; padding-right: 0px;")
  # })
  
}, info = a0_info)