# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Define server logic
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

shinyServer(function(input, output, session) {
  no_all <- reactiveValues(points = NA)

  # button to new tab
  observeEvent(input$jumpToSoil, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Soil properties")
  },
  ignoreInit = TRUE)

  # landing page close

  observeEvent(input$closePanel,{
    shinyjs::hide("landingpage")
  },
  ignoreInit = TRUE)
  observeEvent(input$openLand,{
    shinyjs::show("landingpage")
  },
  ignoreInit = TRUE)


  # tweeter page close

  observeEvent(input$closePanelTweet,{
    shinyjs::hide("tweeterpage")
  },
  ignoreInit = TRUE)
  observeEvent(input$openTweet,{
    shinyjs::show("tweeterpage")
  },
  ignoreInit = TRUE)



  # OSM search trough nominatim
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  svalues <- reactive({
    # print("AAA")
    text_i <- input$search_OSM_text
    # print(text_i)
    osms <- nominatim::osm_search(text_i, limit = 5, key = mapQuestKey)
    # print("filter")
    print(osms)
    osms
  })


  # observe(svalues(), {
  observeEvent(svalues(), {
    text_i <- input$search_OSM_text
    # print(text_i)
    # svalues <- nominatim::osm_search(text_i, limit = 5, key = mapQuestKey)
    # print(svalues$display_name)
    # print(svalues)
    # print(svalues())
    # print("choices")
    choices <- c(text_i, svalues()$display_name)
    # print(choices)
    updateSelectizeInput(session = session,
                         inputId = "search_OSM_text",
                         choices = choices,
                         label = NULL,
                         # server = TRUE,
                         selected = text_i
                         # selected = NULL # tail(svalues$display_name, 1)
                         )
    input$search_OSM_text
  },
  ignoreInit = TRUE)
  
  # search_osm <- eventReactive(input$searchOSM,{
  #   text_i <- input$search_OSM
  #   search_data <- nominatim::osm_search(text_i, key = mapQuestKey) # osm_geocode
  #   if(is.null(search_data$lon)){
  #     search_data <- data.frame(lon = 0, lat = 0)
  #   }
  #   df_search <- data_frame(Long = search_data$lon, Lat = search_data$lat)
  #   # sf_search <- st_as_sf(df_search,
  #   #                       coords = c("Long", "Lat"),
  #   #                       crs = 4326)
  #   df_search
  # })

  # observeEvent(search_osm(),{
  #   print(search_osm())
  # })


  # Add search
  observeEvent(input$search_OSM, {
    text_i <- input$search_OSM_text
    search_data <- nominatim::osm_search(text_i, key = mapQuestKey) # osm_geocode
    # print(search_data)
    # if(is.null(search_data$lon)){
    if(nrow(search_data) == 0){
      # STAVI PORUKU DA JE
      # print("NEMA")
      show(id = "no_locs")
      # output$no_locs <- renderText({""})
      # return()
      # search_data <- data.frame(lon = 0, lat = 0) # ovo sredi
      # selectizeInput(inputId = "search_OSM_text"
    } else {
      hide(id = "no_locs")
      search_data <- search_data[1, ]
      search_osm <- data_frame(Long = search_data$lon, Lat = search_data$lat)
      # print(search_osm)
      # lapply(
      #   ds$drawnshapes,
      #   function(todelete) {
      #     session$sendCustomMessage(
      #       "removeleaflet",
      #       list(elid="map", layerid=todelete)
      #     )
      #   }
      # )
      # ds$drawnshapes <- list()
      # ds$drawnfeatures <- NA
      #no_all$points <- sf_dat
      # bbox_up <- st_bbox(sf_dat)

      leafletProxy("map") %>% # , data = sf_dat_unique) %>%
        # removeMarker(layerId = "osm_search") %>% # zato sto ce novi search da prepise psotojeci marker
        # clearMarkers() %>%
        # clearShapes() %>%
        # clearGeoJSON() %>%
        # #fitBounds(bbox_up[[1]], bbox_up[[2]], bbox_up[[3]], bbox_up[[4]]) %>%
        # removeDrawToolbar(clearFeatures=TRUE) %>%
        # clearGroup(group = c("one","two")) %>%
        # clearGroup(group = "x") %>%
        # addCircles(data = sf_labs_f, group = "Further registered laboratories in GLOSOLAN", fillColor = "blue", color = "blue", popup = sf_labs_f$Description) %>%
        # addCircles(data = sf_labs_n, group = "National Reference Soil Laboratories in GLOSOLAN", fillColor = "red", color = "red", popup = sf_labs_n$Description) %>%
        # addMarkers(clusterOptions = markerClusterOptions(),
        #            group = "two",
        #            layerId = sf_dat_unique$id.layer_local_c) %>%
        # addDrawToolbar(targetGroup = "x",
        #                polygonOptions = drawPolygonOptions(showArea = TRUE, shapeOptions=drawShapeOptions(fillColor = "yellow", color = "green", clickable = F)),
        #                rectangleOptions = drawRectangleOptions(showArea = TRUE, shapeOptions=drawShapeOptions(fillColor = "yellow", color = "green", clickable = F)),
        #                position = "topright",
        #                polylineOptions = FALSE,
        #                circleOptions = FALSE,
        #                circleMarkerOptions = FALSE,
        #                markerOptions = FALSE) %>%
        addCircles( lng = search_osm$Long[1], lat = search_osm$Lat[1], radius = 10000, fillColor = "yellow", color = "green", layerId = "osm_search", group="osm_search") %>%
        setView(lng = search_osm$Long[1], lat = search_osm$Lat[1], zoom = 8)
      }
    },
    ignoreInit = TRUE
  )

  # Remove Search
  observeEvent(input$remove_OSM, {
    hide(id = "no_locs")
    #updateTextInput(session = session, inputId = "search_OSM", label = "", placeholder = "Search for a geographic or administrative area", value = "")
    updateSelectizeInput(session = session,
                         inputId = "search_OSM_text",
                         choices = NULL,
                         label = NULL,
                         selected = NULL,
                         server = TRUE)#,
                         # options = list(placeholder = 'Search for a geographic or administrative area'))

    # lapply(
    #   ds$drawnshapes,
    #   function(todelete) {
    #     session$sendCustomMessage(
    #       "removeleaflet",
    #       list(elid="map", layerid=todelete)
    #     )
    #   }
    # )
    # ds$drawnshapes <- list()
    # ds$drawnfeatures <- NA
    #no_all$points <- sf_dat
    # bbox_up <- st_bbox(sf_dat_unique) # ovde map_data()!!!
    bbox_up <- st_bbox(data_all()$map_points)

    leafletProxy("map") %>% # , data = sf_dat_unique) %>%
      # clearMarkers() %>%
      # clearShapes() %>%
      # clearGeoJSON() %>%
      # removeMarker(layerId = "osm_search") %>%
      clearGroup(group = "osm_search") %>%
      fitBounds(bbox_up[[1]], bbox_up[[2]], bbox_up[[3]], bbox_up[[4]]) # %>%
      # removeDrawToolbar(clearFeatures=TRUE) %>%
      # clearGroup(group = c("one","two")) %>%
      # clearGroup(group = "x") %>%
      # addCircles(data = sf_labs_f, group = "Further registered laboratories in GLOSOLAN", fillColor = "blue", color = "blue", popup = sf_labs_f$Description) %>%
      # addCircles(data = sf_labs_n, group = "National Reference Soil Laboratories in GLOSOLAN", fillColor = "red", color = "red", popup = sf_labs_n$Description) %>%
      # addMarkers(clusterOptions = markerClusterOptions(),
      #            group = "two",
      #            layerId = sf_dat_unique$id.layer_local_c) %>%
      # addDrawToolbar(targetGroup = "x",
      #                polygonOptions = drawPolygonOptions(showArea = TRUE, shapeOptions=drawShapeOptions(fillColor = "yellow", color = "green", clickable = F)),
      #                rectangleOptions = drawRectangleOptions(showArea = TRUE, shapeOptions=drawShapeOptions(fillColor = "yellow", color = "green", clickable = F)),
      #                position = "topright",
      #                polylineOptions = FALSE,
      #                circleOptions = FALSE,
      #                circleMarkerOptions = FALSE,
      #                markerOptions = FALSE)
    },
    ignoreInit = TRUE
  )


  # dynamic selectInput based on the nation selection
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  output$state.dynamicui <- renderUI({
    req(input$nation)
    selectizeInput(
      inputId= 'sel_state',
      label = "State/territory: ",
      choices = (sp_bounds_1 %>% filter(NAME_0 %in% input$nation))$NAME_1,
      multiple = T,
      options = list(
        placeholder = 'Please select a State/territory',
        onInitialize = I('function() { this.setValue(""); }')
      ))
  })
  
  
  # Leaflet initial state of the map
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  output$map <- renderLeaflet({
    
    leaflet(options = leafletOptions(minZoom = 2)) %>% # options = leafletOptions(preferCanvas = TRUE)) %>%
      addTiles() %>%
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90)  %>%
      addMarkers(data = sf_dat_unique, clusterOptions = markerClusterOptions(), group = "one", layerId = sf_dat_unique$id.layer_local_c) %>%
      addProviderTiles("OpenStreetMap.Mapnik",group="OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery",group="Esri.WorldImagery") %>%
      addProviderTiles("Esri.DeLorme",group="Esri.DeLorme") %>%
      addProviderTiles("Esri.WorldTopoMap",group="Esri.WorldTopoMap") %>%
      addCircles(data = sf_labs_f, group = "Further registered laboratories in GLOSOLAN", fillColor = "blue", color = "blue", popup = sf_labs_f$popup) %>%
      addCircles(data = sf_labs_n, group = "National Reference Soil Laboratories in GLOSOLAN", fillColor = "red", color = "red", popup = sf_labs_n$popup) %>%
      hideGroup("Further registered laboratories in GLOSOLAN") %>%
      hideGroup("National Reference Soil Laboratories in GLOSOLAN") %>%
      addLayersControl(baseGroups = c("Esri.WorldImagery", "OpenStreetMap", "Esri.DeLorme","Esri.WorldTopoMap"), 
                       overlayGroups = c("Further registered laboratories in GLOSOLAN", "National Reference Soil Laboratories in GLOSOLAN")) %>%
      #leaflet.extras::addSearchOSM(options = searchOptions(collapsed = F, minLength = 2)) %>%
      addDrawToolbar(targetGroup = "x",
                     polygonOptions = drawPolygonOptions(showArea = TRUE, shapeOptions=drawShapeOptions(fillColor = "yellow", color = "green", clickable = F)),
                     rectangleOptions = drawRectangleOptions(showArea = TRUE, shapeOptions=drawShapeOptions(fillColor = "yellow", color = "green", clickable = F)),
                     position = "topright", 
                     polylineOptions = FALSE, 
                     circleOptions = FALSE, 
                     circleMarkerOptions = FALSE, 
                     markerOptions = FALSE) %>% 
      addResetMapButton()
  }) # renderLeaflet
  
  
  # Click on the map - feature inspection
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  observeEvent(input$map_click, {
    shinyjs::hide(id = "conditionalPanel")
    # updateSelectInput(inputId = "selectPointInfo",
    #                   label=NULL,
    #                   choices = NULL,
    #                   selected=NULL)
  },
  ignoreInit = TRUE)

  observeEvent(input$map_marker_click, {
    points_clicked = clicked_points()
    choices = unique(points_clicked$id.layer_local_c)
    
    shinyjs::show(id = "conditionalPanel")
    
    updateSelectInput(inputId = "selectPointInfo",
                      label=NULL,
                      choices = choices,
                      selected=choices[1])
  },
  ignoreInit = TRUE)
  
  # observeEvent(input$selectPointInfo, {
  #   output$point_table()
  # },
  # ignoreInit = TRUE)

  # values <- reactiveValues(
  #   which_marker = NULL
  # )
  # 
  # observe({
  #   values$which_marker <- input$map_marker_click$id
  # })

  # output$point_table <- reactive({
  # na input$selectPointInfo
  clicked_points <- reactive({
    click <- input$map_marker_click$id # values$which_marker # id.layer_local_c
    if(is.null(click)){return()}
    # pos = sf_dat
    point_click <- sf_dat_unique %>% filter(id.layer_local_c %in% click)
    if(nrow(point_click) == 0){return()}
    # ids = st_intersects(sf_dat_unique[1,], sf_dat, sparse = T)
    # ids = st_intersects(point_click, sf_dat, sparse = T)[[1]] # ovde map_data()!!!
    all_selected_points <- data_all()$points
    ids = st_intersects(point_click, all_selected_points, sparse = T)[[1]] # ovde map_data()!!!
    # points_clicked = sf_dat[ids, ] # ovde map_data()!!!
    points_clicked = all_selected_points[ids, ] # ovde map_data()!!!
    # choices = unique(points_clicked$id.layer_local_c)
    # updateSelectInput(inputId = "selectPointInfo",
    #                   label=NULL,
    #                   choices = choices,
    #                   selected=choices[1])
    points_clicked
  })
  
  # point_info <- reactive({
  output$point_table <- reactive({
    # na input$selectPointInfo
    point_id <- input$selectPointInfo
    point_click <- clicked_points()[clicked_points()$id.layer_local_c == point_id, ]
    df_f <- data.frame(Name = c("ID: ", "Soil texture: ", "Upper depth [cm]: ", "Lower depth [cm]: ",  "Address: "),
                       Values = c(point_click$id.layer_local_c[1], # vadi samo prvu!!!
                                  point_click$layer.texture_usda_c[1],
                                  point_click$layer.upper.depth_usda_cm[1],
                                  point_click$layer.lower.depth_usda_cm[1],
                                  point_click$location.address_utf8_txt[1]))
    
    
    
    df_f %>%
      kable(caption = "Attributes: ", digits = 4, align = "c", col.names = NULL) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE) #%>%
    #row_spec(2, background = "#94BB54", color = "black") %>%
    #row_spec(4, background = "#94BB54", color = "black")
  })

  mir <- reactive({
    # na input$selectPointInfo
    point_id <- input$selectPointInfo
    # point_click <- clicked_points()[clicked_points()$id.layer_local_c == point_id, ]
    # click<-input$map_marker_click
    # if(is.null(click)){return()}

    mir.data %>%
      select(c(-1:-3, -5:-16)) %>%
      filter(mir.data$id.layer_local_c %in% point_id) %>%
      select(c(-1)) %>%
      arrange()
  })

  visnir <- reactive({
    # na input$selectPointInfo
    point_id <- input$selectPointInfo
    # click<-input$map_marker_click
    # if(is.null(click)){return()}
    
    visnir.data %>%
      select(c(-1:-3, -5:-16)) %>%
      filter(visnir.data$id.layer_local_c %in% point_id) %>%
      select(c(-1)) %>%
      arrange()
  })


  #cols = brewer.pal(3, "YlOrRd")
  #pal = colorRampPalette(cols)

  # Plot
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


  output$pl.vis <- renderPlotly({
    aaa <- visnir()
    y <- unlist(aaa[, -1])
    x <- readr::parse_number(gsub(".*scan_visnir.(.*)\\_pcnt.*", "\\1", names(y)))#/1000
    #color <- rep(as.character(aaa[, 1]), ncol(aaa[, -1]))
    df <- data.frame(
      x = x,
      y = y#,
      #cut = color
    )

    # ggplot(data = df, aes(x = x, y = y)) +
    #   geom_line(colour = "blue") +
    #   labs(x = 'Wavelength [nm]', y = "Absorbance") +
    #   theme_minimal()+
    #   theme(legend.position = 'none')
    shiny::validate(
      shiny::need(nrow(df) > 0, "Non-existent VISNIR data for the selected location!")
    )
    plot_ly(df, x = ~x, y = ~y, line = list(color = 'rgb(22, 96, 167)')) %>%
      add_lines() %>%
      layout(xaxis = list(title = 'Wavelength [nm]'),
             yaxis = list(title = 'Reflectance [%]'))  %>%
      layout(autosize = TRUE, margin = list(
        l = 0, r = 0, b = 0, t = 0, pad = 0
      ))


  })

  output$pl.mir <- renderPlotly({
    aaa <- mir()
    y <- unlist(aaa[, -1])
    x <- readr::parse_number(gsub(".*scan_mir.(.*)\\_abs.*", "\\1", names(y)))#/1000
    #color <- rep(as.character(aaa[, 1]), ncol(aaa[, -1]))
    df <- data.frame(
      x = x,
      y = y#,
      #cut = color
    )
    shiny::validate(
      shiny::need(nrow(df) > 0, "Non-existent MIR data for the selected location!")
    )
    a <- ggplot(data = df, aes(x = x, y = y)) +
      geom_line(colour = "red") +
      scale_x_reverse() +
      labs(x = 'Wavenumber [cm-1]',
           y = "Absorbance") +
      theme_minimal()+
      theme(legend.position = 'none')

    ggplotly(a)

    # onRender(p, "
    #              function(el, x) {
    #                el.on('plotly_hover', function(d) {
    #                  var ann = {
    #                    text: d.points[0].customdata,
    #                    x: 0,
    #                    y: 0,
    #                    xref: 'paper',
    #                    yref: 'paper',
    #                    yshift: -40,
    #                    showarrow: false
    #                  }
    #                  Plotly.relayout(el.id, {annotations: [ann]})
    #                });
    #              }
    #            ")

    # plot_ly(df, x = ~x, y = ~y, line = list(color = 'rgb(205, 12, 24)')) %>%
    #   add_lines() %>%
    #   layout(xaxis = list(title = 'Wavelength [nm]'),
    #          yaxis = list(title = 'Absorbance')) %>%
    #   layout(autosize = TRUE, margin = list(
    #     l = 0, r = 0, b = 0, t = 0, pad = 0
    #   ))

  })


  # Draw polygon
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  observeEvent(input$draw_poly, {
    js$polygon_click()
  },
  ignoreInit = TRUE)

  observeEvent(input$draw_rectangle, {
    js$rectangle_click()
  },
  ignoreInit = TRUE)


  # https://github.com/bhaskarvk/leaflet.extras/issues/96   # timelyportfolio

  ds <- reactiveValues(drawnshapes = list(), drawnfeatures = NA)

  observeEvent(input$map_draw_new_feature,{
    # ds$drawnshapes <<- lapply(
    #    input$map_draw_new_feature$features,
    #    function(ftr) {
    #      ftr$properties$`_leaflet_id`
    #    }
    #  )


    # map_draw_all_features ima slot $features dok map_draw_new_feature nema i ide odmah na $properties$`_leaflet_id`


    ds$drawnshapes <- input$map_draw_new_feature$properties$`_leaflet_id`

    ds$drawnfeatures <- input$map_draw_new_feature

  },
  ignoreInit = TRUE)


  observeEvent(input$draw_poly | input$draw_rectangle,{
    updateSelectizeInput(session = session, inputId = 'nation', label = "Nation: ", choices = factor(sp_bounds_0$NAME_0),
                         options = list(
                           placeholder = 'Please select a Nation',
                           onInitialize = I('function() { this.setValue(""); }')
                         ))
    updateSelectizeInput(session = session, inputId= 'sel_state', label = "State/territory: ", choices = (sp_bounds_1 %>% filter(NAME_0 %in% input$nation))$NAME_1,
                         options = list(
                           placeholder = 'Please select a Nation',
                           onInitialize = I('function() { this.setValue(""); }')
                         ))
    updateSelectizeInput(session = session, inputId = 'soil_order', label = "Soil texture: ", choices = unique(soilsite.data$layer.texture_usda_c),
                         options = list(
                           placeholder = 'Please select a Soil texture',
                           onInitialize = I('function() { this.setValue(""); }')
                         ))
    # updateNumericRangeInput(session = session, inputId = "depth_range", label = "Depth range [cm]: ", value = c(min(soilsite.data$layer.lower.depth_usda_cm, na.rm = T), max(soilsite.data$layer.lower.depth_usda_cm, na.rm = T)))
    ds$drawnshapes <- list()
    ds$drawnfeatures <- NA
    #no_all$points <- sf_dat
    # updateTextInput(session = session, inputId = "search_OSM", label = "", placeholder = "Search for a geographic or administrative area", value = "")
  },
  ignoreInit = TRUE)







  # Remove polygon (also in clear selection)
  observeEvent(
    input$del_poly, {
      # updateTextInput(session = session, inputId = "search_OSM", label = "", placeholder = "Search for a geographic or administrative area", value = "")

      lapply(
        ds$drawnshapes,
        function(todelete) {
          session$sendCustomMessage(
            "removeleaflet",
            list(elid="map", layerid=todelete)
          )
        }
      )
      ds$drawnshapes <- list()
      ds$drawnfeatures <- NA
      #no_all$points <- sf_dat
      bbox_up <- st_bbox(sf_dat_unique) # ovde map_data()!!!

      leafletProxy("map", data = sf_dat_unique) %>% # ovde map_data()!!!
        clearMarkers() %>%
        clearShapes() %>%
        clearGeoJSON() %>%
        fitBounds(bbox_up[[1]], bbox_up[[2]], bbox_up[[3]], bbox_up[[4]]) %>%
        removeDrawToolbar(clearFeatures=TRUE) %>%
        clearGroup(group = c("one","two")) %>%
        clearGroup(group = "x") %>%
        addCircles(data = sf_labs_f, group = "Further registered laboratories in GLOSOLAN", fillColor = "blue", color = "blue", popup = sf_labs_f$popup) %>%
        addCircles(data = sf_labs_n, group = "National Reference Soil Laboratories in GLOSOLAN", fillColor = "red", color = "red", popup = sf_labs_n$popup) %>%
        addMarkers(clusterOptions = markerClusterOptions(),
                   group = "two",
                   layerId = sf_dat_unique$id.layer_local_c) %>% # ovde map_data()!!!
        addDrawToolbar(targetGroup = "x",
                       polygonOptions = drawPolygonOptions(showArea = TRUE, shapeOptions=drawShapeOptions(fillColor = "yellow", color = "green", clickable = F)),
                       rectangleOptions = drawRectangleOptions(showArea = TRUE, shapeOptions=drawShapeOptions(fillColor = "yellow", color = "green", clickable = F)),
                       position = "topright",
                       polylineOptions = FALSE,
                       circleOptions = FALSE,
                       circleMarkerOptions = FALSE,
                       markerOptions = FALSE)

    },
    ignoreInit = TRUE
  )



  # Spatial selection
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  # observeEvent(input$del_poly,{
  #   drawnshapes <- NULL
  # })

  drawn_poly <- reactive({

    #req(!is.null(unlist(ds$drawnshapes)))
    #ids <- unlist(ds$drawnshapes)
    #ids.d <- input$map_draw_new_feature$properties$`_leaflet_id`
    #ids.f <- intersect(ids, ids.d)
    sf.points.int <- sf_dat
    polys <- ds$drawnfeatures
    #polys <- polys[polys$properties$`_leaflet_id` %in% ids.f]
    polys <- jsonlite::toJSON(polys, auto_unbox = TRUE) # string
    polys <- geojsonio::geojson_sf(polys) # sf
    sf.points.int <- sf.points.int[polys, ] # ovde radi intersect
    # selektuj tacke na toj poziciji
    return(sf.points.int)
  })

  # output$id1 <- renderPrint({
  #   input$nation
  # })
  # output$id2 <- renderPrint({
  #   input$sel_state
  # })
  # output$id3 <- renderPrint({
  #   input$soil_order
  # })
  # output$id4 <- renderPrint({
  #   ds$drawnfeatures
  # })

  val_state <- reactiveValues(state = NULL)

  observe({
    val_state$state <- input$sel_state
  })

  data_sp_sel <- reactive({
    # nation
    # state
    sf.points.int <- sf_dat
    sel.poly <- NA

    if(input$nation != ""){
      if(!is.null(val_state$state)){
        sel.poly <- sp_bounds_1 %>%
          dplyr::filter(NAME_0 %in% input$nation) %>%
          dplyr::filter(NAME_1 %in% val_state$state)
        sf.points.int <- sf.points.int[sel.poly, ]
      } else{
        sel.poly <- sp_bounds_0 %>%
          dplyr::filter(NAME_0 %in% input$nation)
        sf.points.int <- sf.points.int[sel.poly, ] # ovde puca
      }
    } else{
      sf.points.int <- sf_dat
      sel.poly <- NA
    }
    p_list <- list(points = sf.points.int, polys = sel.poly)
    return(p_list)
  })


  observeEvent(input$nation,{
    lapply(
      ds$drawnshapes,
      function(todelete) {
        session$sendCustomMessage(
          "removeleaflet",
          list(elid="map", layerid=todelete)
        )
      }
    )
    ds$drawnshapes <- list()
    ds$drawnfeatures <- NA
    #no_all$points <- sf_dat
  },
  ignoreInit = TRUE)

  observe({
    updateSelectizeInput(
      session = session,
      inputId = 'soil_order',
      label = "Soil texture: ",
      choices = unique(data_sp_sel()$points$layer.texture_usda_c),
      options = list(
        placeholder = 'Please select a Soil texture',
        onInitialize = I('function() { this.setValue(""); }')
      ))
  })

  observe({
    updateSelectizeInput(
      session = session,
      inputId = 'library_site',
      label = "Source dataset: ",
      choices = unique(data_sp_sel()$points$dataset.title_utf8_txt),
      options = list(
        placeholder = 'Please select a Source dataset',
        onInitialize = I('function() { this.setValue(""); }')
      ))
  })

  # selekcija po atributima
  data_nonsp_sel <- reactive({

    sf.points.int <- sf_dat

    # Soil texture
    if(!is.null(input$soil_order)){
      sf.points.int %<>%
        dplyr::filter(layer.texture_usda_c %in% input$soil_order)
    }

    # soil depth
    if(!is.null(input$depth_range[1])){ # range_values$range_min
      sf.points.int %<>%
        dplyr::filter(layer.upper.depth_usda_cm >=  input$depth_range[1]) # range_values$range_min
    }

    if(!is.null(input$depth_range[2])){ # range_values$range_max
      sf.points.int %<>%
        dplyr::filter(layer.lower.depth_usda_cm <= input$depth_range[2]) # range_values$range_max)
    }

    # library
    if(!is.null(input$library_site)){
      sf.points.int %<>%
        dplyr::filter(dataset.title_utf8_txt %in% input$library_site)
    }

    p_list <- list(points = sf.points.int)
    return(p_list)

  })

  # Intersect all filters
  data_all <- reactive({
    shinyjs::hide(id = "conditionalPanel")
    id.final <- sf_dat$id.layer_local_c
    id.final <- intersect(data_nonsp_sel()$points$id.layer_local_c, id.final)

    polygons <- data_sp_sel()$polys

    if(input$nation != ""){
      id.final <- intersect(data_nonsp_sel()$points$id.layer_local_c, data_sp_sel()$points$id.layer_local_c)
    } else if (length(ds$drawnfeatures) > 1){

      polys <- ds$drawnfeatures
      polys <- jsonlite::toJSON(polys, auto_unbox = TRUE) # string
      polys <- geojsonio::geojson_sf(polys) # sf
      polygons <- polys
      id.drawn <- drawn_poly()$id.layer_local_c
      id.final <- intersect(id.drawn, data_nonsp_sel()$points$id.layer_local_c)

    }

    sf.points.int <- sf_dat %>% filter(id.layer_local_c %in% id.final)

    # Dodatna selekcija po range-u atributa iz soil properties tab-a
    soillab.data_sub <- soillab.data
    if(input$switchselection == TRUE){

      soillab.data_sub <- soillab.data_sub[soillab.data_sub[, input$var1] >= input$range_var1[1], ]
      soillab.data_sub <- soillab.data_sub[soillab.data_sub[, input$var1] <= input$range_var1[2], ]
      soillab.data_sub <- soillab.data_sub[soillab.data_sub[, input$var2] >= input$range_var2[1], ]
      soillab.data_sub <- soillab.data_sub[soillab.data_sub[, input$var2] <= input$range_var2[2], ]
      soillab.data_sub <- soillab.data_sub[soillab.data_sub[, input$var3] >= input$range_var3[1], ]
      soillab.data_sub <- soillab.data_sub[soillab.data_sub[, input$var3] <= input$range_var3[2], ]
      soillab.data_sub <- soillab.data_sub[soillab.data_sub[, input$var4] >= input$range_var4[1], ]
      soillab.data_sub <- soillab.data_sub[soillab.data_sub[, input$var4] <= input$range_var4[2], ]

      sf.points.int <- sf.points.int %>% dplyr::filter(id.layer_local_c %in% soillab.data_sub$id.layer_local_c)
    }
    
    # vrati unique locations - sf.points.map
    sf.points.map <- sf_dat_unique%>% dplyr::filter(id.layer_local_c %in% sf.points.int$id.layer_local_c)
    
    no_all$points <-  sf.points.int
    p_list <- list(points = sf.points.int, map_points = sf.points.map, polys = polygons)
    return(p_list)
  })


  observeEvent(data_all(),{

    if(length(data_all()$polys)>1){
      bbox_up <- st_bbox(data_all()$polys)

      leafletProxy("map", data = data_all()$map_points) %>%
        clearMarkers() %>%
        clearShapes() %>%
        fitBounds(bbox_up[[1]], bbox_up[[2]], bbox_up[[3]], bbox_up[[4]]) %>%
        clearGroup(group = c("one","two")) %>%
        removeDrawToolbar(clearFeatures=TRUE) %>%
        clearGroup(group = "x") %>%
        addCircles(data = sf_labs_f, group = "Further registered laboratories in GLOSOLAN", fillColor = "blue", color = "blue", popup = sf_labs_f$popup) %>%
        addCircles(data = sf_labs_n, group = "National Reference Soil Laboratories in GLOSOLAN", fillColor = "red", color = "red", popup = sf_labs_n$popup) %>%
        addMarkers(clusterOptions = markerClusterOptions(), group = "two", layerId = data_all()$map_points$id.layer_local_c) %>%
        addPolygons(data = data_all()$polys, fillColor = "yellow", color = "green") %>%
        addDrawToolbar(targetGroup = "x",
                       polygonOptions = drawPolygonOptions(showArea = TRUE, shapeOptions=drawShapeOptions(fillColor = "yellow", color = "green", clickable = F)),
                       rectangleOptions = drawRectangleOptions(showArea = TRUE, shapeOptions=drawShapeOptions(fillColor = "yellow", color = "green", clickable = F)),
                       position = "topright",
                       polylineOptions = FALSE,
                       circleOptions = FALSE,
                       circleMarkerOptions = FALSE,
                       markerOptions = FALSE)




     } else {
      bbox_up <- st_bbox(data_all()$map_points)

      leafletProxy("map", data = data_all()$map_points) %>%
        clearMarkers() %>%
        clearShapes() %>%
        fitBounds(bbox_up[[1]], bbox_up[[2]], bbox_up[[3]], bbox_up[[4]]) %>%
        clearGroup(group = c("one","two")) %>%
        addCircles(data = sf_labs_f, group = "Further registered laboratories in GLOSOLAN", fillColor = "blue", color = "blue", popup = sf_labs_f$popup) %>%
        addCircles(data = sf_labs_n, group = "National Reference Soil Laboratories in GLOSOLAN", fillColor = "red", color = "red", popup = sf_labs_n$popup) %>%
        addMarkers(clusterOptions = markerClusterOptions(), group = "two", layerId = data_all()$map_points$id.layer_local_c) #%>%
        #removeDrawToolbar(clearFeatures=TRUE) %>%
        #clearGroup(group = "x")
      # %>%
      #   addDrawToolbar(targetGroup = "x",
      #                polygonOptions = drawPolygonOptions(showArea = TRUE, shapeOptions=drawShapeOptions(fillColor = "yellow", color = "green", clickable = F)),
      #                rectangleOptions = drawRectangleOptions(showArea = TRUE, shapeOptions=drawShapeOptions(fillColor = "yellow", color = "green", clickable = F)),
      #                position = "topright",
      #                polylineOptions = FALSE,
      #                circleOptions = FALSE,
      #                circleMarkerOptions = FALSE,
      #                markerOptions = FALSE)
    }

  },
  ignoreInit = TRUE)


  # Numbers
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


  # observe({
  #   no_all$points <- data_all()$points
  # })

  output$no_pts <- output$no_pts1 <- renderText({
    dim(no_all$points)[1]
  })

  output$no_mir <- output$no_mir1 <- renderText({
    length(intersect(id.mir, no_all$points$id.layer_local_c))
  })

  output$no_vnir <- output$no_vnir1 <- renderText({
    length(intersect(id.vnir, no_all$points$id.layer_local_c))
  })

  # Data subset and download based on selection
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  # soilsite.data
  # soillab.data
  # mir.data
  # visnir.data

  output$downloadData <- output$downloadData1 <- downloadHandler(
    filename = "data.zip",
    content = function( file){
      shiny::withProgress(
        message = "Preparing data download",
        value = 0,
        {
          shiny::incProgress(1/10)
          Sys.sleep(1)
          shiny::incProgress(4/10)
          # Set temporary working directory
          # owd <- setwd( tempdir())
          # on.exit( setwd( owd))
          tmp_path <- tempdir()

          # Save the data
          sub_data <- data_all()$points

          sub_soilsite.data <- soilsite.data %>% dplyr::filter(id.layer_local_c %in% sub_data$id.layer_local_c)
          sub_soillab.data <- soillab.data %>% dplyr::filter(id.layer_local_c %in% sub_data$id.layer_local_c)
          sub_mir.data <- mir.data %>% dplyr::filter(id.layer_local_c %in% sub_data$id.layer_local_c)
          sub_visnir.data <- visnir.data %>% dplyr::filter(id.layer_local_c %in% sub_data$id.layer_local_c)

          shiny::incProgress(6/10)
          write.csv(sub_soilsite.data, paste(tmp_path, "/soilsite.data.csv", sep=""), row.names = FALSE)
          write.csv(sub_soillab.data, paste(tmp_path, "/soillab.data.csv", sep=""), row.names = FALSE)
          write.csv(sub_mir.data, paste(tmp_path, "/mir.data.csv", sep=""), row.names = FALSE)
          write.csv(sub_visnir.data, paste(tmp_path, "/visnir.data.csv", sep=""), row.names = FALSE)

          # Zip them up
          zip( file, c( paste(tmp_path, "/soilsite.data.csv", sep=""),
                        paste(tmp_path, "/soillab.data.csv", sep=""),
                        paste(tmp_path, "/mir.data.csv", sep=""),
                        paste(tmp_path, "/visnir.data.csv", sep="")),
               extras = '-j') # da ne pise celu putanju u zip, samo fajlove
          shiny::incProgress(10/10)
        }
      )

    })


  # Has VISNIR or MIR or both
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  data_all_has <- reactive({
    sub_data <- data_all()$points

    if(input$check.mir == F & input$check.vis == T){
      sub_data %<>% dplyr::filter(id_vis == TRUE)
    } else if(input$check.mir == T & input$check.vis == F){
      sub_data %<>% dplyr::filter(id_mir == TRUE)
    } else if(input$check.mir == F & input$check.vis == F){
      sub_data %<>% dplyr::filter(id_mir == FALSE) %>% dplyr::filter(id_vis == FALSE)
    } else {
      sub_data <- sub_data
    }

    return(sub_data)
  })


  # Soil Properties
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  output$var_plot1 = renderPlotly({
    #hist(sub_soillab.data[, input$var1])
    plot_ly(x = soillab.data[, input$var1], type = "histogram", marker = list(color = "#47d679", line = list(color = "black", width = 0.2))) %>%
      layout(yaxis = list(title = 'Number of samples'),
             xaxis = list(title = paste(strsplit(input$var1, "_")[[1]][1]," [", strsplit(input$var1, "_")[[1]][3], "]", sep = "")))
  })

  output$var_plot2 = renderPlotly({
    #hist(sub_soillab.data[, input$var1])
    plot_ly(x = soillab.data[, input$var2], type = "histogram", marker = list(color = "#47d679", line = list(color = "black", width = 0.2))) %>%
      layout(yaxis = list(title = 'Number of samples'),
             xaxis = list(title = paste(strsplit(input$var1, "_")[[1]][1]," [", strsplit(input$var1, "_")[[1]][3], "]", sep = "")))
  })

  output$var_plot3 = renderPlotly({
    #hist(sub_soillab.data[, input$var1])
    plot_ly(x = soillab.data[, input$var3], type = "histogram", marker = list(color = "#47d679", line = list(color = "black", width = 0.2))) %>%
      layout(yaxis = list(title = 'Number of samples'),
             xaxis = list(title = paste(strsplit(input$var1, "_")[[1]][1]," [", strsplit(input$var1, "_")[[1]][3], "]", sep = "")))
  })

  output$var_plot4 = renderPlotly({
    #hist(sub_soillab.data[, input$var1])
    plot_ly(x = soillab.data[, input$var4], type = "histogram", marker = list(color = "#47d679", line = list(color = "black", width = 0.2))) %>%
      layout(yaxis = list(title = 'Number of samples'),
             xaxis = list(title = paste(strsplit(input$var1, "_")[[1]][1]," [", strsplit(input$var1, "_")[[1]][3], "]", sep = "")))
  })




  observeEvent(input$var1,{
    range_min = min(soillab.data[, input$var1], na.rm = T)
    range_max = max(soillab.data[, input$var1], na.rm = T)
    updateNumericRangeInput(session = session, inputId = "range_var1",
                            label = "Range: ", #paste("Range [", sp_unit, "]:", sep=""),
                            value = c(range_min, range_max))
  },
  ignoreInit = TRUE)
  observeEvent(input$var2,{
    range_min = min(soillab.data[, input$var2], na.rm = T)
    range_max = max(soillab.data[, input$var2], na.rm = T)
    updateNumericRangeInput(session = session, inputId = "range_var2",
                            label = "Range: ", #paste("Range [", sp_unit, "]:", sep=""),
                            value = c(range_min, range_max))
  },
  ignoreInit = TRUE)
  observeEvent(input$var3,{
    range_min = min(soillab.data[, input$var3], na.rm = T)
    range_max = max(soillab.data[, input$var3], na.rm = T)
    updateNumericRangeInput(session = session, inputId = "range_var3",
                            label = "Range: ", #paste("Range [", sp_unit, "]:", sep=""),
                            value = c(range_min, range_max))
  },
  ignoreInit = TRUE)
  observeEvent(input$var4,{
    range_min = min(soillab.data[, input$var4], na.rm = T)
    range_max = max(soillab.data[, input$var4], na.rm = T)
    updateNumericRangeInput(session = session, inputId = "range_var4",
                            label = "Range: ", #paste("Range [", sp_unit, "]:", sep=""),
                            value = c(range_min, range_max))
  },
  ignoreInit = TRUE)

  # videti zasto trepce# input$var1, - smaknuto
  observeEvent(list(input$jumpToSoil, input$inTabset, input$range_var1), {
    if(input$inTabset == "Soil properties"){
      pts <- data_all_has()
  
      sub_soillab.data <- soillab.data %>% dplyr::filter(id.layer_local_c %in% pts$id.layer_local_c)
      ## update Range label with units and max and min
      sp_unit = strsplit(input$var1, "_")[[1]][3]

      ## update Range label with units and max and min
      range_min = ifelse(is.na(input$range_var1[1]), min(sub_soillab.data[, input$var1], na.rm = T), input$range_var1[1])
      range_max = ifelse(is.na(input$range_var1[2]), max(sub_soillab.data[, input$var1], na.rm = T), input$range_var1[2])
      p_1 = plot_ly(x = sub_soillab.data[sub_soillab.data[, input$var1] < range_max & sub_soillab.data[, input$var1] > range_min, input$var1], type = "histogram",
                  marker = list(color = "#47d679", line = list(color = "black", width = 0.2))) %>%
        layout(yaxis = list(title = 'Number of samples'),
               xaxis = list(title = paste(strsplit(input$var1, "_")[[1]][1]," [", strsplit(input$var1, "_")[[1]][3], "]", sep = "")))
      output$var_plot1 = renderPlotly({
        p_1
      })
    }
  },
  ignoreInit = TRUE)
  
  observeEvent(list(input$jumpToSoil, input$inTabset, input$range_var2), {
    if(input$inTabset == "Soil properties"){
      pts <- data_all_has()
      
      sub_soillab.data <- soillab.data %>% dplyr::filter(id.layer_local_c %in% pts$id.layer_local_c)
      ## update Range label with units and max and min
      sp_unit = strsplit(input$var2, "_")[[1]][3]
      
      ## update Range label with units and max and min
      range_min = ifelse(is.na(input$range_var2[1]), min(sub_soillab.data[, input$var2], na.rm = T), input$range_var2[1])
      range_max = ifelse(is.na(input$range_var2[2]), max(sub_soillab.data[, input$var2], na.rm = T), input$range_var2[2])
      p_2 = plot_ly(x = sub_soillab.data[sub_soillab.data[, input$var2] < range_max & sub_soillab.data[, input$var2] > range_min, input$var2], type = "histogram",
                    marker = list(color = "#47d679", line = list(color = "black", width = 0.2))) %>%
        layout(yaxis = list(title = 'Number of samples'),
               xaxis = list(title = paste(strsplit(input$var2, "_")[[1]][1]," [", strsplit(input$var2, "_")[[1]][3], "]", sep = "")))
      output$var_plot2 = renderPlotly({
        p_2
      })
    }
  },
  ignoreInit = TRUE)
  
  observeEvent(list(input$jumpToSoil, input$inTabset, input$range_var3), {
    if(input$inTabset == "Soil properties"){
      pts <- data_all_has()
      
      sub_soillab.data <- soillab.data %>% dplyr::filter(id.layer_local_c %in% pts$id.layer_local_c)
      ## update Range label with units and max and min
      sp_unit = strsplit(input$var3, "_")[[1]][3]
      
      ## update Range label with units and max and min
      range_min = ifelse(is.na(input$range_var3[1]), min(sub_soillab.data[, input$var3], na.rm = T), input$range_var3[1])
      range_max = ifelse(is.na(input$range_var3[2]), max(sub_soillab.data[, input$var3], na.rm = T), input$range_var3[2])
      p_3 = plot_ly(x = sub_soillab.data[sub_soillab.data[, input$var3] < range_max & sub_soillab.data[, input$var3] > range_min, input$var3], type = "histogram",
                    marker = list(color = "#47d679", line = list(color = "black", width = 0.2))) %>%
        layout(yaxis = list(title = 'Number of samples'),
               xaxis = list(title = paste(strsplit(input$var3, "_")[[1]][1]," [", strsplit(input$var3, "_")[[1]][3], "]", sep = "")))
      output$var_plot3 = renderPlotly({
        p_3
      })
    }
  },
  ignoreInit = TRUE)
  
  observeEvent(list(input$jumpToSoil, input$inTabset, input$range_var4), {
    if(input$inTabset == "Soil properties"){
      pts <- data_all_has()
      
      sub_soillab.data <- soillab.data %>% dplyr::filter(id.layer_local_c %in% pts$id.layer_local_c)
      ## update Range label with units and max and min
      sp_unit = strsplit(input$var4, "_")[[1]][3]
      
      ## update Range label with units and max and min
      range_min = ifelse(is.na(input$range_var4[1]), min(sub_soillab.data[, input$var4], na.rm = T), input$range_var4[1])
      range_max = ifelse(is.na(input$range_var4[2]), max(sub_soillab.data[, input$var4], na.rm = T), input$range_var4[2])
      p_4 = plot_ly(x = sub_soillab.data[sub_soillab.data[, input$var4] < range_max & sub_soillab.data[, input$var4] > range_min, input$var4], type = "histogram",
                    marker = list(color = "#47d679", line = list(color = "black", width = 0.2))) %>%
        layout(yaxis = list(title = 'Number of samples'),
               xaxis = list(title = paste(strsplit(input$var4, "_")[[1]][1]," [", strsplit(input$var4, "_")[[1]][3], "]", sep = "")))
      output$var_plot4 = renderPlotly({
        p_4
      })
    }
  },
  ignoreInit = TRUE)

  # Clear selection button and return all into inital state
  # :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  observeEvent(input$clearS | input$clearS1, {
    updateSelectizeInput(session = session, inputId = 'nation', label = "Nation: ", choices = factor(sp_bounds_0$NAME_0),
                         options = list(
                           placeholder = 'Please select a Nation',
                           onInitialize = I('function() { this.setValue(""); }')
                         ))
    updateSelectizeInput(session = session, inputId= 'sel_state', label = "State/territory: ", choices = (sp_bounds_1 %>% filter(NAME_0 %in% input$nation))$NAME_1,
                         options = list(
                           placeholder = 'Please select a Nation',
                           onInitialize = I('function() { this.setValue(""); }')
                         ))
    updateSelectizeInput(session = session, inputId = 'soil_order', label = "Soil texture: ", choices = unique(soilsite.data$layer.texture_usda_c),
                         options = list(
                           placeholder = 'Please select a Soil texture',
                           onInitialize = I('function() { this.setValue(""); }')
                         ))
    updateNumericRangeInput(session = session, inputId = "depth_range", label = "Depth range [cm]: ", value = c(min(soilsite.data$layer.lower.depth_usda_cm, na.rm = T), max(soilsite.data$layer.lower.depth_usda_cm, na.rm = T)))

    # updateSelectizeInput(
    #   session = session,
    #   inputId = 'library_site',
    #   label = "Source dataset: ",
    #   choices = unique(soilsite.data$dataset.title_utf8_txt),
    #   options = list(
    #     placeholder = 'Please select a Source dataset',
    #     onInitialize = I('function() { this.setValue(""); }')
    #   ))

    lapply(
      ds$drawnshapes,
      function(todelete) {
        session$sendCustomMessage(
          "removeleaflet",
          list(elid="map", layerid=todelete)
        )
      }
    )

    ds$drawnshapes <- list()
    ds$drawnfeatures <- NA
    #no_all$points <- sf_dat
    # NEMA POTREBE ZA OVIME JER TO RADE UPDATE SELECTION-i
    # bbox_up <- st_bbox(sf_dat_unique)
    # 
    # leafletProxy("map", data = sf_dat_unique) %>%
    #   clearMarkers() %>%
    #   clearShapes() %>%
    #   clearGeoJSON() %>%
    #   fitBounds(bbox_up[[1]], bbox_up[[2]], bbox_up[[3]], bbox_up[[4]]) %>%
    #   removeDrawToolbar(clearFeatures=TRUE) %>%
    #   clearGroup(group = c("one","two")) %>%
    #   clearGroup(group = "x") %>%
    #   addCircles(data = sf_labs_f, group = "Further registered laboratories in GLOSOLAN", fillColor = "blue", color = "blue", popup = sf_labs_f$popup) %>%
    #   addCircles(data = sf_labs_n, group = "National Reference Soil Laboratories in GLOSOLAN", fillColor = "red", color = "red", popup = sf_labs_n$popup) %>%
    #   addMarkers(clusterOptions = markerClusterOptions(),
    #              group = "two",
    #              layerId = sf_dat_unique$id.layer_local_c) %>%
    #   addDrawToolbar(targetGroup = "x",
    #                  polygonOptions = drawPolygonOptions(showArea = TRUE, shapeOptions=drawShapeOptions(fillColor = "yellow", color = "green", clickable = F)),
    #                  rectangleOptions = drawRectangleOptions(showArea = TRUE, shapeOptions=drawShapeOptions(fillColor = "yellow", color = "green", clickable = F)),
    #                  position = "topright",
    #                  polylineOptions = FALSE,
    #                  circleOptions = FALSE,
    #                  circleMarkerOptions = FALSE,
    #                  markerOptions = FALSE)

    updateMaterialSwitch(session = session, inputId = "switchselection", value = FALSE)
    updateSelectizeInput(session = session, inputId = 'var1', label = '', choices = factor(soillab.df$full_name),
                         selected =  factor(soillab.df$full_name)[27]
                         # options = list(
                         #   placeholder = 'Please select a soil property below',
                         #   onInitialize = I('function() { this.setValue(""); }')
                         # )
    )
    updateSelectizeInput(session = session, inputId = 'var2', label = '', choices = factor(soillab.df$full_name),
                         selected =  factor(soillab.df$full_name)[13]
                         # options = list(
                         #   placeholder = 'Please select a soil property below',
                         #   onInitialize = I('function() { this.setValue(""); }')
                         # )
    )
    updateSelectizeInput(session = session, inputId = 'var3', label = '', choices = factor(soillab.df$full_name),
                         selected =  factor(soillab.df$full_name)[19]
                         # options = list(
                         #   placeholder = 'Please select a soil property below',
                         #   onInitialize = I('function() { this.setValue(""); }')
                         # )
    )
    updateSelectizeInput(session = session, inputId = 'var4', label = '', choices = factor(soillab.df$full_name),
                         selected =  factor(soillab.df$full_name)[3]
                         # options = list(
                         #   placeholder = 'Please select a soil property below',
                         #   onInitialize = I('function() { this.setValue(""); }')
                         # )
    )

    updateNumericRangeInput(session = session, inputId = "range_var1", label = "Range: ", value = c(min(soillab.data[, soillab.df$full_name[27]], na.rm = T), max(soillab.data[, soillab.df$full_name[27]], na.rm = T)))
    updateNumericRangeInput(session = session, inputId = "range_var2", label = "Range: ", value = c(min(soillab.data[, soillab.df$full_name[13]], na.rm = T), max(soillab.data[, soillab.df$full_name[13]], na.rm = T)))
    updateNumericRangeInput(session = session, inputId = "range_var3", label = "Range: ", value = c(min(soillab.data[, soillab.df$full_name[19]], na.rm = T), max(soillab.data[, soillab.df$full_name[19]], na.rm = T)))
    updateNumericRangeInput(session = session, inputId = "range_var4", label = "Range: ", value = c(min(soillab.data[, soillab.df$full_name[3]], na.rm = T), max(soillab.data[, soillab.df$full_name[3]], na.rm = T)))

    # updateSelectizeInput(session = session, inputId = "search_OSM", choices = "", label = "", options = list(placeholder = 'Search for a geographic or administrative area', create = TRUE))
    #updateTextInput(session = session, inputId = "search_OSM", label = "", placeholder = "Search for a geographic or administrative area", value = "")
  },
  ignoreInit = TRUE)
  
  # output$frame <- renderUI({
  #   tags$iframe(src="//ra.revolvermaps.com/w/6/a/a2.php?i=57z5yqmd25p&m=0&c=ff0000&cr1=ffffff&l=33&s=180", height=300, width=250, style = "background-color: #fff0; border-color: #ccc0; float: right; padding-right: 35px;")
  # })
  
})
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
