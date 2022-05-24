# Server

shinyServer(function(input, output, session) {
    
    #observeEvent(input$DASFilter, {
    #    print(paste0("You have chosen: ", input$DASFilter))
    #    print(paste0("tipo: ", typeof(input$DASFilter)))
    #    
    #})
    
    ## GisaidReactive
    #gisaid_reactive = reactive({
    #    data = gisaid %>%
            #require(
                #input$fechaReporte[1], 
                #input$fechaReporte[2], 
                #input$DASFilter,
                #input$DMSFilter,
                #) %>%
    #        dplyr::filter(
    #            #if(input$DASFilter != 'TODOS')  (departamento %like% input$DASFilter) else TRUE,
    #            if(!'TODOS' %in% input$DASFilter)  (departamento %in% input$DASFilter) else TRUE,
    #            if(input$DMSFilter != 'TODOS')  (location %like% input$DMSFilter) else TRUE,
    #            if(input$VarianteFilter != 'TODOS')  (Variante %like% input$VarianteFilter) else TRUE,
    #            if(input$originatingLabFilter != 'TODOS')  (originating_lab %like% input$originatingLabFilter) else TRUE,
    #            if(input$submittingLabFilter != 'TODOS')  (submitting_lab %like% input$submittingLabFilter) else TRUE,
    #            date >= format(input$fechaReporte[1]),
    #            date <= format(input$fechaReporte[2])
                
                #date >= format(input$fechaReporte[1]) & date <= format(input$fechaReporte[2])
               # )
            #filter(
            #    date >= as.Date(format(input$fechaReporte[1])) & date <= as.Date(format(input$fechaReporte[2]))
            #)
    #})
    
    ## LNS REACTIVE
    lns_reactive = reactive({
        data = lns %>%
          dplyr::filter(
                        #if(input$DASFilter != 'TODOS')  (departamento %like% input$DASFilter) else TRUE,
                        if(!'TODOS' %in% input$DASFilter)  (`ÁREA DE SALUD` %in% input$DASFilter) else TRUE,
                        #if(input$DMSFilter != 'TODOS')  (`DISTRITO/HOSPITAL` %like% input$DMSFilter) else TRUE,
                        if(!'TODOS' %in% input$DMSFilter)  (`DISTRITO/HOSPITAL` %in% input$DMSFilter) else TRUE,
                        #if(input$VarianteFilter != 'TODOS')  (Variante %like% input$VarianteFilter) else TRUE,
                        if(!'TODOS' %in% input$VarianteFilter)  (Variante %in% input$VarianteFilter) else TRUE,
                        #if(input$originatingLabFilter != 'TODOS')  (originating_lab %like% input$originatingLabFilter) else TRUE,
                        #if(input$submittingLabFilter != 'TODOS')  (submitting_lab %like% input$submittingLabFilter) else TRUE,
                        `FECHA INGRESO DE MUESTRA` >= format(input$fechaReporte[1]),
                        `FECHA INGRESO DE MUESTRA` <= format(input$fechaReporte[2])
                        )
    })
    
    ## Filtros
    dases = reactive({
        lns %>%
            select(`ÁREA DE SALUD`) %>%
            unique() %>%
            arrange(`ÁREA DE SALUD`)
    })
    
    dasList = reactive({
        append(c("TODOS"),dases()$`ÁREA DE SALUD`)
    })
    
    observe({
        updateSelectInput(session, "DASFilter",
                          choices = dasList(),
                          selected = input$DASFilter
        )})
    
    dmses = reactive({
      lns_reactive() %>%
            select(`DISTRITO/HOSPITAL`) %>%
            unique() %>%
            arrange(`DISTRITO/HOSPITAL`)
    })
    
    dmsList = reactive({
        append(c("TODOS"),dmses()$`DISTRITO/HOSPITAL`)
    })
    
    observe({
        updateSelectInput(session, "DMSFilter",
                          choices = dmsList(),
                          selected = input$DMSFilter
        )})
    
    lasVariantes = reactive({
        lns_reactive() %>%
            mutate(Variante = as.character(Variante)) %>%
            select(Variante) %>%
            unique() %>%
            arrange(Variante)
    })
    
    variantesList = reactive({
        append(c("TODOS"),lasVariantes()$Variante)
    })
    
    observe({
        updateSelectInput(session, "VarianteFilter",
                          choices = variantesList(),
                          selected = input$VarianteFilter
        )})
    
    ## Epicurva lns
    
    fechas_reactiveLNSToma = reactive({
      lnsIncidence = incidence(
        lns_reactive(),
        date_index = `FECHA DE TOMA DE MUESTRA`,
        interval = 'week',
        groups = Variante
      ) %>%
        mutate(
          date_index = as.Date(date_index)
        )  %>%
        as.data.frame(.) %>%
        group_by(
          date_index,
          Variante
        ) %>%
        summarise(n = sum(count))
    })
    
    fechasLNS = lns %>%
      group_by(`FECHA INGRESO DE MUESTRA`, Variante) %>%
      tally() 
    
    weekly_breaks_allLNS <- reactive({ 
      seq.Date(
        from = floor_date(min(input$fechaReporte[1], na.rm=T),   "week", week_start = 1), # monday before first case
        to   = ceiling_date(max(input$fechaReporte[2], na.rm=T), "week", week_start = 1), # monday after last case
        by   = "month")
    })
    
    output$variantesPorSemanaEpidemiologicaLNSToma = renderPlotly({
      p = ggplot(fechas_reactiveLNSToma(), aes(x=date_index, y=n, fill=Variante)) +
        geom_bar(stat="identity", color = 'black') +
        scale_fill_manual(values = colorVariants) +
        labs(x="Semana epidemiológica", y="No. de muestras")+ 
        theme(axis.text.x = element_text(angle = 90))+
        scale_x_date(breaks = weekly_breaks_allLNS(), date_labels = "%d/%m/%Y",expand = c(0,0), 
                     limits = c(input$fechaReporte[1]-1, max(input$fechaReporte[2])+7)) +
          ylim(0, max(fechas_reactiveLNSToma()$n)+25) 
      
      p2 = ggplotly(p, height = 500) %>%
        layout(#legend = list(orientation = "h",  y = -0.3),
          title = list(text = paste0('Variantes de preocupación e interes de SARS-CoV-2',
                                     '<br>',
                                     'en Guatemala por fecha de toma de muestra',
                                     '<br>',
                                     '<sup>',
                                     '(N= ',
                                     nrow(lns_reactive()),
                                     ')',
                                     '</sup>'), y = 0.95)) #%>% layout(height = 500)
    })
    
    output$variantesPorSemanaEpidemiologicaDBLNSToma = DT::renderDataTable(server = F,
      DT::datatable(
        fechas_reactiveLNSToma(),
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename = 'variantesPorFechaDeToma'
            ),
            list(
              extend = 'excel',
              filename = 'variantesPorFechaDeToma'
            )
          ),
          scrollx = T
        )
      )
    )
    
    
    ## Epicurva lns2
    
    fechas_reactiveLNSIngreso = reactive({
      lnsIncidence = incidence(
        lns_reactive(),
        date_index = `FECHA INGRESO DE MUESTRA`,
        interval = 'week',
        groups = Variante
      ) %>%
        mutate(
          date_index = as.Date(date_index)
        )  %>%
        as.data.frame(.) %>%
        group_by(
          date_index,
          Variante
        ) %>%
        summarise(n = sum(count))
    })
    
    fechasLNS = lns %>%
      group_by(`FECHA INGRESO DE MUESTRA`, Variante) %>%
      tally() 
    
    weekly_breaks_allLNS <- reactive({ 
      seq.Date(
      from = floor_date(min(input$fechaReporte[1], na.rm=T),   "week", week_start = 1), # monday before first case
      to   = ceiling_date(max(input$fechaReporte[2], na.rm=T), "week", week_start = 1), # monday after last case
      by   = "month")
    })
    
    output$variantesPorSemanaEpidemiologicaLNS = renderPlotly({
      p = ggplot(fechas_reactiveLNSIngreso(), aes(x=date_index, y=n, fill=Variante)) +
        geom_bar(stat="identity", color = 'black') +
        scale_fill_manual(values = colorVariants) +
        labs(x="Semana epidemiológica", y="No. de muestras")+ 
        theme(axis.text.x = element_text(angle = 90))+
        scale_x_date(breaks = weekly_breaks_allLNS(), date_labels = "%d/%m/%Y",expand = c(0,0), 
                     limits = c(input$fechaReporte[1]-1, max(input$fechaReporte[2])+7)) +
          ylim(0, max(fechas_reactiveLNSIngreso()$n)+25) 
      
      p2 = ggplotly(p, height = 500) %>%
      layout(#legend = list(orientation = "h",  y = -0.3),
               title = list(text = paste0('Variantes de preocupación e interes de SARS-CoV-2',
                                          '<br>',
                                          'en Guatemala por fecha de ingreso de muestra',
                                          '<br>',
                                          '<sup>',
                                          '(N= ',
                                          nrow(lns_reactive()),
                                          ')',
                                          '</sup>'), y = 0.95)) #%>% layout(height = 500)
    })
    
    output$variantesPorSemanaEpidemiologicaDBLNSIngreso = DT::renderDataTable(server = F,
      DT::datatable(
        fechas_reactiveLNSIngreso(),
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = list(
            list(
              extend = 'csv',
              filename = 'variantesPorFechaDeIngreso'
            ),
            list(
              extend = 'excel',
              filename = 'variantesPorFechaDeIngreso'
            )
          ),
          scrollx = T
        )
      )
    )
    
    
    ## Pangolin identificadas LNS
    pangolinLNS = reactive({
        lns_reactive() %>%
            group_by(pangoVoc) %>%
            tally() %>%
            arrange(n) 
    })
    
    output$variantesPangolinLNS = renderPlotly({
        p = ggplot(pangolinLNS() %>% tail(.,20), aes(x=reorder(pangoVoc,n), y=n, fill=pangoVoc)) +
            geom_bar(stat = "identity") +
            #geom_text(aes(label=n)) +
            theme_minimal() +
            #scale_fill_brewer(palette="Set1") +
            theme(legend.position = "none") +
            xlab('Linaje de Pangolin') +
            ylab('Cantidad de muestras') + 
            ggtitle('Frecuencia de variantes del SARS-CoV-2 identificadas en Guatemala') +
            labs(subtitle = paste('(Muestras = ', nrow(lns_reactive()),')')) +
            coord_flip()
        
        p2 = ggplotly(p) %>%
          layout(#legend = list(orientation = "h",  y = -0.3),
            title = list(text = paste0('Frecuencia de variantes del SARS-CoV-2 <br>presentes en Guatemala',
                                       ' ',
                                       '',
                                       '(N= ',
                                       nrow(lns_reactive()),
                                       ')',
                                       ''), y = 0.95))
        
        
    })
    
    output$variantesPangolinDBLNS = DT::renderDataTable(server = F,
        DT::datatable(
            pangolinLNS() %>%
                arrange(-n) ,
            extensions = 'Buttons',
            options = list(
                dom = 'Bfrtip',
                buttons = list(
                    list(
                        extend = 'csv',
                        filename = 'variantesPangolin'
                    ),
                    list(
                        extend = 'excel',
                        filename = 'variantesPangolin'
                    )
                ),
                scrollx = T
            )
        )
    )
    
    
    ## VOCs LNS
    vocLNS = reactive({
        lns_reactive() %>%
            group_by(Variante) %>%
            tally() %>%
            arrange(n) 
    })
    
    output$variantesOMSLNS = renderPlotly({
        p = ggplot(vocLNS() %>% tail(.,20), aes(x=reorder(Variante,n), y=n, fill=Variante)) +
            geom_bar(stat = "identity") +
            #geom_text(aes(label=n)) +
            theme_minimal() +
            scale_fill_manual(values = colorVariants) +
            theme(legend.position = "none") +
            xlab('Linaje de Pangolin') +
            ylab('Cantidad de muestras') + 
            ggtitle('Las variantes de preocupación e interes de SARS-CoV-2 \npresentes en Guatemala') +
            labs(subtitle = paste('(Muestras = ', nrow(lns_reactive()),')')) +
            coord_flip()
      
      p2 = ggplotly(p) %>%
        layout(#legend = list(orientation = "h",  y = -0.3),
          title = list(text = paste0('Las variantes de preocupación e interes de SARS-CoV-2 <br>presentes en Guatemala',
                                     ' ',
                                     '',
                                     '(N= ',
                                     nrow(lns_reactive()),
                                     ')',
                                     ''), y = 0.95))
    })
    
    output$variantesOMSDBLNS = DT::renderDataTable(server = F,
        DT::datatable(
            vocLNS() %>%
                arrange(-n) ,
            extensions = 'Buttons',
            options = list(
                dom = 'Bfrtip',
                buttons = list(
                    list(
                        extend = 'csv',
                        filename = 'variantesOMS'
                    ),
                    list(
                        extend = 'excel',
                        filename = 'variantesOMS'
                    )
                ),
                scrollx = T
            )
        )
    )
    
    ## Areas LNS
    area = reactive({
        lns_reactive() %>%
            #mutate(
            #    departamento = tolower(as.character(departamento)),
            #    departamento = stri_trans_totitle(departamento),
            #) %>% 
            group_by(`ÁREA DE SALUD`, Variante) %>% 
            tally() %>% 
            ungroup() %>%
            complete(`ÁREA DE SALUD` = areas$`ÁREA DE SALUD`, fill = list(Variante = 'Otro', n = 0)) %>%
            group_by(`ÁREA DE SALUD`) %>%
            mutate(nsum = sum(n)) %>%
            arrange(nsum) 
    })
    
    output$variantesPorDASLNS = renderPlotly({
        ggplot(area(), aes(x=reorder(`ÁREA DE SALUD`,nsum), y=n, fill=Variante)) +
            geom_bar(stat = "identity") +
            #geom_text(aes(label=n)) +
            theme_minimal() +
            scale_fill_manual(values = colorVariants) +
            #theme(legend.position = "none") +
            xlab('Área de salud') +
            ylab('Cantidad de muestras') + 
            ggtitle('Muestras por área de salud') +
            labs(subtitle = paste('(Muestras = ', nrow(lns_reactive()),')')) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })
    
    output$variantesPorDASDBLNS = DT::renderDataTable(server = F,
        DT::datatable(
            area() %>%
                arrange(-nsum) ,
            extensions = 'Buttons',
            options = list(
                dom = 'Bfrtip',
                buttons = list(
                    list(
                        extend = 'csv',
                        filename = 'variantesPorDAS'
                    ),
                    list(
                        extend = 'excel',
                        filename = 'variantesPorDAS'
                    )
                ),
                scrollx = T
            )
        )
    )
    
    ## Mapa variantes LNS
    area_mapa = reactive({
        area() %>%
            filter(
                `ÁREA DE SALUD` != 'IGSS'
            ) %>%
            #mutate(
            #    departamento = tolower(departamento)
            #) %>%
            select(-nsum) %>%
            pivot_wider(names_from = Variante, values_from = n, values_fill = 0) %>%
            left_join(., areasGeo)
    })
    
    areas_shapes_reactive = reactive({
        areas_shapes #%>%
            #mutate(nombre = tolower(nombre)) %>%
            #dplyr::rename(departamento = nombre) 
    })
    
    areas_shapesMapa = reactive({left_join(areas_shapes_reactive(), area_mapa())})
    
    output$mapaVariantesPorDASLNS = renderLeaflet({
        basemap = leaflet(areas_shapesMapa()) %>%
            addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
            addPolygons(
                group = 'Areas',
                dashArray = '3',
                weight = 2,
                color = 'grey',
                opacity = 1
            )
        
        spectral = brewer.pal(name="Set3",n=12)
        basemap %>%
            addMinicharts(
                area_mapa()$lng, area_mapa()$lat,
                type = 'pie',
                #showLabels = T,
                legend = T,
                #layerId = 'variantes',
                #chartdata = departamento_mapa()[,c('Omicron', 'Delta', 'Gamma', 'Beta', 'Alpha', 'Iota', 'Epsilon', 'Mu', 'Otro', 'Desconocido')] ,
                chartdata = area_mapa() %>% ungroup() %>% select(-`ÁREA DE SALUD`, -lat, -lng),
                colorPalette = spectral,
                
            )
    })
    
    output$mapaVariantesPorDASLNSNumero = renderLeaflet({
      basemap = leaflet(areas_shapesMapa()) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
        addPolygons(
          group = 'Areas',
          dashArray = '3',
          weight = 2,
          color = 'grey',
          opacity = 1
        )
      
      spectral = brewer.pal(name="Set3",n=12)
      basemap %>%
        addMinicharts(
          area_mapa()$lng, area_mapa()$lat,
          type = 'pie',
          showLabels = T,
          legend = T,
          #layerId = 'variantes',
          #chartdata = departamento_mapa()[,c('Omicron', 'Delta', 'Gamma', 'Beta', 'Alpha', 'Iota', 'Epsilon', 'Mu', 'Otro', 'Desconocido')] ,
          chartdata = area_mapa() %>% ungroup() %>% select(-`ÁREA DE SALUD`, -lat, -lng),
          colorPalette = spectral,
          
        )
    })
    
    ## Departamentos LNS
    departamento = reactive({
        lns_reactive() %>%
            #mutate(
            #    departamento = tolower(as.character(departamento)),
            #    departamento = stri_trans_totitle(departamento),
            #) %>% 
            group_by(departamento, Variante) %>% 
            tally() %>% 
            ungroup() %>%
            complete(departamento = departamentos$departamento, fill = list(Variante = 'Otro', n = 0)) %>%
            group_by(departamento) %>%
            mutate(nsum = sum(n)) %>%
            arrange(nsum) 
    })
    
    output$variantesPorDepartamentoLNS = renderPlotly({
        ggplot(departamento(), aes(x=reorder(departamento,nsum), y=n, fill=Variante)) +
            geom_bar(stat = "identity") +
            #geom_text(aes(label=n)) +
            theme_minimal() +
            scale_fill_manual(values = colorVariants) +
            #theme(legend.position = "none") +
            xlab('Departamento') +
            ylab('Cantidad de muestras') + 
            ggtitle('Muestras por departamento') +
            labs(subtitle = paste('(Muestras = ', nrow(lns_reactive()),')')) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })
    

    
    ## Mapa variantes departamento LNS
    departamento_mapa = reactive({
        departamento() %>%
            filter(
                departamento != 'IGSS'
            ) %>%
            #mutate(
            #    departamento = tolower(departamento)
            #) %>%
            select(-nsum) %>%
            pivot_wider(names_from = Variante, values_from = n, values_fill = 0) %>%
            left_join(., departamentosGeo)
    })
    
    departamentos_shapes_reactive = reactive({
        departamentos_shapes #%>%
        #mutate(nombre = tolower(nombre)) %>%
        #dplyr::rename(departamento = nombre) 
    })
    
    departamentos_shapesMapa = reactive({left_join(departamentos_shapes_reactive(), departamento_mapa())})
    
    output$mapaVariantesPorDepartamentoLNS = renderLeaflet({
        
        
        basemap = leaflet(departamentos_shapesMapa()) %>%
            addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
            addPolygons(
                group = 'Departamentos',
                dashArray = '3',
                weight = 2,
                color = 'grey',
                opacity = 1
            )
        
        spectral = brewer.pal(name="Set3",n=12)
        basemap %>%
            addMinicharts(
                departamento_mapa()$lng, departamento_mapa()$lat,
                type = 'pie',
                #showLabels = T,
                #legend = T,
                #layerId = 'variantes',
                #chartdata = departamento_mapa()[,c('Omicron', 'Delta', 'Gamma', 'Beta', 'Alpha', 'Iota', 'Epsilon', 'Mu', 'Otro', 'Desconocido')] ,
                chartdata = departamento_mapa() %>% ungroup() %>% select(-departamento, -lat, -lng),
                colorPalette = spectral,
                
            )
    })
    
    output$mapaVariantesPorDepartamentoLNSNumero = renderLeaflet({
        basemap = leaflet(departamentos_shapesMapa()) %>%
            addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
            addPolygons(
                group = 'Areas',
                dashArray = '3',
                weight = 2,
                color = 'grey',
                opacity = 1
            )
        
        spectral = brewer.pal(name="Set3",n=12)
        basemap %>%
            addMinicharts(
                departamento_mapa()$lng, departamento_mapa()$lat,
                type = 'pie',
                showLabels = T,
                legend = T,
                #layerId = 'variantes',
                #chartdata = departamento_mapa()[,c('Omicron', 'Delta', 'Gamma', 'Beta', 'Alpha', 'Iota', 'Epsilon', 'Mu', 'Otro', 'Desconocido')] ,
                chartdata = departamento_mapa() %>% ungroup() %>% select(-departamento, -lat, -lng),
                colorPalette = spectral,
                
            )
    })
    
    output$variantesPorDepartamentoDBLNS = DT::renderDataTable(server = F, 
        DT::datatable(
            departamento() %>%
                arrange(-nsum) ,
            extensions = 'Buttons',
            options = list(
                dom = 'Bfrtip',
                buttons = list(
                    list(
                        extend = 'csv',
                        filename = 'variantesPorDepartamento'
                    ),
                    list(
                        extend = 'excel',
                        filename = 'variantesPorDepartamento'
                    )
                ),
                scrollx = T
            )
        )
    )
    

    
    ## Grupo etario LNS
    edadLNS = reactive({
        lns_reactive() %>%
            mutate(
                SEXO = as.factor(SEXO)
            ) %>%
            group_by(grupo_etario, SEXO, Variante) %>% 
            tally() %>%
            dplyr::rename(
                age = grupo_etario,
                gender = SEXO,
                count = n
            ) 
    })
    
    output$variantesPorGrupoEtarioLNS = renderPlot({
        age_pyramid(
            data = edadLNS(),
            age_group = "age",
            split_by = "gender",
            count = 'count',
            show_midpoint = FALSE,
            stack_by = 'Variante'
        ) +
            #theme_minimal()+                               # simplfy background
            #scale_fill_manual(                             # specify colors AND labels
            #  values = c("Masculino"="orange", "Femenino"="purple", 'Desconocido'='grey'))+
            scale_fill_brewer(palette='Spectral') +
            labs(y = "Cantidad de muestras",              # note x and y labs are switched
                 x = "Grupo etario",                          
                 fill = "Variante", 
                 title = "Distribucion de variantes por grupo etario en Guatemala",
                 subtitle = paste('(Muestras = ', nrow(lns_reactive()),')')
            ) +
            theme(
                legend.position = "bottom",                          # legend to bottom
                axis.text = element_text(size = 10, face = "bold"),  # fonts/sizes
                axis.title = element_text(size = 12, face = "bold"))
    })
    
    output$variantesPorGrupoEtarioDBLNS = DT::renderDataTable(server = F,
        DT::datatable(
            edadLNS() %>% 
                dplyr::rename(
                    `Grupo etario` = age,
                    `Cantidad de muestras` = count,
                    Sexo = gender
                ),
            extensions = 'Buttons',
            options = list(
                dom = 'Bfrtip',
                buttons = list(
                    list(
                        extend = 'csv',
                        filename = 'casosPorGrupoEtario'
                    ),
                    list(
                        extend = 'excel',
                        filename = 'casosPorGrupoEtario'
                    )
                ),
                scrollx = T
            )
        )
    )
    
    
    ## Filogenia
    # output$filogenia = renderPlot({
    #     ggtree(tree, layout = 'circular', branch.length = 'none') %<+% gisaid  +
    #         geom_tippoint(
    #             mapping = aes(color = Variante),          # tip color by continent. You may change shape adding "shape = "
    #             size = 1.5)+
    #         scale_color_brewer(
    #             name = "Variante",                    # name of your color scheme (will show up in the legend like this)
    #             palette = "Spectral",                      # we choose a set of colors coming with the brewer package
    #             na.value = "grey") +
    #         ggtitle('Arbol filogenético de SARS-CoV-2 en Guatemala') +
    #         labs(subtitle = '(Muestras = 1575)')
    # })
    
    
    
    
    
    
})
