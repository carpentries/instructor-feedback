library(shiny)
library(tidyverse)
library(waffle)
library(packcircles)  # package to calculate postion of circles!
library(viridis)
library(ggiraph)

# start shiny app with shiny::runApp('shinyapp')

# ---- functions for data preparation ----
# function make dataframe with counts/percentages
pre_cnt <- function(x, groupvar){
  x_out <- x %>%
    dplyr::group_by_(.dots = groupvar) %>%  # use group_by_() evaluate content of groupvar!
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(n_percent = round(100 * n/sum(n)))
  return(x_out)
}

# function separate strings
sep_str <- function(x){
  require(dplyr)
  x_out <- x %>%
    strsplit(';') %>% 
    lapply(stringi::stri_trim) %>%
    unlist()
  return(x_out)
}

# function combine rare domains (<xx percent) into group Other
pre_pl <- function(x, column_comb, percent_comb = 2){
  for(index in seq(1, dim(x)[1])){
    if(x[index,'n_percent'] < percent_comb){
      x[index,column_comb] <- 'Other' 
    } 
  }
  x_out <- x %>%
    dplyr::group_by_(column_comb) %>%
    dplyr::summarise(n_percent_s = sum(n_percent), n_s = sum(n)) %>%
    dplyr::ungroup() %>%
    arrange(desc(n_s))
  return(x_out)
}

# ----- data import and pre-preparation -----
pre_os <- readr::read_csv('data/20181002-pre-os-no-open.csv', 
                          na = '<NA>')
# change order of factor variable os and rename factors ('Not sure' does not contain real space, more steps in renaming ...!)
# convert os to factor
pre_os$os <- as.factor(pre_os$os)
# rename levels of os, combine 'NA' and 'Not sure' to 'unknown'
levels(pre_os$os) <- c('Mac OS', 'Linux', 'unknown', 'unknown', 'Windows')
# re-order levels of os
pre_os$os <- factor(pre_os$os, 
                    levels = c('Windows', 'Mac OS', 'Linux', 'unknown'))
 

## ----- what Domains/Occupation do your participants come from? -----
pre_dom <- readr::read_csv('data/20181002-pre-professional_profile-no-open.csv', 
                    na = '<NA>')

# ---- domains ----
domains <- sep_str(pre_dom$domain)
# combine some similar domains from main list
domains[domains == 'Medicine'] <- 'Biomedical or Health Sciences'
domains[domains == 'Biomedical or Health Sciences'] <- 'Medical or Health Sciences'
domains[domains == 'Earth Sciences'] <- 'Planetary Sciences (Geology, Climatology, Oceanography, etc.)'
# make a tibble with domains
domains <- dplyr::tibble(domain = domains)

# make dataframe with counts/percentages
pre_dom_cnt <- pre_cnt(domains, 
                       groupvar = 'domain')

# prepare data for plotting
# combine rare domains (<xx percent) into group Other
pre_dom_pl <- pre_pl(pre_dom_cnt, 
                     column_comb = 'domain', 
                     percent_comb = 2)


# ---- occupations ----
occupation <- sep_str(pre_dom$occupation)

# make a tibble with occupations
occupation <- dplyr::tibble(occupation)

# make dataframe with counts/percentages
pre_occ_cnt <- pre_cnt(occupation, 
                       groupvar = 'occupation')

# prepare data for plotting
# combine rare domains (<xx percent) into group Other
pre_occ_pl <- pre_pl(pre_occ_cnt, 
                     column_comb = 'occupation', 
                     percent_comb = 2)



# ----- define UI (user interface object) for application -----
ui <- shinyUI(
  pageWithSidebar(
    
  # Application title
  headerPanel('Instructor Feedback'),
  
  # sidebar with controls to select the variable to plot 
  sidebarPanel(
    selectInput(inputId = 'variable',
                label = 'Variable:',
                choices = list('dc' = 'Data Carpentry',
                              'swc' = 'Software Carpentry')),
  
    selectInput(inputId = 'plottype', label = 'Choose a plot type:',
                choices = c('barplot','circleplot'))
  ), # end sidebarPanel
  
  # main panel with text and visualisations
  mainPanel(
    h3(textOutput(outputId = 'caption')),
    textOutput(outputId = 'question1'),
    plotOutput(outputId = 'osPlot'),
    textOutput(outputId = 'question2'),
    conditionalPanel(condition = "input.plottype == 'barplot'",
                     plotOutput(outputId = 'domPlotbar')
                     ),
    conditionalPanel(condition = "input.plottype == 'circleplot'",
                     ggiraphOutput(outputId = 'domPlotcircle')
                     ),
    textOutput(outputId = 'question3'),
    conditionalPanel(condition = "input.plottype == 'barplot'",
                     plotOutput(outputId = 'occPlotbar')
    ),
    conditionalPanel(condition = "input.plottype == 'circleplot'",
                     ggiraphOutput(outputId = 'occPlotcircle')
    )
  ) # end mainPanel
  ) # end pageWithSidebar
) # end shinyUI



# ----- define server logic required to plot various variables -----
server <- shinyServer(function(input, output){
  
  # compute the formula text in a reactive expression since it is 
  # shared by the output$caption and output$osPlot expressions
  formulaText <- reactive({
    paste('Results from the pre-workshop survey for your upcoming ', 
          input$variable, 'Workshop')
  })
  
  osInput <- reactive({
    switch(input$variable,
           "Data Carpentry" = subset(pre_os, workshop_type == 'dc', -workshop_type),
           "Software Carpentry" = subset(pre_os, workshop_type == 'swc', -workshop_type)
           ) %>%
    # make dataframe with counts/percentages for plotting
    pre_cnt(groupvar = 'os')
  })
  
  domInput <- reactive({
    switch(input$variable,
           "Data Carpentry" = subset(pre_dom, workshop_type == 'dc'),
           "Software Carpentry" = subset(pre_dom, workshop_type == 'swc')
    )
  })
  
  occInput <- reactive({
    switch(input$variable,
           "Data Carpentry" = subset(pre_occ_pl, workshop_type == 'dc'),
           "Software Carpentry" = subset(pre_occ_pl, workshop_type == 'swc')
    )
  })
  
  
  # return the formula text for printing as a caption
  output$caption <- renderText({
    formulaText()
  })
  
  
  # ---- Print text ----
  output$question1 <- renderText({
    paste('Most participants in your workshop will have',
          osInput()$os[osInput()$n==max(osInput()$n)], 
          'computers.')
  })
  
  output$question2 <- renderText({
    paste('Participants in your workshop mainly have a background in ',
          pre_dom_pl$domain[1], ', ', pre_dom_pl$domain[2], ', and ', pre_dom_pl$domain[3], '.',
          sep = '')
  })
  
  output$question3 <- renderText({
    paste('Most participants in your workshop will be ',
          pre_occ_pl$occupation[1], pre_occ_pl$occupation[2], ', and', pre_occ_pl$occupation[3])
  })

  
  # ---- generate plots of the requested variables ----
  # ---- start plot 1 - operation system ----
  output$osPlot <- renderPlot(width = 250, height = 250, {
    waffle(osInput())
  }) # end plot 1
  
  # ---- start plot 2 - barplot for domain ----
  output$domPlotbar <- renderPlot(width = 500, height = 250, {
    validate(need(input$plottype == "barplot", message=FALSE))
    #if(input$plottype == 'barplot'){ # barplot
      ggplot(data = pre_dom_pl) +
        geom_bar(aes(x = reorder(domain,n_s), y = n_percent_s), 
                 stat='identity', fill = 'grey') + 
        geom_text(aes(x = domain, y = n_percent_s/2, label = n_s), 
                  stat = 'identity', size = 4) + 
        coord_flip() +
        # remove padding on percentage axis, set limits
        scale_y_continuous(expand = c(0,0), 
                            limits = c(0, ifelse(signif(max(pre_dom_pl$n_percent_s),1)>=max(pre_dom_pl$n_percent_s),
                                        signif(max(pre_dom_pl$n_percent_s),1), 
                                        signif(max(pre_dom_pl$n_percent_s),1)+10))) +
        labs(y = '% of participants', x = '') +
        theme_classic() +
        theme(axis.ticks.y = element_blank(),
              axis.line.y = element_blank(),
              axis.text.y=element_text(size = 10))
  })# end plot 2 - barplot
  
  # ---- start plot 2 - circle plot for domain ----
  output$domPlotcircle <- renderggiraph({
    validate(need(input$plottype == "circleplot", message=FALSE))
    
    # Create data
    data <- pre_pl(x = pre_dom_cnt, column_comb = 'domain', percent_comb = 0)
    
    #data$text <- domain
    data$text_short <- stringr::str_sub(data$domain, start=1, end=3)
    # Add a column with the text you want to display for each bubble:
    data$text <- paste(data$domain, "\n", "n =", data$n_s)
    
    # Generate the layout
    packing <- circleProgressiveLayout(data$n_s, sizetype='area')
    data = cbind(data, packing)
    dat.gg <- circleLayoutVertices(packing, npoints=50)
    
    # Make the plot
    p=ggplot() + 
      geom_polygon_interactive(data = dat.gg, aes(x, y, group = id, fill=id, tooltip = data$text[id], data_id = id), colour = "black", alpha = 0.6) +
      scale_fill_distiller(type = "div", palette = 'Spectral',direction = 1) +
      #scale_fill_viridis() +
      geom_text(data = data, aes(x, y, label = text_short), size=5, color="black") +
      theme_void() + 
      theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
      coord_equal()
    
    # final interactive plot
    ggiraph(ggobj = p, width_svg = 7, height_svg = 7)
  }) # end plot 2 circle plot - domain

  
  # ---- start plot 3 - barplot for occupation ----
  output$occPlotbar <- renderPlot(width = 500, height = 250, {
    validate(need(input$plottype == "barplot", message=FALSE))
    ggplot(data = pre_occ_pl) +
      geom_bar(aes(x = reorder(occupation,n_s), y = n_percent_s), 
               stat='identity', fill = 'grey') + 
      geom_text(aes(x = occupation, y = n_percent_s/2, label = n_s), 
                stat = 'identity', size = 4) + 
      coord_flip() +
      # remove padding on percentage axis, set limits
      scale_y_continuous(expand = c(0,0), 
                         limits = c(0, ifelse(signif(max(pre_occ_pl$n_percent_s),1)>=max(pre_occ_pl$n_percent_s),
                                              signif(max(pre_occ_pl$n_percent_s),1), 
                                              signif(max(pre_occ_pl$n_percent_s),1)+10))) +
      labs(y = '% of participants', x = '') +
      theme_classic() +
      theme(axis.ticks.y = element_blank(),
            axis.line.y = element_blank(),
            axis.text.y=element_text(size = 10))
  })# end plot 3 - barplot - occupation
  
  # ---- start plot 3 - circle plot for occupation ----
  output$occPlotcircle <- renderggiraph({
    validate(need(input$plottype == "circleplot", message=FALSE))
    
    # Create data
    data <- pre_pl(x = pre_occ_cnt, column_comb = 'occupation', percent_comb = 0)
    
    data$text_short <- stringr::str_sub(data$occupation, start=1, end=3)
    # Add a column with the text you want to display for each bubble:
    data$text <- paste(data$occupation, "\n", "n =", data$n_s)
    
    # Generate the layout
    packing <- circleProgressiveLayout(data$n_s, sizetype='area')
    data = cbind(data, packing)
    dat.gg <- circleLayoutVertices(packing, npoints=50)
    
    # Make the plot
    p=ggplot() + 
      geom_polygon_interactive(data = dat.gg, aes(x, y, group = id, fill=id, tooltip = data$text[id], data_id = id), colour = "black", alpha = 0.6) +
      scale_fill_distiller(type = "div", palette = 'Spectral',direction = 1) +
      #scale_fill_viridis() +
      geom_text(data = data, aes(x, y, label = text_short), size=5, color="black") +
      theme_void() + 
      theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
      coord_equal()
    
    # final interactive plot
    ggiraph(ggobj = p, width_svg = 7, height_svg = 7)
  }) # end plot 2 circle plot - occupation
  
  
}) # end server

shinyApp(ui = ui, server = server)
