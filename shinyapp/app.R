library(shiny)
library(tidyverse)
library(waffle)
library(packcircles)  # package to calculate postion of circles!

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
pre_os <- readr::read_csv('../data/20181002-pre-os-no-open.csv', na = '<NA>')
# change order of factor variable os and rename factors ('Not sure' does not contain real space, more steps in renaming ...!)
# convert os to factor
pre_os$os <- as.factor(pre_os$os)
# rename levels of os, combine 'NA' and 'Not sure' to 'unknown'
levels(pre_os$os) <- c('Mac OS', 'Linux', 'unknown', 'unknown', 'Windows')
# re-order levels of os
pre_os$os <- factor(pre_os$os, levels = c('Windows', 'Mac OS', 'Linux', 'unknown'))
 

## ----- What Disciplines/Domains do your participants come from? -----
pre_dom <- read_csv('../data/20181002-pre-professional_profile-no-open.csv', 
                    na = '<NA>')

# domains_dc <- dom_splt(pre_dom$domain[pre_dom$workshop_type == 'dc'])
# domains_swc <- dom_splt(pre_dom$domain[pre_dom$workshop_type == 'swc'])
# # make a tibble with domains
# domains <- rbind(tibble(domain = domains_dc, workshop_type = 'dc'),
#                  tibble(domain = domains_swc, workshop_type = 'swc'))
# 
# # combine some similar domains from main list
# domains$domain[domains$domain == 'Medicine'] <- 'Biomedical or Health Sciences'
# domains$domain[domains$domain == 'Biomedical or Health Sciences'] <- 'Medical or Health Sciences'
# domains$domain[domains$domain == 'Earth Sciences'] <- 'Planetary Sciences (Geology, Climatology, Oceanography, etc.)'
# domains$domain[domains$domain == 'NA'] <- 'Other'
# 
# # make dataframe with counts/percentages
# pre_dom_cnt <- pre_cnt(x = domains, groupvar = c('domain','workshop_type'))
# 
# pre_dom_cnt <- domains %>%
#   dplyr::group_by(domain, workshop_type) %>%
#   dplyr::count() %>%
#   dplyr::ungroup() %>%
#   # percent should be dependent on number of respondendts!
#   dplyr::group_by(workshop_type) %>%
#   dplyr::mutate(n_percent = round(100 * n/sum(n))) %>%
#   dplyr::ungroup()
# 
# # prepare data for plotting
# # combine rare domains (<5 percent) into group Other
# pre_dom_pl <- pre_dom_cnt %>%
#   dplyr::mutate(domain = ifelse(n_percent >= 5, domain, 'Other')) %>%
#   dplyr::group_by(domain, workshop_type) %>%
#   dplyr::summarise(n_percent_s = sum(n_percent), n_s = sum(n)) %>%
#   dplyr::ungroup() %>%
#   arrange(desc(n_s))

# ---- domains ----
domains <- sep_str(pre_dom$domain)
# combine some similar domains from main list
domains[domains == 'Medicine'] <- 'Biomedical or Health Sciences'
domains[domains == 'Biomedical or Health Sciences'] <- 'Medical or Health Sciences'
domains[domains == 'Earth Sciences'] <- 'Planetary Sciences (Geology, Climatology, Oceanography, etc.)'
#domains[domains == 'NA'] <- 'Other'
# make a tibble with domains
domains <- dplyr::tibble(domain = domains)

# make dataframe with counts/percentages
pre_dom_cnt <- pre_cnt(domains, 'domain')

# prepare data for plotting
# combine rare domains (<5 percent) into group Other
pre_dom_pl <- pre_pl(pre_dom_cnt, column_comb = 'domain', percent_comb = 2)



## ----- What is the position/career stage of participants in your workshop? -----
# separate occupation
occupation <- pre_dom$occupation %>%
  strsplit(';') %>%
  # strip quotation marks and space
  lapply(stringi::stri_trim) %>%
  unlist()
# combine some similar domains from main list
occupation[occupation == 'NA'] <- 'Other'
# make a tibble with occupations
occupation <- dplyr::tibble(occupation)

# make dataframe with counts/percentages
pre_occ_cnt <- occupation %>%
  dplyr::group_by(occupation) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(n_percent = round(100 * n/sum(n)))

# prepare data for plotting
# combine rare domains (<5 percent) into group Other
pre_occ_pl <- pre_occ_cnt %>%
  dplyr::mutate(occupation = ifelse(n_percent >= 5, occupation, 'Other')) %>%
  dplyr::group_by(occupation) %>%
  dplyr::summarise(n_percent_s = sum(n_percent), n_s = sum(n)) %>%
  dplyr::ungroup() %>%
  arrange(desc(n_s))



# ----- define UI (user interface object) for application -----
ui <- shinyUI(pageWithSidebar(
  # Application title
  headerPanel('Instructor Feedback'),
  
  # sidebar with controls to select the variable to plot 
  sidebarPanel(
    selectInput(inputId = 'variable',
                label = 'Variable:',
  #            choices = c('dc', 'swc'))
                choices = list('dc' = 'Data Carpentry',
                              'swc' = 'Software Carpentry')),
  
    selectInput(inputId = 'plottype', label = 'Choose a plot type:',
                choices = c('barplot', 'circleplot'))
    
    #checkboxInput('outliers', 'Show outliers', FALSE)
  ),
  
  mainPanel(
    h3(textOutput(outputId = 'caption')),
    textOutput(outputId = 'question1'),
    plotOutput(outputId = 'osPlot'),
    textOutput(outputId = 'question2'),
    plotOutput(outputId = 'domPlotbar'),
    plotOutput(outputId = 'domPlotcircle'),
    textOutput(outputId = 'question3'),
    plotOutput(outputId = 'occPlot')
  )
))


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
    ) %>%
      pre_cnt(groupvar = 'domain')
  })
  
  occInput <- reactive({
    switch(input$variable,
           "Data Carpentry" = subset(pre_occ_pl, workshop_type == 'dc'),
           "Software Carpentry" = subset(pre_occ_pl, workshop_type == 'swc')
    ) %>%
      sep_str(domain) %>%
      mutate(domains[domains == 'Medicine'] <- 'Biomedical or Health Sciences',
             domains[domains == 'Biomedical or Health Sciences'] <- 'Medical or Health Sciences',
             domains[domains == 'Earth Sciences'] <- 'Planetary Sciences (Geology, Climatology, Oceanography, etc.)') %>%
      tibble()
  })
  
  
  domains <- sep_str(pre_dom$domain)
  # combine some similar domains from main list
  domains[domains == 'Medicine'] <- 'Biomedical or Health Sciences'
  domains[domains == 'Biomedical or Health Sciences'] <- 'Medical or Health Sciences'
  domains[domains == 'Earth Sciences'] <- 'Planetary Sciences (Geology, Climatology, Oceanography, etc.)'
  #domains[domains == 'NA'] <- 'Other'
  # make a tibble with domains
  domains <- dplyr::tibble(domain = domains)
  
  # make dataframe with counts/percentages
  pre_dom_cnt <- pre_cnt(domains, 'domain')
  
  # prepare data for plotting
  # combine rare domains (<5 percent) into group Other
  pre_dom_pl <- pre_pl(pre_dom_cnt, column_comb = 'domain', percent_comb = 2)
  
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
          domInput()$domain[1], ', ', domInput()$domain[2], ', and ', domInput()$domain[3], '.',
          sep = '')
  })
  
  output$question3 <- renderText({
    paste('Most participants in your workshop will be ',
          occInput()$occupation[1], occInput()$occupation[2], occInput()$occupation[3])
  })

  
  # ---- generate plots of the requested variable ----
  # start plot 1
  output$osPlot <- renderPlot(width = 250, height = 250, {
    #ggplot(data = osInput(), 
    #             aes(x = "", y = n_percent, fill = os)) + 
    #  geom_bar(stat = "identity") +
    #  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
    #  coord_polar(theta = "y") + 
    #  theme_void()
    waffle(osInput())
  }) # end plot 1
  
  # start plot 2 - barplot
  output$domPlotbar <- renderPlot(width = 500, height = 250, {
    validate(need(input$plottype == "barplot", message=FALSE))
    ggplot(data = pre_dom_pl) +
      geom_bar(aes(x = reorder(domain,n_s), y = n_percent_s), 
               stat='identity', fill = 'grey') + 
      geom_text(aes(x = domain, y = n_percent_s/2, label = n_s), 
                stat = 'identity', size = 4) + 
      coord_flip() +
      # remove padding on percentage axis, set limits
     # scale_y_continuous(expand = c(0,0), 
    #                     limits = c(0, ifelse(signif(max(domInput()$n_percent_s),1)>=max(domInput()$n_percent_s),
    #                                 signif(max(domInput()$n_percent_s),1), 
    #                                 signif(max(domInput()$n_percent_s),1)+10))) +
      labs(y = '% of participants', x = '') +
      theme_classic() +
      theme(axis.ticks.y = element_blank(),
            axis.line.y = element_blank(),
            axis.text.y=element_text(size = 10))
  }) # end plot 2 - barplot
  
  # start plot 2 - circleplot
  output$domPlotcircle <- renderPlot(width = 500, height = 250, {
    validate(need(input$plottype == "circleplot", message=FALSE))
    # Generate the layout. This function return a dataframe with one line per bubble. 
    # It gives its center (x and y) and its radius, proportional of the value
    packing <- circleProgressiveLayout(pre_dom_pl$n_s, sizetype='area')
    # We can add these packing information to the initial data frame
    data = cbind(pre_dom_pl, packing)
    # Check that radius is proportional to value. We don't want a linear relationship, since it is the AREA that must be proportionnal to the value
    plot(data$radius, data$n_s)
    # The next step is to go from one center + a radius to the coordinates of a circle that
    # is drawn by a multitude of straight lines.
    dat.gg <- circleLayoutVertices(packing, npoints=50)
    # Make the plot
    ggplot() + 
      # Make the bubbles
      geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
      # Add text in the center of each bubble + control its size
      geom_text(data = data, aes(x, y, label = domain)) +   #size=n_s, 
      scale_size_continuous(range = c(1,4)) +
      # General theme:
      theme_void() + 
      theme(legend.position="none") +
      coord_equal()
  }) # end plot2 - circleplot
})

shinyApp(ui = ui, server = server)
