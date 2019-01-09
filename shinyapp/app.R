library(shiny)
library(tidyverse)
library(waffle)

# start shiny app with shiny::runApp('shinyapp')

# ----- data import-----
pre_os <- readr::read_csv('../data/20181002-pre-os-no-open.csv', na = '<NA>')

# ---- data preparation ----
# ---- What Operation system do participants have? ----
# change order of factor variable os and rename factors ('Not sure' does not contain real space, more steps in renaming ...!)
# convert os to factor
pre_os$os <- as.factor(pre_os$os)
# rename levels of os, combine 'NA' and 'Not sure' to 'unknown'
levels(pre_os$os) <- c('Mac OS', 'Linux', 'unknown', 'unknown', 'Windows')
# re-order levels of os
pre_os$os <- factor(pre_os$os, levels = c('Windows', 'Mac OS', 'Linux', 'unknown'))

# make dataframe with counts/percentages for plotting
pre_os_pl <- pre_os %>%
  dplyr::group_by(os, workshop_type) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(n_percent = round(100 * n/sum(n)))

# prepare data for plotting
pre_os_pl <- pre_os_pl %>%
  # only show values > 2%
  dplyr::filter(n_percent > 2) %>%
  # sort os
  arrange(desc(os))


## ----- What Disciplines/Domains do your participants come from? -----
pre_dom <- read_csv('../data/20181002-pre-professional_profile-no-open.csv', 
                    na = '<NA>')

# separate domains
dom_splt <- function(x){
  x_splt <- x %>%
  strsplit(';') %>%
  # strip quotation marks and space
  lapply(stringi::stri_trim) %>%
  unlist()
  return(x_splt)
}

domains_dc <- dom_splt(pre_dom$domain[pre_dom$workshop_type == 'dc'])
domains_swc <- dom_splt(pre_dom$domain[pre_dom$workshop_type == 'swc'])
# make a tibble with domains
domains <- rbind(tibble(domain = domains_dc, workshop_type = 'dc'),
                 tibble(domain = domains_swc, workshop_type = 'swc'))

# combine some similar domains from main list
domains$domain[domains$domain == 'Medicine'] <- 'Biomedical or Health Sciences'
domains$domain[domains$domain == 'Biomedical or Health Sciences'] <- 'Medical or Health Sciences'
domains$domain[domains$domain == 'Earth Sciences'] <- 'Planetary Sciences (Geology, Climatology, Oceanography, etc.)'
domains$domain[domains$domain == 'NA'] <- 'Other'

# make dataframe with counts/percentages
pre_dom_cnt <- domains %>%
  dplyr::group_by(domain, workshop_type) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  # percent should be dependent on number of respondendts!
  dplyr::group_by(workshop_type) %>%
  dplyr::mutate(n_percent = round(100 * n/sum(n))) %>%
  dplyr::ungroup()

# prepare data for plotting
# combine rare domains (<5 percent) into group Other
pre_dom_pl <- pre_dom_cnt %>%
  dplyr::mutate(domain = ifelse(n_percent >= 5, domain, 'Other')) %>%
  dplyr::group_by(domain, workshop_type) %>%
  dplyr::summarise(n_percent_s = sum(n_percent), n_s = sum(n)) %>%
  dplyr::ungroup() %>%
  arrange(desc(n_s))

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
                              'swc' = 'Software Carpentry'))
    
    #checkboxInput('outliers', 'Show outliers', FALSE)
  ),
  
  mainPanel(
    h3(textOutput(outputId = 'caption')),
    textOutput(outputId = 'question1'),
    plotOutput(outputId = 'osPlot'),
    textOutput(outputId = 'question2'),
    plotOutput(outputId = 'domPlot'),
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
           "Data Carpentry" = subset(pre_os_pl, workshop_type == 'dc', -workshop_type),
           "Software Carpentry" = subset(pre_os_pl, workshop_type == 'swc', -workshop_type)
           )
  })
  
  domInput <- reactive({
    switch(input$variable,
           "Data Carpentry" = subset(pre_dom_pl, workshop_type == 'dc'),
           "Software Carpentry" = subset(pre_dom_pl, workshop_type == 'swc')
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
  
  # start plot 2
  output$domPlot <- renderPlot(width = 500, height = 250, {
  ggplot(data = domInput()) +
    geom_bar(aes(x = reorder(domain,n_s), y = n_percent_s), 
             stat='identity', fill = 'grey') + 
    geom_text(aes(x = domain, y = n_percent_s/2, label = n_s), 
              stat = 'identity', size = 4) + 
    coord_flip() +
    # remove padding on percentage axis, set limits
    scale_y_continuous(expand = c(0,0), 
                       limits = c(0, ifelse(signif(max(domInput()$n_percent_s),1)>=max(domInput()$n_percent_s),
                                   signif(max(domInput()$n_percent_s),1), 
                                   signif(max(domInput()$n_percent_s),1)+10))) +
    labs(y = '% of participants', x = '') +
    theme_classic() +
    theme(axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          axis.text.y=element_text(size = 10))
  }) # end plot 2
  
})

shinyApp(ui = ui, server = server)
