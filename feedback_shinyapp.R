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
    dplyr::group_by_(.dots = groupvar) %>%  # using group_by_() evaluates content of groupvar!
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(n_percent = round(100 * n/sum(n))) %>% 
    arrange(desc(n_percent))
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

# function prepare for plot, combine rare domains (<xx percent) into group Other, sort
pre_pl <- function(x, column_comb, percent_comb = 5){
  for(index in seq(1, dim(x)[1])){
    if(x[index,'n_percent'] < percent_comb){
      x[index,column_comb] <- 'Other' 
    } 
  }
  x_out <- x %>%
    dplyr::group_by_(column_comb) %>%
    dplyr::summarise(n_s = sum(n)) %>%
    dplyr::mutate(n_percent_s = round(100 * n_s/sum(n_s))) %>% 
    dplyr::ungroup() %>%
    arrange(desc(n_s))
  return(x_out)
}

# function omit na values, omit columns, reshape data into long format 
data_long <- function(x, columns_to_remove = c(1:3)){
  x_out <- x %>% 
    na.omit() %>% 
    select(-columns_to_remove) %>% 
    gather(key = question, value = level)
} #end function

# function make question and levels a factor with custom re-ordered levels, make numeric level variable
data_factor <- function(x, question_levels, question_labels, answer_levels){
  x_out <- x %>% 
    mutate(question = factor(question, levels = question_levels, labels = question_labels)) %>% 
    mutate(level = factor(level, levels = answer_levels)) %>% 
    mutate(level_weight = NA)
  for(i in 1:length(levels(x_out$level))){
    x_out$level_weight[x_out$level == levels(x_out$level)[i]] = i
  } #end for loop
  return(x_out)
} #end function

# assign numeric values to agreement levels
levels_num <- function(x){
  x_out <- x %>%
    mutate(level_weight = ifelse(level == 'Strongly disagree', 1, 
                                 ifelse(level == 'Disagree', 2, 
                                        ifelse(level == 'Neutral', 3,
                                               ifelse(level == 'Agree', 4, 5)))))
  return(x_out)
}

# function mean levels
levels_mean <- function(x){
  x_out <- x %>% 
    group_by(question) %>% 
    summarise(mean_level = mean(level_weight), std_level = sd(level_weight),
              min_level = min(level_weight), max_level = max(level_weight),
              median_level = median(level_weight))
  return(x_out)
}


# ----- plot functions -----
circleplot <- function(x){
  pl_data <- x
  # add display text to data
  pl_data$text_short <- stringr::str_sub(pl_data$group, start=1, end=3)
  # Add a column with the text you want to display for each bubble:
  pl_data$text <- paste(pl_data$group, "\n", "n =", pl_data$n_s)
  
  # Generate the layout
  packing <- circleProgressiveLayout(pl_data$n_s, sizetype='area')
  pl_data = cbind(pl_data, packing)
  dat.gg <- circleLayoutVertices(packing, npoints=50)
  
  # Make the plot
  p=ggplot() + 
    geom_polygon_interactive(data = dat.gg, 
                             aes(x, y, group = id, fill=id, tooltip = pl_data$text[id], data_id = id), 
                             colour = "black", alpha = 0.6) +
    scale_fill_distiller(type = "div", palette = 'Spectral',direction = 1) +
    #scale_fill_viridis() +
    geom_text(data = pl_data, aes(x, y, label = text_short), size=5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  # final interactive plot
  ggiraph(ggobj = p, width_svg = 7, height_svg = 7)
} # end function circleplot

barplot_cust <- function(x){
  ggplot(data = x) +
    geom_bar(aes(x = reorder(group,n_s), y = n_percent_s), 
             stat='identity', fill = 'grey') + 
    geom_text(aes(x = group, y = n_percent_s/2, label = n_s), 
              stat = 'identity', size = 4) + 
    coord_flip() +
    # remove padding on percentage axis, set limits
    scale_y_continuous(expand = c(0,0), 
                       limits = c(0, ifelse(signif(max(x$n_percent_s),1)>=max(x$n_percent_s),
                                            signif(max(x$n_percent_s),1), 
                                            signif(max(x$n_percent_s),1)+10))) +
    labs(y = '% of participants', x = '') +
    theme_classic() +
    theme(axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          axis.text.y=element_text(size = 10))
} # end function barplot_cust

# function violin plot to show distribution
violin_plot <- function(data_violin, data_point, col_violin='skyblue', col_point='blue'){
  p <- ggplot() +
    geom_violin(data = data_violin, aes(x = question, y = level_weight), color = NA, fill = col_violin, alpha = 0.3) + 
    geom_point(data = data_point, aes(x = question, y = mean_level), size = 3, color = col_point) +
    scale_y_continuous(breaks = 1:length(levels(data_violin$level)),
                       labels = levels(data_violin$level)) +
    coord_flip() +
    theme_classic() +
    theme(axis.title = element_blank(),
          panel.border = element_rect(fill = NA),
          text = element_text(size = 16),
          axis.text.x = element_text(angle = 45, hjust = 1)
    )
  return(p)
}

# ----- data import and pre-preparation -----
# ----- Which operation system do participants have? -----
pre_os <- readr::read_csv('data/20181002-pre-os-no-open.csv', 
                          na = '<NA>')
# extract workshop type for workshop
workshoptype <- ifelse(pre_os$workshop_type[1] == 'dc', 'Data Carpentry', 
                       ifelse(pre_os$workshop_type[1] == 'swc', 'Software Carpentry',
                              ifelse(pre_os$workshop_type[1] == 'lc', 'Library Carpentry', 'unknown')))

# change order of factor variable os and rename factors ('Not sure' does not contain real space, more steps in renaming ...!)
# convert os to factor
pre_os$os <- as.factor(pre_os$os)
# rename levels of os, combine 'NA' and 'Not sure' to 'unknown'
levels(pre_os$os) <- c('Mac OS', 'Linux', 'unknown', 'unknown', 'Windows')
# re-order levels of os
pre_os$os <- factor(pre_os$os, 
                    levels = c('Windows', 'Mac OS', 'Linux', 'unknown'))
# make dataframe with counts/percentages
pre_os_cnt <- pre_cnt(pre_os, 
                      groupvar = 'os')

## ----- Which domains/occupation do your participants come from? -----
pre_dom <- readr::read_csv('data/20181002-pre-professional_profile-no-open.csv', 
                    na = '<NA>')

# ---- domains ----
domains <- sep_str(pre_dom$domain)
# combine some similar domains from main list
domains[domains == 'Medicine'] <- 'Biomedical or Health Sciences'
domains[domains == 'Biomedical or Health Sciences'] <- 'Medical or Health Sciences'
domains[domains == 'Earth Sciences'] <- 'Planetary Sciences (Geology, Climatology, Oceanography, etc.)'
# make a tibble with domains
domains <- dplyr::tibble(group = domains)

# make dataframe with counts/percentages
pre_dom_cnt <- pre_cnt(domains, 
                       groupvar = 'group')


# ---- occupations ----
occupation <- sep_str(pre_dom$occupation)

# make a tibble with occupations
occupation <- dplyr::tibble(group = occupation)

# make dataframe with counts/percentages
pre_occ_cnt <- pre_cnt(occupation, 
                       groupvar = 'group')

# ----- usage -----
usage <- read_csv('data/20181002-pre-usage_profile-no-open.csv')
usage_lng <- data_long(usage, c(1:2,8))
# define custom question text
question_levels <- unique(usage_lng$question)
question_labels <- c('command line', 'version control system', 'databases', 
                     'programming language', 'statistical software with GUI')
answer_levels <- c('Never', 'Less than once per year', 'Several times per year',
                   'Monthly', 'Weekly', 'Daily')
# make questions and answers a factor
usage_fact <- data_factor(usage_lng, question_levels, question_labels, answer_levels)
# average levels
usage_levels_mean <- levels_mean(usage_fact)

# ----- skill -----
skill <- read_csv('data/20181002-pre-skill_pre-no-open.csv')
skill_long <- data_long(skill)
question_levels = c('skill_efficient_programming','skill_reproducibility_programming','skill_confidence_programming',
           'skill_overcome_problem','skill_search_answers','skill_write_program','skill_data_raw')
question_labels = c('programming makes analysis efficient',
           'programming makes analysis reproducible','confident to use programming', 'can overcome problem',
           'can search for answers','can write program','raw data important')
answer_levels = c('Strongly disagree', 'Disagree', 'Neutral', 'Agree', 'Strongly agree')
skill_fact <- data_factor(skill_long, question_levels, question_labels, answer_levels)
skill_lnum <- levels_num(skill_fact)
skill_mean <- levels_mean(skill_lnum)


# ----- define UI (user interface object) for application -----
ui <- shinyUI(
  pageWithSidebar(
    
  # Application title
  headerPanel('Instructor Feedback'),
  
  # sidebar with controls to select the variable to plot 
  sidebarPanel(
    # make a slider to choose which domains/careers to show
    sliderInput(inputId = 'percent_choose', label = 'Choose how many categories to show (based on percentage)',
                min = 0, max = 20, value = 5, step = 1, round = TRUE, pre = 'categories > ', post = '%', dragRange = FALSE),
    # 
    selectInput(inputId = 'plottype', label = 'Choose a plot type for domain and career stage:',
                choices = c('circleplot','barplot'))
  ), # end sidebarPanel
  
  # main panel with text and visualisations
  mainPanel(
    h3(textOutput(outputId = 'caption')),
    h4(textOutput(outputId = 'os_heading')),
    textOutput(outputId = 'os'),
    plotOutput(outputId = 'osPlot'),
    h4(textOutput(outputId = 'dom_heading')),
    textOutput(outputId = 'dom'),
    conditionalPanel(condition = "input.plottype == 'barplot'",
                     plotOutput(outputId = 'domPlotbar')),
    conditionalPanel(condition = "input.plottype == 'circleplot'",
                     ggiraphOutput(outputId = 'domPlotcircle')),
    h4(textOutput(outputId = 'occ_heading')),
    textOutput(outputId = 'occ'),
    conditionalPanel(condition = "input.plottype == 'barplot'",
                     plotOutput(outputId = 'occPlotbar')),
    conditionalPanel(condition = "input.plottype == 'circleplot'",
                     ggiraphOutput(outputId = 'occPlotcircle')),
    h4(textOutput(outputId = 'use_heading')),
    plotOutput(outputId = 'useViolin'),
    h4(textOutput(outputId = 'skill_heading')),
    plotOutput(outputId = 'skillViolin'),
    h4(textOutput(outputId = 'reason_heading'))
  ) # end mainPanel
  ) # end pageWithSidebar
) # end shinyUI



# ----- define server logic required to plot various variables -----
server <- shinyServer(function(input, output){
  
  # compute the formula text in a reactive expression since it is 
  # shared by the output$caption and output$osPlot expressions
  formulaText <- reactive({
    paste('Results from the pre-workshop survey for your upcoming ', 
          workshoptype, 'Workshop')
  })
  # return the formula text for printing as a caption
  output$caption <- renderText({
    formulaText()
  })
  
  # ----- operation system ---
  output$os_heading <- renderText({
    'Which operation system do the participants have?'
  })
  output$os <- renderText({
    paste('Most participants in your workshop will have',
          pre_os_cnt$os[pre_os_cnt$n==max(pre_os_cnt$n)], 
          'computers.')
  })
  
  # ----- domain -----
  output$dom_heading <- renderText({
    'Which domain are the participants come from?'
  })
  output$dom <- renderText({
    paste('Participants in your workshop mainly have a background in ',
          pre_dom_cnt$group[1], ', ', pre_dom_cnt$group[2], ', and ', pre_dom_cnt$group[3], '.', sep = '')
  })
  
  # ----- occupation -----
  output$occ_heading <- renderText({
    'At which career stage are the participants?'
  })
  output$occ <- renderText({
    paste('Most participants in your workshop will be ',
          pre_occ_cnt$group[1], ', ', pre_occ_cnt$group[2], ', and ', pre_occ_cnt$group[3], '.', sep = '')
  })

  # ----- usage of software -----
  output$use_heading <- renderText({
    'What is the usage of different programs?'
  })
  
  # ----- skills -----
  output$skill_heading <- renderText({
    'What are the programming skills of participants?'
  })
  
  # ----- reason attend -----
  output$reason_heading <- renderText({
    'What are the reasons of participants to attend the workshop?'
  })
  
  
  # ---- generate plots of the requested variables ----
  # ---- start plot 1 - operation system ----
  output$osPlot <- renderPlot({ #width = 300, height = 300, {
    # prepare data for plotting
    waffle(pre_os_cnt,
           legend_pos = 'top')
    }) # end plot 1
  
  # ---- start plot 2 - barplot for domain ----
  output$domPlotbar <- renderPlot(width = 500, height = 250, {
    validate(need(input$plottype == "barplot", message=FALSE))
    # prepare data for plotting
    # combine rare domains (<xx percent) into group Other based on input slider
    pre_dom_pl <- pre_pl(pre_dom_cnt, 
                         column_comb = 'group', 
                         percent_comb = input$percent_choose)
    # call custom function barplot_cust() to make the plot
    barplot_cust(pre_dom_pl)
  })# end plot 2 - barplot
  
  # ---- start plot 2 - circle plot for domain ----
  output$domPlotcircle <- renderggiraph({
    validate(need(input$plottype == "circleplot", message=FALSE))
    # prepare data for plotting
    pre_dom_pl <- pre_pl(pre_dom_cnt, 
                   column_comb = 'group', 
                   percent_comb = input$percent_choose)
    # call custom function circleplot() to make the plot
    circleplot(pre_dom_pl)
  }) # end plot 2 circle plot - domain

  
  # ---- start plot 3 - barplot for occupation ----
  output$occPlotbar <- renderPlot(width = 500, height = 250, {
    validate(need(input$plottype == "barplot", message=FALSE))
    # prepare data for plotting
    # combine rare domains (<xx percent) into group Other based on input slider
    pre_occ_pl <- pre_pl(pre_occ_cnt, 
                         column_comb = 'group', 
                         percent_comb = input$percent_choose)
    # call custom function barplot_cust() to make the plot
    barplot_cust(pre_occ_pl)
  })# end plot 3 - barplot - occupation
  
  # ---- start plot 3 - circle plot for occupation ----
  output$occPlotcircle <- renderggiraph({
    validate(need(input$plottype == "circleplot", message=FALSE))
    # prepare data for plotting
    pre_occ_pl <- pre_pl(pre_occ_cnt, 
                         column_comb = 'group', 
                         percent_comb = input$percent_choose)
    # call function circleplot() to make the plot
    circleplot(pre_occ_pl)
  }) # end plot 2 circle plot - occupation
  
  # ----- start plot 4 - violin plot of usage ----
  output$useViolin <- renderPlot({
    violin_plot(usage_fact, usage_levels_mean)
  }) #end plot4
  
  # ----- start plot 5 - violin plot of skill ----
  output$skillViolin <- renderPlot({
    violin_plot(skill_lnum, skill_mean)
  })
  
  
}) # end server

shinyApp(ui = ui, server = server)
