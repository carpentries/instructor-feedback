library(shiny)
library(tidyverse)
library(waffle)
library(packcircles)  # package to calculate postion of circles!
library(viridis)
library(ggiraph)  # interactive graphs
library(Cairo)

# usecairo = T from package Cairo for better quality of figures in shiny app
options(shiny.usecairo=T)

# start shiny app with shiny::runApp('shinyapp')

# ---- functions for data preparation ----
# function to make dataframe with counts/percentages of categories
pre_cnt <- function(x, groupvar){
  x_out <- x %>%
    dplyr::group_by_(.dots = groupvar) %>%  # using group_by_() evaluates content of groupvar!
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(n_percent = round(100 * n/sum(n))) %>% 
    arrange(desc(n_percent))
  return(x_out)
} #end function

# function to separate strings
sep_str <- function(x){
  require(dplyr)
  x_out <- x %>%
    strsplit(';') %>% 
    lapply(stringi::stri_trim) %>%
    unlist()
  return(x_out)
} #end function

# function to prepare data for plot, combine rare domains (<xx percent) into group Other, sort
pre_pl <- function(x, column_comb, percent_comb = 2){
  # check each row whether minimum percentage reached, if not change name of category to 'Other'
  for(index in seq(1, dim(x)[1])){
    if(x[index,'n_percent'] < percent_comb){
      x[index,column_comb] <- 'Other' 
    } #end if statement
  } #end for loop
  # recalculate n and percentage with category 'Other'
  x_out <- x %>%
    dplyr::group_by_(column_comb) %>%
    dplyr::summarise(n_s = sum(n)) %>%
    dplyr::mutate(n_percent_s = round(100 * n_s/sum(n_s))) %>% 
    dplyr::ungroup() %>%
    arrange(desc(n_s))
  return(x_out)
} #end function

# function to omit na values, omit columns, reshape data into long format 
data_long <- function(x, columns_to_remove = c(1:3)){
  x_out <- x %>% 
    na.omit() %>% 
    select(-columns_to_remove) %>% 
    gather(key = question, value = level)
} #end function

# function to make question and levels a factor with custom re-ordered levels, make a variable with numeric score for each level
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

# function to assign numeric values to agreement levels
levels_num <- function(x){
  x_out <- x %>%
    mutate(level_weight = ifelse(level == 'Strongly disagree', 1, 
                                 ifelse(level == 'Disagree', 2, 
                                        ifelse(level == 'Neutral', 3,
                                               ifelse(level == 'Agree', 4, 5)))))
  return(x_out)
} #end function

# function to calculate mean and other stats of level scores
levels_mean <- function(x){
  x_out <- x %>% 
    group_by(question) %>% 
    summarise(mean_level = mean(level_weight), std_level = sd(level_weight),
              min_level = min(level_weight), max_level = max(level_weight),
              median_level = median(level_weight))
  return(x_out)
} #end function


# ----- plot functions -----
# function to make an interactive circleplot
circleplot <- function(x){
  pl_data <- x
  # add display text to data for interactivity
  pl_data$text_short <- stringr::str_sub(pl_data$group, start=1, end=3)
  # Add a column with the text you want to display for each bubble:
  pl_data$text <- paste(pl_data$group, "\n", "n =", pl_data$n_s)
  
  # Generate the layout of circles
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
  
  # display final interactive plot
  girafe(ggobj = p)
} # end function circleplot

# function for customised barplot
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

# function for violin plot to show distribution with interactive points of mean scores
violin_plot <- function(data_violin, data_point, col_violin='skyblue', col_point='blue', girafe = TRUE){
  p = ggplot() +
    # the violin plot to show score distribution
    geom_violin(data = data_violin, aes(x = question, y = level_weight), color = NA, fill = col_violin, alpha = 0.3) + 
    # the dot plot displaying mean score when hovering over 
    geom_point_interactive(data = data_point, aes(x = question, y = mean_level, 
                                                  tooltip = paste('mean score =', as.character(round(mean_level,1))), data_id = as.character(round(mean_level,1))), 
                           size = 3, color = col_point) +
    # show text levels as axis label instead of numeric scores
    scale_y_continuous(breaks = 1:length(levels(data_violin$level)),
                       labels = levels(data_violin$level)) +
    coord_flip() +
    theme_classic() +
    theme(axis.title = element_blank(),
          panel.border = element_rect(fill = NA),
          text = element_text(size = 16),
          axis.text.x = element_text(angle = 45, hjust = 1)
    )
  if(girafe == TRUE){
  # display final interactive plot
  girafe(ggobj = p)
  }else{
    return(p)
  }#end if
} #end function violin_plot

# function for stem plot
stem_plot <- function(pl_data){
  x_var <- reorder(pl_data$group, pl_data$n_s)
  p <- ggplot(data = pl_data) +
  geom_segment(aes(x = x_var, xend = x_var, 
                    y=0, yend=n_percent_s), color="skyblue") +
  geom_point(aes(x = x_var, y = n_percent_s), color="blue", size=4, alpha=0.6) +
  # add text on top of stem, the question
  geom_text(aes(x = x_var, y = 0, label = x_var), 
            stat = 'identity', size = 6, nudge_x = 0.35, hjust = 'left' ) + 
  # add text at end of stem, the count and percentage of answers
  geom_text(aes(x = x_var, y = n_percent_s + 0.75, label = paste(n_s, ' (', n_percent_s, '%)', sep = '')), 
            stat = 'identity', size = 5, hjust = 'left') + 
  coord_flip() +
  labs(y = '% of participants', x = '') +
  theme_void() + 
  theme(plot.margin = unit(c(0.25,2,0.25,0.25),"cm"))

  # turn off clipping of text at the end of plot area
  library(grid)
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  p2 <- grid.draw(gt, recording = F)

  return(p2)
} # end function stem_plot


# ----- data import and pre-preparation -----
### DATA IMPORT NEEDS TO BE AUTOMATED! ###
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
# --- split up listed domains from each observation ---
domains <- sep_str(pre_dom$domain)
# combine some similar domains from main list
domains[domains == 'Medicine'] <- 'Biomedical or Health Sciences'
domains[domains == 'Biomedical or Health Sciences'] <- 'Medical or Health Sciences'
domains[domains == 'Earth Sciences'] <- 'Planetary Sciences (Geology, Climatology, Oceanography, etc.)'
# make a tibble with domains
domains <- dplyr::tibble(group = domains)
# make dataframe with counts/percentages of domains
pre_dom_cnt <- pre_cnt(domains, 
                       groupvar = 'group')

# --- split up listed domains from each observation ---
occupation <- sep_str(pre_dom$occupation)
# make a tibble with occupations
occupation <- dplyr::tibble(group = occupation)
# make dataframe with counts/percentages of occupations
pre_occ_cnt <- pre_cnt(occupation, 
                       groupvar = 'group')

# ----- What are reasons for attending? -----
reason <- read_csv('data/20181002-pre-reason_attending-no-open.csv')
# split strings
attend <- sep_str(reason$why_attend)
# make a tibble with attend reasons
attends <- dplyr::tibble(group = attend)
# make dataframe with counts/percentages
pre_att_cnt <- pre_cnt(attends, groupvar = 'group')
# replace missing values with 'unknown'
pre_att_cnt <- pre_att_cnt %>% 
  replace_na(list(group = 'Unknown'))

# ----- What is the usage of different types of software? -----
usage <- read_csv('data/20181002-pre-usage_profile-no-open.csv')
# convert data to long format
usage_long <- data_long(usage, c(1:2,8))
# define custom question text for display with plot
question_levels <- unique(usage_long$question)
question_labels <- c('command line', 'version control system', 'databases', 
                     'programming language', 'statistical software with GUI')
answer_levels <- c('Never', 'Less than once per year', 'Several times per year',
                   'Monthly', 'Weekly', 'Daily')
# make questions and answers a factor
usage_fact <- data_factor(usage_long, question_levels, question_labels, answer_levels)
# calculate average usage
usage_levels_mean <- levels_mean(usage_fact)

# ----- What is the skill before workshop? -----
skill_pre <- read_csv('data/20181002-pre-skill_pre-no-open.csv')
# convert data to long format
skill_pre_long <- data_long(skill_pre)
# define custom question text for display with plot
skillqu_levels = c('skill_efficient_programming','skill_reproducibility_programming','skill_confidence_programming',
                 'skill_overcome_problem','skill_search_answers','skill_write_program','skill_data_raw')
skillqu_labels = c('programming makes analysis efficient',
                 'programming makes analysis reproducible','confident to use programming', 'can overcome problem',
                 'can search for answers','can write program','raw data important')
skillans_levels = c('Strongly disagree', 'Disagree', 'Neutral', 'Agree', 'Strongly agree')
# make questions and answers a factor
skill_pre_fact <- data_factor(skill_pre_long, skillqu_levels, skillqu_labels, skillans_levels)
# calculate average usage
skill_pre_lnum <- levels_num(skill_pre_fact)
skill_pre_mean <- levels_mean(skill_pre_lnum)

# ----- What is the skill after workshop? -----
skill_post <- read_csv('data/20181002-post-skills-no-open.csv')
# convert data to long format
skill_post_long <- data_long(skill_post)
# make questions and answers a factor
skill_post_fact <- data_factor(skill_post_long, skillqu_levels, skillqu_labels, skillans_levels)
# calculate average usage
skill_post_lnum <- levels_num(skill_post_fact)
skill_post_mean <- levels_mean(skill_post_lnum)

# ----- end of data import -----


# ----- start of shiny app specific part -----
# ----- define UI (user interface object) for application -----
ui <-  fluidPage(

  # Application title
  titlePanel('Instructor Feedback'),
  
  # ----- start first panel -----
  tabsetPanel(
    tabPanel('Pre-Workshop Survey',
             sidebarLayout(
               
               # sidebar with controls to select variables to plot etc.
               sidebarPanel(
                 # make a checkbox to choose which questions to display
                 checkboxGroupInput(inputId = 'question_choose', label = 'Choose which questions to display', 
                                    choices = c('operation system','domain','occupation','reason to attend','usage of programs','skills'), 
                                    selected = 'operation system', inline = FALSE
                 ), # end checkbox
                 # make a slider to choose how many domains/careers to show depending on percentage
                 sliderInput(inputId = 'percent_choose', label = 'Choose how many categories to show (based on percentage)',
                             min = 0, max = 20, value = 2, step = 1, round = TRUE, pre = 'categories > ', post = '%', dragRange = FALSE
                 ), # end slider
                 # make drop down selection menu to choose a plot type for domain and occupation
                 selectInput(inputId = 'plottype', label = 'Choose a plot type for domain and career stage:',
                             choices = c('circleplot','barplot')
                 ) # end select
               ), # end sidebarPanel
               
               # main panel with text and visualisations
               # (text and vis are shown in the order they appear here)
               mainPanel(
                 # use fluidRow with aling='center' to center all text, vis ...
                 fluidRow(
                   column(width = 10, align = "center",
                          # start with actual output to display
                          h3(textOutput(outputId = 'caption_pre')),
                          # start os vis
                          conditionalPanel(condition = "input.question_choose.includes('operation system')",  #condition needs to be in javascript code!
                                           h4(textOutput(outputId = 'os_heading')),
                                           textOutput(outputId = 'os'),
                                           plotOutput(outputId = 'osPlot', width = "60%")
                          ), #end os vis
                          # start domain vis
                          conditionalPanel(condition = "input.question_choose.includes('domain')",
                                           h4(textOutput(outputId = 'dom_heading')),
                                           textOutput(outputId = 'dom'),
                                           conditionalPanel(condition = "input.plottype == 'barplot'",
                                                            plotOutput(outputId = 'domPlotbar')),
                                           conditionalPanel(condition = "input.plottype == 'circleplot'",
                                                            ggiraphOutput(outputId = 'domPlotcircle', width = "80%"))
                          ), #end domain vis
                          # start occupation vis
                          conditionalPanel(condition = "input.question_choose.includes('occupation')",
                                           h4(textOutput(outputId = 'occ_heading')),
                                           textOutput(outputId = 'occ'),
                                           conditionalPanel(condition = "input.plottype == 'barplot'",
                                                            plotOutput(outputId = 'occPlotbar')),
                                           conditionalPanel(condition = "input.plottype == 'circleplot'",
                                                            girafeOutput(outputId = 'occPlotcircle'))
                          ), #end occupation vis
                          # start reason attend vis
                          conditionalPanel(condition = "input.question_choose.includes('reason to attend')",
                                           h4(textOutput(outputId = 'reason_heading')),
                                           plotOutput(outputId = 'attendStem', width = "95%")
                          ), #end reason attend vis
                          # start usage vis
                          conditionalPanel(condition = "input.question_choose.includes('usage of programs')",
                                           h4(textOutput(outputId = 'use_heading')),
                                           textOutput(outputId = 'use_text'),
                                           girafeOutput(outputId = 'useViolin')
                          ), #end usage vis
                          # start skill vis
                          conditionalPanel(condition = "input.question_choose.includes('skills')",
                                           h4(textOutput(outputId = 'skill_pre_heading')),
                                           textOutput(outputId = 'skill_pre_text'),
                                           girafeOutput(outputId = 'skill_pre_Violin')
                          ) #end skill vis
                   )) # end fluid row
               ) # end mainPanel
             )  #end sidebarLayout
    ), # end tabPanel 1
    
    # ----- start second panel -----
    tabPanel('Post-Workshop Survey',
            sidebarLayout(
              sidebarPanel(
                # make a checkbox to choose which questions to display
                checkboxGroupInput(inputId = 'question_post_choose', label = 'Choose which questions to display', 
                                   choices = c('skills'), 
                                   selected = 'skills', inline = FALSE
                ), # end checkbox
                # make checkbox asking whether displaying the pre-workshop skill only when skills are chosen in questions
                conditionalPanel(condition = "input.question_post_choose.includes('skills')",
                                 checkboxInput(inputId = 'show_pre_skill', label = 'show pre-workshop skills (in blue)', value = FALSE)
                ) # end checkbox 2
              ), #end sidbarPanel
              mainPanel(
                fluidRow(
                  column(width = 10, align = "center",
                         h3(textOutput(outputId = 'caption_post')),
                         # start skill vis
                         conditionalPanel(condition = "input.question_post_choose.includes('skills')",
                                          # check whether post skill alone or pre and post skill displayed together
                                          h4(textOutput(outputId = 'skill_post_heading')),
                                          textOutput(outputId = 'skill_post_text'),
                                          conditionalPanel(condition = "input.show_pre_skill == 0",
                                                           ggiraphOutput(outputId = 'skill_post_Violin')
                                                           ),
                                          conditionalPanel(condition = "input.show_pre_skill == 1",
                                                           ggiraphOutput(outputId = 'skill_pre_post_Violin')
                                                           )
                                          ) #end of conditional panel for skills
                  )) # end fluid row
             )# end mainPanel
            ) #end sidbarLayout
    ) #end tabPanel 2
  ) #end tabsetPanel
) # end shinyUI



# ----- define server logic required to plot various variables -----
server <- shinyServer(function(input, output){
  
  # ----- headings of panels -----
  output$caption_pre <- renderText({
    paste('Results from the pre-workshop survey for your upcoming ', 
          workshoptype, 'Workshop')
    })
  
  output$caption_post <- renderText({
    paste('Results from the post-workshop survey for your ', 
          workshoptype, 'Workshop')
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
  
  # ----- reason attend -----
  output$reason_heading <- renderText({
    'What are the reasons of participants to attend the workshop?'
  })

  # ----- usage of software -----
  output$use_heading <- renderText({
    'What is the usage of different programs?'
  })
  output$use_text <- renderText({
    'Average usage of all participants on top of distribution (usage levels were translated to scores 1 to 6 before calculations)'
  })
  
  # ----- skills pre -----
  output$skill_pre_heading <- renderText({
    'What are the programming skills of participants?'
  })
  output$skill_pre_text <- renderText({
    'Average skills of all participants on top of distribution (agreement levels were translated to scores 1 to 5 before calculations)'
  })
  
  # ----- skills post -----
  output$skill_post_heading <- renderText({
    'What are the programming skills of participants?'
  })
  output$skill_post_text <- renderText({
    'Average skills of all participants on top of distribution (agreement levels were translated to scores 1 to 5 before calculations)'
    })
  output$skill_prepost_text <- renderText({
    'Average skills of all participants on top of distribution (agreement levels were translated to scores 1 to 5 before calculations),
    post workshop answers are displayed in red, pre workshop answers in blue'
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
  
  # ----- start plot 4 - stem plot for reason attend -----
  output$attendStem <- renderPlot({
    # prepare data for plotting
    pre_att_pl <- pre_pl(pre_att_cnt, 
                         column_comb = 'group', 
                         percent_comb = input$percent_choose)
    # make the plot
    stem_plot(pre_att_pl)
  }) #end plot 4
  
  # ----- start plot 5 - violin plot of usage ----
  output$useViolin <- renderggiraph({
    violin_plot(usage_fact, usage_levels_mean)
  }) #end plot 5
  
  # ----- start plot 6 - violin plot of pre skill ----
  output$skill_pre_Violin <- renderggiraph({
    violin_plot(skill_pre_lnum, skill_pre_mean, girafe = TRUE)
  }) #end plot 6
  
  # ----- start plot 7 - violin plot of post skill ----
  output$skill_post_Violin <- renderggiraph({
    violin_plot(skill_post_lnum, skill_post_mean, col_violin = 'pink', col_point = 'red', girafe = TRUE)
  }) #end plot 7
  
  # ----- start plot 8 - violin plot of pre-post skill ----
  output$skill_pre_post_Violin <- renderggiraph({
    # make plot of pre-skills
    violin_pre <- violin_plot(skill_pre_lnum, skill_pre_mean, girafe = FALSE)
    violin_post <- violin_plot(skill_post_lnum, skill_post_mean, col_violin = 'pink', col_point = 'red', girafe = FALSE)
    # add parts for post-skills
    violin_post2 <- violin_pre + 
      geom_violin(data = skill_post_lnum, aes(x = question, y = level_weight), color = NA, fill = 'pink', alpha = 0.3) + 
     geom_point_interactive(data = skill_post_mean, aes(x = question, y = mean_level, 
                                                  tooltip = paste('mean score =', as.character(round(mean_level,1))), data_id = as.character(round(mean_level,1))), 
                          size = 3, color = 'red')
    # make interactive object
    girafe(ggobj = violin_post2)
    }) #end plot 8
  
}) # end server

shinyApp(ui = ui, server = server)
