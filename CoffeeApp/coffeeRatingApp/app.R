
library(shiny)
library(shinydashboard)
library(tidyverse)
library(broom)
library(DT)
library(kableExtra)
coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv') 
coffee_ratings <-  coffee_ratings %>% 
rename(`Country of Origin` = country_of_origin,
           `Total cup Points` = total_cup_points,
       Region = region,
       Species = species)


# Building the User-Interface ---- 

ui <- dashboardPage(
    dashboardHeader(title = "Coffee Rating Dashboard", titleWidth = 400),
    dashboardSidebar(
        selectInput("country", "Select Country", choices = coffee_ratings %>% 
                        select(`Country of Origin`) %>% 
                        distinct() %>% 
                        arrange(`Country of Origin`) %>% 
                        drop_na(), 
                    selected = "Ethiopia"),
        tags$body(
            h4(
        textOutput("info"))),
        tableOutput("txt")
    ),
    dashboardBody(
     
        fluidRow(box(plotOutput("coffee_dist")), box(plotOutput("flavour_profile"))),
        fluidRow(box(plotOutput("coffee_variety")), box(plotOutput("flavour_year"))),
        fluidRow(box(plotOutput("flavour_sig")), box(tableOutput("coffee_table")))
    )
)

# Building Server-Side application ---- 

server <- function(input, output) {
    output$info <- renderText({
          print("Processing Method")
    })
    output$txt <- function(){
        coffee_ratings %>% 
            filter(`Country of Origin` == input$country) %>% 
            group_by(processing_method) %>% 
            count(processing_method, sort = T) %>% 
            drop_na() %>% 
            kable() %>% 
            kable_styling() %>% 
            scroll_box(height = '200px', width = "200px")
    }
    
    output$coffee_dist <- renderPlot({
        coffee_ratings %>% 
            filter(`Country of Origin` == input$country) %>% 
            ggplot(aes(`Total cup Points`))+
            geom_histogram(color = "black")+
            scale_x_continuous(trans = "log2")+
            labs(title = "Distribution of Total Ratings",
                 x = "Total Cup Points")+
            ggeasy::easy_text_colour("#7d5910")
        
    })
    
    output$flavour_profile <- renderPlot({
        coffee_ratings %>% 
            filter(`Country of Origin` == input$country) %>% 
            select(aroma:cupper_points) %>% 
            gather() %>% 
            group_by(key) %>% 
            summarise(value = mean(value)) %>%  # calculate the mean in each metrics
            ungroup() %>% 
            mutate(key = str_replace(key, "_", " ") %>% 
                       str_to_title()) %>% 
            ggplot(aes(key, value , color = key))+
            geom_point(size = 3.5)+
            geom_segment(aes(x = key, xend = key, y = value, yend = 0))+
            theme(legend.position = "none")+
            coord_flip()+
            ylab("")+
            xlab("")+
            labs(title = "Average Flavour Profile")
    })
    output$coffee_variety <- renderPlot({
        coffee_ratings %>% 
            filter(`Country of Origin` == input$country) %>% 
            select(variety) %>% 
            drop_na() %>% 
            count(variety) %>% 
            mutate(variety = fct_reorder(variety, n)) %>% 
            ggplot(aes(n , variety, fill = variety))+
            geom_col(alpha = 0.5)+
            labs(title = "Coffee Bean Variety", y = "", x = "# of Variety")+
            theme(legend.position = 'none')+
            ggeasy::easy_text_colour("#472e2b")
    })
    
    output$flavour_year <- renderPlot({
        coffee_ratings %>% 
            filter(`Country of Origin`==input$country) %>% 
            select(harvest_year, aroma:cupper_points) %>% 
            mutate(Year = readr::parse_number(harvest_year)) %>% 
            mutate(Year = factor(Year )) %>% 
            drop_na() %>% 
            select(-harvest_year) %>% 
            pivot_longer(names_to = "key", values_to = "values",
                         cols = aroma:cupper_points) %>% 
            mutate(key = str_replace_all(key , "_"," ") %>% str_to_title()) %>%
            filter(key %in%c("Body", "Sweetness","Flavor","Aroma")) %>% 
            group_by(Year, key) %>% 
            summarise(Avg = mean(values)) %>% ungroup() %>% 
            ggplot(aes(Year, Avg, color = key))+
            geom_line(aes(group = key))+
            facet_wrap(key ~., scales = "free_y")+
            labs(title = "Average Flavour over Year", x = "")+
            theme(axis.text.x = element_text(angle = 45, hjust = 1))+
            theme(legend.position = "none")
    })
    
    output$flavour_sig <- renderPlot({
        coffee_ratings %>% 
            select(`Country of Origin`, aroma:cupper_points) %>% 
            mutate(baseline = if_else(`Country of Origin` == input$country,
                                      "Highlight", "Not-Highlight")) %>% 
            select(-`Country of Origin`) %>% 
            pivot_longer(names_to = "key", values_to = "value", 
                         cols = aroma:cupper_points) %>% 
            group_by(key) %>% 
            do(t_test = t.test(value ~ baseline, data =.) %>% tidy()) %>% 
            unnest(t_test) %>% 
            mutate(sig_difference = case_when(
                conf.low < 0 & conf.high < 0 ~ "Significant",
                conf.low > 0 & conf.high > 0 ~ "Significant",
                TRUE ~ "Not-Significant"
            )) %>% 
            mutate(key = str_replace(key, "_", " ") %>% str_to_title()) %>% 
            mutate(key = fct_reorder(key, -estimate)) %>%  # decreasing order by estimates
            ggplot(aes(key, estimate, color = sig_difference))+
            geom_pointrange(aes(ymin = conf.low, ymax = conf.high))+
            geom_hline(yintercept = 0, linetype = "dashed", color = "darkblue")+
            coord_flip()+
            theme(legend.position = "none")+
            xlab("")+
            labs(title = "How Significant are the flavour profiles?")
    })
    
    output$coffee_table <- function(){
        coffee_ratings %>% 
            filter(`Country of Origin` == input$country) %>% 
            select(`Total cup Points`, Species, `Country of Origin`, Region) %>% 
            group_by(Species, Region) %>% 
            ungroup() %>% 
            mutate(Region = str_trunc(Region, 9, "right")) %>% 
            drop_na() %>% 
            kable() %>% 
            kable_styling() %>% 
            scroll_box(height = '350px', width = "450px")
    }
}

# Running the application
shinyApp(ui, server)