############################  Instacart Data analytics    ######################
################################################################################

#loading libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidyr)
library(dplyr)
library(sqldf)
library(ggplot2)
library(DT)
library(hrbrthemes)
library(viridis)
library(plotly)
library(arules)
library(arulesViz)
library(visNetwork)
library(igraph)
library(RColorBrewer)
library(kableExtra)
setwd("E:/Trent/Data Visualization/Trem_project (0)")

source("Preprocessing.R")
 #############################  Loading Dataset    ##############################
 ################################################################################
 
 #departments <- read.csv("departments.csv")
# orders <- read.csv("orders.csv")
# products <- read.csv("products.csv")
 #order_products <- read.csv("order_products__train.csv")
 
# ########################### Data Pre-processing  ###############################
# ################################################################################
# 
 #order_summary <- sqldf("SELECT order_dow, order_hour_of_day FROM orders")
 
 #prior_order <- sqldf("SELECT order_id, days_since_prior_order, order_dow FROM orders 
  #             GROUP BY order_id")
 
 #product_department <- sqldf("SELECT products.product_id, departments.department
  #            FROM products
   #           INNER JOIN departments ON products.department_id = departments.department_id ")
 
 
 #reorders <- sqldf(("SELECT departments.department_id, departments.department, products.product_id, products.product_name, order_products.order_id, orders.order_dow
  #      FROM products 
   #     INNER JOIN departments
    #         ON departments.department_id = products.department_id 
     #   INNER JOIN order_products
      #       ON products.product_id = order_products.product_id
       # INNER JOIN orders
        #     ON  order_products.order_id = orders.order_id"))
 
 #reorders1 <- sqldf("SELECT department_id, department, product_name, count(order_id) AS total_orders, order_dow
 #FROM reorders
 #GROUP BY department_id
 #ORDER BY order_id ")

 #reorders2 <- sqldf("SELECT product_name, department_id, department, COUNT(order_id) AS Total_orders, order_dow
  #                  FROM reorders
   #                 GROUP BY product_name")
#

##################################   UI    #####################################
################################################################################

# Define UI for application 
ui <- dashboardPage(
    skin = "green",
    
    dashboardHeader(title = "Instacart Analysis",
                    tags$li(class="dropdown",tags$a(href='https://www.linkedin.com/in/harshil-patel-6a9481142/',icon("linkedin"),"Harshil",target="_blank")),
                    tags$li(class="dropdown",tags$a(href='https://www.linkedin.com/in/jenil-bhavsar/',icon("linkedin"),"Jenil",target="_blank"))),
    dashboardSidebar(sidebarMenu(
        menuItem("Orders", tabName = "first"),
        menuItem("Products", tabName = "second"),
        menuItem("Interpretation and Analysis",tabName = "fourth"),
        menuItem("Details", tabName = "thrid")
        
    )),
    dashboardBody(# h1("Orders Analysis"),
        
        
        tabItems(
            #first tab COntent 
            tabItem(tabName = "first",
                # h2("Employee Overview")
                
                fluidRow(
                  box(selectInput(inputId = "order_dow",
                                  label = "Day of week",
                                  choices = c("Sunday"=0, "Monday"=1, "Tuesday"=2 , "Wednesday"=3, "Thursday"=4, "Friday"=5, "Saturday"=6),
                                  selected = 1))
                ),
                
                
                fluidRow(
                    
                    box(title="Orders hour of the day", status="primary", solidHeader = TRUE, collapsible = TRUE,
                        plotOutput("plot1", height = 450)),
                    
                    box(title="Days since prior order", status="primary", solidHeader = TRUE, collapsible = TRUE,
                        plotOutput("plot2", height = 450))
                ),

            ),
            
            
            
            #Second tab COntent 
            tabItem(
                tabName = "second",
                
                
                fluidRow(
                         plotOutput("plot3"),

                ),
                
                fluidRow(
                    br(),
                    br(),

                ),
                
                fluidRow(
                    
                    box(title="Bestselling Products", status="primary", solidHeader = TRUE, collapsible = TRUE,
                        plotOutput("plot6")),
                    
                    box(title="Most Reordered Products", status="primary", solidHeader = TRUE, collapsible = TRUE,
                        plotOutput("plot7"))
                ),
                fluidRow(
                    
                    box(title="Orders and Reordering association", status="primary", solidHeader = TRUE, collapsible = TRUE,
                        plotOutput("plot4")),
                    
                    box(title="Items first in Cart", status="primary", solidHeader = TRUE, collapsible = TRUE,
                        plotOutput("plot5"))
                ),
            ),
            
            tabItem(
                tabName = "fourth",
                
                
                fluidRow(
                    box(title="Frequency for most selling products", status="primary", solidHeader = TRUE, collapsible = TRUE,
                        plotOutput("plot10",height = "450px")),
                    
                    box(title="Support vs Confidence scatterplot", status="primary", solidHeader = TRUE, collapsible = TRUE,
                        plotlyOutput("plot11", height = "450px"))
                    
                ),
                fluidRow(
                    br(),
                    br(),
                    
                ),
                
                fluidRow(
                    visNetworkOutput("plot12",height = "550px"),
                    
                )
                ),
            
            # Third Tab Content
            tabItem(tabName = "thrid",
                    # 
                    fluidRow(
                        box(width = 20,
                            background = "green",
                            DT::dataTableOutput("table")
                        )
                    )
                    
            )
        ))
)


##############################   Server    #####################################
################################################################################
server <- function(input, output) {
    
    
    x1 <- reactive({
        req(input$order_dow)
          
        order_summary <- order_summary %>%
                         filter(order_dow %in% input$order_dow)
    })
    
    x2 <- reactive({
        req(input$order_dow)
        prior_order <- prior_order %>%
            filter(order_dow %in% input$order_dow)
        
        
    }) 
    


    x4 <- reactive({
        req(input$order_dow)
        reorders1 <- reorders1 %>%
            filter(department %in% input$department)
        
        
    })
    
    reactiveData <- reactive({
        req(input$tableinput)
        x5 <- reorders %>%
            filter(department %in% input$tableinput
                 
            )
        
    })

    output$plot1 <- renderPlot({
        ggplot(x1(), aes(x=order_hour_of_day)) +
            geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
            labs(y="Total Number of Orders", x = "Hour of the day") +
            theme(
                panel.background = element_blank(),
                panel.border = element_rect(fill=NA)
            )
    })
    
    
    output$plot2 <- renderPlot({
        ggplot(x2(), aes(x=days_since_prior_order)) +
            geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
            labs(y="Total Number of Orders", x = "Day Of The Month") +
            theme(
                panel.background = element_blank(),
                panel.border = element_rect(fill=NA)
            )
        
    })
    
    output$plot3 <- renderPlot({
         ggplot(reorders1, aes(x=department, y=total_orders)) + 
            geom_bar(stat = "identity", fill = "#69b3a2") +
            ggtitle("Total number of orders by department") +
            labs(y="Total Number of Orders", x = "Department") +
            theme(
                panel.background = element_blank(),
                panel.border = element_rect(fill=NA)
            )
    })

    #Orders and Reordering association
    output$plot4 <- renderPlot({
        order_products %>% 
            group_by(product_id) %>% 
            summarize(proportion_reordered = mean(reordered), n=n()) %>%
            ggplot(aes(x=n,y=proportion_reordered))+
            labs(y="Total Number of Orders", x = "Products") +
            geom_point()+
            geom_smooth(color="red")+
            coord_cartesian(xlim=c(0,2000)) +
            theme(
                panel.background = element_blank(),
                panel.border = element_rect(fill=NA)
            )
    })
    
    # First in Cart
    output$plot5 <- renderPlot({
        cart <- order_products %>% 
            group_by(product_id, add_to_cart_order) %>% 
            summarize(count = n()) %>% mutate(pct=count/sum(count)) %>% 
            filter(add_to_cart_order == 1, count>10) %>% 
            arrange(desc(pct)) %>% 
            left_join(products,by="product_id") %>% 
            select(product_name, pct, count) %>% 
            ungroup() %>% 
            top_n(10, wt=pct)
        
        cart %>% 
            ggplot(aes(x=reorder(product_name,-pct), y=pct))+
            geom_bar(stat="identity",fill="#69b3a2")+
            labs(y="Proportion Ordered", x = "Product Name") +
            theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+coord_cartesian(ylim=c(0.4,0.7)) +
            theme(
                panel.background = element_blank(),
                panel.border = element_rect(fill=NA)
            )
    })
    
    # Bestsellers
    output$plot6 <- renderPlot({
        bestsellers <- order_products %>% 
            group_by(product_id) %>% 
            summarize(count = n()) %>% 
            top_n(10, wt = count) %>%
            left_join(select(products,product_id,product_name),by="product_id") %>%
            arrange(desc(count))
        
        bestsellers %>% 
            ggplot(aes(x=reorder(product_name,-count), y=count))+
            geom_bar(stat="identity",fill="#69b3a2")+
            labs(y="Proportion Reordered", x = "N") +
            theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank()) +
            theme(
                panel.background = element_blank(),
                panel.border = element_rect(fill=NA)
            )
            
    })
    
    #Most reorderd products

    output$plot7 <- renderPlot({
        reordered_products <-order_products %>% 
            group_by(product_id) %>% 
            summarize(proportion_reordered = mean(reordered), n=n()) %>% 
            filter(n>40) %>% 
            top_n(10,wt=proportion_reordered) %>% 
            arrange(desc(proportion_reordered)) %>% 
            left_join(products,by="product_id")
        
        reordered_products %>% 
            ggplot(aes(x=reorder(product_name,-proportion_reordered), y=proportion_reordered))+
            geom_bar(stat="identity",fill="#69b3a2")+
            labs(y="PCT", x = "Product Name") +
            theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+coord_cartesian(ylim=c(0.85,0.95)) +
            theme(
                panel.background = element_blank(),
                panel.border = element_rect(fill=NA)
            )
        
    })
    
    output$plot10 <- renderPlot({
        arules::itemFrequencyPlot(transactions,
                                  topN=20,
                                  col=brewer.pal(8,'Pastel2'),
                                  main='Relative Item Frequency Plot',
                                  type="relative",
                                  ylab="Item Frequency (Relative)")
        
    })
    
    output$plot11 <- renderPlotly({
        
        
        ggplotly(rules_dt %>% ggplot(aes(x=support, y=confidence, color = rules_dt$lift, text = paste(
            "Product :",lhs, " => ", rhs,"\n","Support: ", round(support, digits = 3), "\n",
            "Lift: ", round(lift, digits = 3), "\n",
            "Confidence: ", round(confidence, digits = 3), "\n",
            sep = ""
        )))+geom_point(alpha=0.7)+ labs(x="Support", y="Confidence", color = "Lift"), tooltip = "text")
    })
    
    output$plot12 <- renderVisNetwork({
        plot(sort(rules, by="confidence"), method = "graph", engine = "htmlwidget")
    })
    
    output$table <- renderDT({
        reorders2 %>%
            datatable(colnames = c("Product Name", "Department ID", "Department", "Total Orders", 
                                   "Order Day of Weeek"),
                      extensions = "Buttons",
                      options = list(
                          buttons = c("excel", "csv"), dom = "Bftip"),
                      rownames = FALSE
            )
        
        
    })
    
}


# Running the application 
shinyApp(ui = ui, server = server)
# 

