
#############################  Loading Dataset    ##############################
################################################################################

setwd("E:/Trent/Data Visualization/Trem_project (0)")
library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)
library(plotly)
library(arules)
library(arulesViz)
library(visNetwork)
library(igraph)
library(kableExtra)

departments <- read.csv("departments.csv")
orders <- read.csv("orders.csv")
products <- read.csv("products.csv")
order_products <- read.csv("order_products__train.csv")

########################### Data Pre-processing  ###############################
################################################################################

order_summary <- sqldf("SELECT order_dow, order_hour_of_day FROM orders")

prior_order <- sqldf("SELECT order_id, days_since_prior_order, order_dow FROM orders 
              GROUP BY order_id")

product_department <- sqldf("SELECT products.product_id, departments.department
             FROM products
             INNER JOIN departments ON products.department_id = departments.department_id ")


reorders <- sqldf(("SELECT departments.department_id, departments.department, products.product_id, products.product_name, 
       order_products.order_id, orders.order_dow
       FROM products 
       INNER JOIN departments
            ON departments.department_id = products.department_id 
       INNER JOIN order_products
            ON products.product_id = order_products.product_id
       INNER JOIN orders
            ON  order_products.order_id = orders.order_id"))

 

reorders1 <- sqldf("SELECT department_id, department, product_name, count(order_id) AS total_orders, order_dow
FROM reorders
GROUP BY department_id
ORDER BY order_id ")

reorders2 <- sqldf("SELECT product_name, department_id, department, COUNT(order_id) AS Total_orders, order_dow
                   FROM reorders
                   GROUP BY product_name")

order_baskets=order_products %>% 
        inner_join(products, by="product_id") %>% 
        group_by(order_id) %>%
        summarise(basket = as.vector(list(product_name)))


# Create transaction data
transactions <- as(order_baskets$basket, "transactions")
inspect(transactions[1])

# Implementing Apriori Algorithm
rules <- apriori(transactions, parameter = list(support = 0.005, confidence = 0.25))

# Remove redundant rule    
rules <- rules[!is.redundant(rules)]
rules_dt <- data.table( lhs = labels( lhs(rules) ), 
                        rhs = labels( rhs(rules) ), 
                        quality(rules) )[ order(-lift), ]
head(rules_dt,5)


##################################################################
