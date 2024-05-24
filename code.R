# Load required libraries
library(shiny)
library(readr)
library(rpart)
library(rpart.plot)
library(dplyr)
library(lattice)
library(ggplot2)
library(stats)
library(cluster)
library(factoextra)


# Load your dataset
supermarket_sales <- read.csv("supermarket_sales.csv")

# UI definition
ui <- fluidPage( 
  titlePanel("Super Market Sales"), 
  tabsetPanel( 
    tabPanel("Percentage", 
             fluidRow( 
               column(4, plotOutput("branch")), 
               column(4, plotOutput("city")), 
               column(4, plotOutput("type")) 
             ), 
             fluidRow( 
               column(4, plotOutput("gender")), 
               column(4, plotOutput("productLine")), 
               column(4, plotOutput("payment")) 
             ) 
    ), 
    tabPanel("Freqency", 
             fluidRow( 
               column(6, plotOutput("quantity")), 
               column(6, plotOutput("rating")) 
             ) 
    ),          
    tabPanel("Relations Part1", 
             fluidRow( 
               column(6, plotOutput("branch2")), 
               column(6, plotOutput("city1")) 
             ), 
             fluidRow( 
               column(6, plotOutput("city2")), 
               column(6, plotOutput("city3")) 
             ) 
    ),
    tabPanel("Relations part2",
             fluidRow(
               column(4, plotOutput("num1")),
               column(4, plotOutput("num2")),
               column(4, plotOutput("num3"))
             )
    ),
    tabPanel("Relations part3",
             fluidRow( 
               column(8, plotOutput("pl1")), 
               column(8, plotOutput("pl2")) 
             ), 
             fluidRow( 
               column(8, plotOutput("pl3")), 
               column(8, plotOutput("pl4")) 
             ) 
    ),
    tabPanel("Relations part4",
             fluidRow(
               column(6, plotOutput("r1")),
               column(6, plotOutput("r2"))
               
             ),
             fluidRow(
               column(6, plotOutput("r3")),
               column(6, plotOutput("r4"))
             )
    ),
    tabPanel("Quantity's Box Plot Relations",
             fluidRow(
               column(6, plotOutput("q1")),
               column(6, plotOutput("q2"))
             ),
             fluidRow(
               column(6, plotOutput("q3")),
               column(6, plotOutput("q4"))
             )
    ),
    tabPanel("Unit Cost's Box Plot Relations",
             fluidRow(
               column(6, plotOutput("uc1")),
               column(6, plotOutput("uc2"))
             )
    ),
    tabPanel("Revenue's Box Plot Relations",
             fluidRow(
               column(4, plotOutput("rev1")),
               column(4, plotOutput("rev2")),
               column(4, plotOutput("rev3"))
             )
    ),
    
    tabPanel("Cogs' Box Plot Relations",
             fluidRow(
               column(4, plotOutput("c1")),
               column(4, plotOutput("c2")),
               column(4, plotOutput("c3"))
             )
    ),
    tabPanel("Other Box Plot Relations",
             fluidRow(
               column(6, plotOutput("last"))
             )
    ),
    tabPanel("Decision Tree",
             fluidRow(
               column(6, plotOutput("decision_tree_plot")),
               column(6, plotOutput("decision_tree_plot1"))
             )
    ),
    tabPanel("K-Means Clustering",
             fluidRow(
               column(6, plotOutput("kmeans_plot")),
               column(6, plotOutput("kmeans_plot1"))
             ),
             fluidRow(
               column(6, plotOutput("kmeans_plot2")),
               column(6, plotOutput("kmeans_plot3"))
             ),
             fluidRow(
               column(12, plotOutput("kmeans_plot4"))
             )
    )
  ) 
)

# Server function
server <- function(input, output) { 
  
  output$branch <- renderPlot({ 
    x <- table(supermarket_sales$branch) 
    percentage <- paste0(round(100 * x / sum(x)), "%") 
    pie(x, labels = percentage, main = "Branch Distribution", col = c("red", "blue", "green")) 
    legend("bottomright", legend = c("A", "B", "C"), fill = c("red", "blue", "green")) 
  }) 
  
  output$city <- renderPlot({ 
    x <- table(supermarket_sales$city) 
    percentage <- paste0(round(100 * x / sum(x)), "%") 
    pie(x, labels = percentage, main = "City Distribution Of Customers", col = c("red", "blue", "green")) 
    legend("bottomright", legend = c("Naypyitaw", "Mandalay", "Yangon"), fill = c("red", "blue", "green")) 
  }) 
  
  output$type <- renderPlot({ 
    x <- table(supermarket_sales$customer_type) 
    percentage <- paste0(round(100 * x / sum(x)), "%") 
    pie(x, labels = percentage, main = "Type Distribution of Customers", col = c("red", "blue")) 
    legend("bottomright", legend = c("Member", "Normal"), fill = c("red", "blue")) 
  }) 
  
  output$gender <- renderPlot({ 
    x <- table(supermarket_sales$gender) 
    percentage <- paste0(round(100 * x / sum(x)), "%") 
    pie(x, labels = percentage, main = "Gender Distribution of Customers", col = c("pink", "skyblue")) 
    legend("bottomright", legend = c("Female", "Male"), fill = c("pink", "skyblue")) 
  }) 
  
  output$productLine <- renderPlot({ 
    x <- table(supermarket_sales$product_line) 
    percentage <- paste0(round(100 * x / sum(x)), "%") 
    pie(x, labels = percentage, main = "Product line Distribution of Customers", col = c("red", "blue", "green", "black", "white", "orange", "yellow")) 
    legend("bottomright", legend = c("Electronic accessories", "Sports and travel", "Fashion accessories", "Health and beauty", "Home and lifestyle", "Food and beverages"), fill = c("red", "blue", "green", "black", "white", "orange", "yellow")) 
  }) 
  
  output$payment <- renderPlot({ 
    x <- table(supermarket_sales$payment) 
    percentage <- paste0(round(100 * x / sum(x)), "%") 
    pie(x, labels = percentage, main = "Payment Distribution", col = c("red", "blue", "yellow")) 
    legend("bottomright", legend = c("Cash", "Credit card", "Ewallet"), fill = c("red", "blue", "yellow")) 
  }) 
  
  output$quantity <- renderPlot({ 
    x <- table(supermarket_sales$quantity) 
    barplot(x, main = "Quantity purchased", xlab = "Quantity", ylab = "Frequency", col = "red") 
  })
  
  output$rating <- renderPlot({
    x <- table(supermarket_sales$rating) 
    barplot(x, main = "Rating", xlab = "Rating", ylab = "Frequency", col = "red")
  })
  
  output$branch2 <- renderPlot({ 
    tmp <- table (supermarket_sales$city, supermarket_sales$branch)
    percentages <- prop.table(tmp, margin = 1) * 100
    barplot(percentages, beside = TRUE, legend.text = TRUE,
            args.legend = list(x="bottomright"),
            main = "Percentage Distribution of Branch in Each City",
            xlab = "Branch",
            ylab = "Percentage",
            col = c("black","white","gray"))
  }) 
  
  output$city1 <- renderPlot({
    tmp <- table(supermarket_sales$city, supermarket_sales$gender)
    percentages <- prop.table(tmp, margin = 1) * 100
    barplot(percentages, beside = TRUE, legend.text = TRUE,
            args.legend = list(x="bottomright"),
            main = "Percentage Distribution of Gender in Each City",
            xlab = "Gender",
            ylab = "Percentage",
            col = c("black", "white","gray"))  
  })
  
  output$city2 <- renderPlot({ 
    tmp <- table (supermarket_sales$city, supermarket_sales$customer_type)
    percentages <- prop.table(tmp, margin = 1) * 100
    barplot(percentages, beside = TRUE, legend.text = TRUE,
            args.legend = list(x="bottomright"),
            main = "Percentage Distribution of Customer type in Each City",
            xlab = "Customer type",
            ylab = "Percentage",
            col = c("black","white","gray"))
  })
  
  output$city3 <- renderPlot({    
    tmp <- table (supermarket_sales$city, supermarket_sales$payment)
    percentages <- prop.table(tmp, margin = 1) * 100
    barplot(percentages, beside = TRUE, legend.text = TRUE,
            args.legend = list(x="bottomright"),
            main = "Percentage Distribution of Payment type in Each City",
            xlab = "Payment type",
            ylab = "Percentage",
            col = c("black","white","gray"))
  })
  
  output$num1 <- renderPlot({
    plot(supermarket_sales$quantity,supermarket_sales$unit_price,
         main = "Relation between Quantity and Unit Price",
         xlab = "Quantity",
         ylab = "Unit Price",
         col = "red")
  })
  
  output$num2 <- renderPlot({
    plot(supermarket_sales$quantity,supermarket_sales$tax_5_percent,
         main = "Relation between Quantity and Tax",
         xlab = "Quantity",
         ylab = "Tax",
         col = "red")
  })
  
  output$num3 <- renderPlot({
    plot(supermarket_sales$quantity,supermarket_sales$total,
         main = "Relation between Quantity and Total",
         xlab = "Quantity",
         ylab = "Total",
         col = "red")
  })
  
  output$pl1 <- renderPlot({
    plot(supermarket_sales$unit_price,supermarket_sales$tax_5_percent,
         main = "Relation between Unit Price and Tax",
         xlab = "Unit Price",
         ylab = "Tax",
         col = "red")
  })
  
  output$pl2 <- renderPlot({
    plot(supermarket_sales$unit_price,supermarket_sales$total,
         main = "Relation between Unit Price and Total",
         xlab = "Unit Price",
         ylab = "Total",
         col = "red")
  })
  
  output$pl3 <- renderPlot({
    plot(supermarket_sales$tax_5_percent,supermarket_sales$total,
         main = "Relation between Tax and Total",
         xlab = "Tax",
         ylab = "Total",
         col = "red")
  })
  
  output$pl4 <- renderPlot({
    plot(supermarket_sales$gross_income,supermarket_sales$rating,
         main = "Relation between Gross Income and Rating",
         xlab = "Gross Income",
         ylab = "Rating",
         col = "red")
  })
  
  output$r1 <- renderPlot({
    plot(supermarket_sales$quantity,supermarket_sales$cogs,
         main = "Relation between Quantity and COGS",
         xlab = "Quantity",
         ylab = "COGS",
         col = "red")
  })
  
  output$r2 <- renderPlot({
    plot(supermarket_sales$quantity,supermarket_sales$gross_income,
         main = "Relation between Quantity and Gross Income",
         xlab = "Quantity",
         ylab = "Gross Income",
         col = "red")
  })
  
  output$r3 <- renderPlot({
    plot(supermarket_sales$cogs,supermarket_sales$gross_income,
         main = "Relation between COGS and Gross Income",
         xlab = "COGS",
         ylab = "Gross Income",
         col = "red")
  })
  
  output$r4 <- renderPlot({runApp('11.R')
    plot(supermarket_sales$rating,supermarket_sales$gross_margin_percentage,
         main = "Relation between Rating and Gross Margin Percentage",
         xlab = "Rating",
         ylab = "Gross Margin Percentage",
         col = "red")
  })
  
  output$q1 <- renderPlot({
    boxplot(quantity ~ branch,
            data = supermarket_sales,
            main = "Quantity in Each Branch",
            col = "red")
  })
  
  output$q2 <- renderPlot({
    boxplot(quantity ~ gender,
            data = supermarket_sales,
            main = "Quantity by Gender",
            col = "red")
  })
  
  output$q3 <- renderPlot({
    boxplot(quantity ~ customer_type,
            data = supermarket_sales,
            main = "Quantity by Customer Type",
            col = "red")
  })
  
  output$q4 <- renderPlot({
    boxplot(quantity ~ payment,
            data = supermarket_sales,
            main = "Quantity by Payment Type",
            col = "red")
  })
  
  output$uc1 <- renderPlot({
    boxplot(unit_price ~ branch,
            data = supermarket_sales,
            main = "Unit Price by Branch",
            col = "red")
  })
  
  output$uc2 <- renderPlot({
    boxplot(unit_price ~ customer_type,
            data = supermarket_sales,
            main = "Unit Price by Customer Type",
            col = "red")
  })
  
  output$rev1 <- renderPlot({
    boxplot(total ~ branch,
            data = supermarket_sales,
            main = "Revenue by Branch",
            col = "red")
  })
  
  output$rev2 <- renderPlot({
    boxplot(total ~ city,
            data = supermarket_sales,
            main = "Revenue by City",
            col = "red")
  })
  
  output$rev3 <- renderPlot({
    boxplot(total ~ customer_type,
            data = supermarket_sales,
            main = "Revenue by Customer Type",
            col = "red")
  })
  
  output$c1 <- renderPlot({
    boxplot(cogs ~ branch,
            data = supermarket_sales,
            main = "Cogs by Branch",
            col = "red")
  })
  
  output$c2 <- renderPlot({
    boxplot(cogs ~ city,
            data = supermarket_sales,
            main = "Cogs by City",
            col = "red")
  })
  
  output$c3 <- renderPlot({
    boxplot(cogs ~ customer_type,
            data = supermarket_sales,
            main = "Cogs by Customer Type",
            col = "red")
  })
  
  output$last <- renderPlot({
    boxplot(rating ~ branch,
            data = supermarket_sales,
            main = "Rating by Branch",
            col = "red")
  })
  
  output$decision_tree_plot <- renderPlot({
    tree <- rpart(payment ~ gender + total + cogs + gross_income + gross_margin_percentage + rating, 
                  data = supermarket_sales, method = "class")
    rpart.plot(tree)
  })
  
  output$decision_tree_plot1 <- renderPlot({
    tree <- rpart(payment ~ city + quantity + unit_price + tax_5_percent + date + time, 
                  data = supermarket_sales, method = "class")
    rpart.plot(tree)
  })
  
  output$kmeans_plot <- renderPlot({
    set.seed(123)
    df <- supermarket_sales[, c("quantity", "unit_price", "tax_5_percent")]
    km_res <- kmeans(df, 3, nstart = 25)
    fviz_cluster(km_res, geom = "point", data = df) + ggtitle("k = 3")
  })
  
  output$kmeans_plot1 <- renderPlot({
    set.seed(123)
    df <- supermarket_sales[, c("total", "cogs", "gross_income")]
    km_res <- kmeans(df, 3, nstart = 25)
    fviz_cluster(km_res, geom = "point", data = df) + ggtitle("k = 3")
  })
  
  output$kmeans_plot2 <- renderPlot({
    set.seed(123)
    df <- supermarket_sales[, c("total", "gross_margin_percentage", "rating")]
    km_res <- kmeans(df, 3, nstart = 25)
    fviz_cluster(km_res, geom = "point", data = df) + ggtitle("k = 3")
  })
  
  output$kmeans_plot3 <- renderPlot({
    set.seed(123)
    df <- supermarket_sales[, c("quantity", "unit_price", "total")]
    km_res <- kmeans(df, 3, nstart = 25)
    fviz_cluster(km_res, geom = "point", data = df) + ggtitle("k = 3")
  })
  
  output$kmeans_plot4 <- renderPlot({
    set.seed(123)
    df <- supermarket_sales[, c("unit_price", "tax_5_percent", "cogs", "gross_income")]
    km_res <- kmeans(df, 3, nstart = 25)
    fviz_cluster(km_res, geom = "point", data = df) + ggtitle("k = 3")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
