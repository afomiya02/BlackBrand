# make sure to set working directory in either ui.R or server.R
# if not this will not load!
source("code/sodem.R") # load color palettes

#Graphs for Headquarter Locations, Ratio of Sentiment and Diversity Vs Pos and Neg
# Function to load data from CSV file
load_data <- function() {
    article_dat <- read.csv("data/media/articledata.csv")
    # Fixing year format for one specific value
    article_dat$year[167] <- 2022
    # Removing empty rows
    article_dat <- article_dat[-(104:110),]
    # Converting columns to factors
    article_dat$hqlocation <- as.factor(article_dat$hqlocation)
    article_dat$source <- as.factor(article_dat$source)
    article_dat
}

# Function to summarize data by year
summarize_data <- function(data) {
    pos_sum <- aggregate(pos_len ~ year, data = data, FUN = sum)
    neg_sum <- aggregate(neg_len ~ year, data = data, FUN = sum)
    div_sum <- aggregate(div_len ~ year, data = data, FUN = sum)
    tot_sum <- aggregate(text_len ~ year, data = data, FUN = sum)
    
    sub_dat <- merge(pos_sum, neg_sum, all=TRUE, no.dups = TRUE)
    sub_dat <- merge(sub_dat, div_sum, all=TRUE, no.dups = TRUE)
    sub_dat <- merge(sub_dat, tot_sum, all=TRUE, no.dups = TRUE)
    
    sub_dat$pos_ratio <- sub_dat$pos_len/sub_dat$text_len
    sub_dat$neg_ratio <- sub_dat$neg_len/sub_dat$text_len
    sub_dat$div_ratio <- sub_dat$div_len/sub_dat$text_len
    sub_dat$year <- as.character(sub_dat$year)
    sub_dat
}

# Function to calculate ratios for each article
calculate_ratios <- function(data) {
    data$pos_ratio <- data$pos_len/data$text_len
    data$neg_ratio <- data$neg_len/data$text_len
    data$div_ratio <- data$div_len/data$text_len
    data
}

# Function to create the headquarters plot
create_headquarters_plot <- function(data) {
    ggplot(data, aes(x = hqlocation)) +
        geom_bar(fill = "firebrick4") + 
        xlab("City") +
        ylab("Number of Articles") +
        theme_bw() +
        ggtitle("Headquarters Locations of Media Companies") +
        theme(axis.text.x = element_text(color = "grey", size = 12, angle = 30),
              axis.text.y = element_text(color = "grey", size = 12), 
              axis.title = element_text(size = 17),
              plot.title = element_text(size = 19))
}

# Function to create the sentiment by year plot
create_sentiment_by_year_plot <- function(data, year) {
    sentiment_sub_dat <- subset(data, year == year)
    dat_for_bar_graph <- data.frame(Ratios = c(sentiment_sub_dat$pos_ratio[1], 
                                               sentiment_sub_dat$neg_ratio[1], 
                                               sentiment_sub_dat$div_ratio[1]),
                                    Sentiments = c("Positive", "Negative", "Diversity"))
    ggplot(data = dat_for_bar_graph, aes(x = Sentiments, y = Ratios)) + 
        theme_bw() +
        geom_bar(stat = "identity", fill = "firebrick4") +
        ylab("Proportion to Total Words")
}

# Function to create the diversity by positive and negative sentiment plot
create_div_by_pos_and_neg_plot <- function(data) {
    ratios <- melt(data[, c("pos_ratio", "neg_ratio", "div_ratio")], id.vars = "div_ratio")
    ggplot(ratios, aes(x = div_ratio, y = value, color = variable)) +  
        geom_point() + 
        theme_bw() +
        scale_colour_manual(values = c("black", "red")) +
        ylab("Positive and Negative Ratios") + 
        xlab("Diversity Word Ratio") + 
        ggtitle("Positive and Negative Sentiment Ratios versus Diversity Ratio") +
        theme(axis.text.x = element_text(color = "black", size = 10),
              axis.text.y = element_text(color = "black", size = 10), 
              axis.title = element_text(size = 15),
              plot.title = element_text(size = 18))
}

# headquarter_sentiment_deversity <- function(input,output,session){
#     # Output for headquarters plot
#     output$headquarters_graph <- renderPlot({
#         headquarters_data <- load_data()
#         create_headquarters_plot(headquarters_data)
#     })
#     
#     # Reactive expression for selecting sentiment year
#     sentiment_year <- reactive({
#         input$select_sent_year
#     })
#     
#     # Output for sentiment by year plot
#     # TODO investigate why this slider won't change output
#     output$sentiment_by_year <- renderPlotly({
#         sentiment_data <- calculate_ratios(load_data())
#         create_sentiment_by_year_plot(sentiment_data, sentiment_year())
#     })
#     
#     # Output for diversity by positive and negative sentiment plot
#     output$div_by_pos_and_neg <- renderPlot({
#         sentiment_data <- calculate_ratios(load_data())
#         create_div_by_pos_and_neg_plot(sentiment_data)
#     })
#     
#     # Link to Word Bags
#     url <- a("Link to Word Bags", href="https://tinyurl.com/4ym9njb7")
#     output$tab <- renderUI({
#         tagList("URL link:", url)
#     })
# }
