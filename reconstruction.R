library(ggplot2)
library(dplyr)
library(readxl)

df_1 <- read.csv("companiesmarketcap.com - Largest pharma companies by market cap.csv")

# data preparation

df_1 <- df_1 %>% arrange(desc(marketcap))
df_1 <- head(df_1,50)

# converting marketcap in Billion
df_1$marketcap <- round(df_1$marketcap/1000000000,2)


df_1$country <- factor(df_1$country)
levels(df_1$country)


# assign regions
df_1$region[df_1$country=="Australia"] <- "Oceania"
df_1$region[df_1$country=="China"] <- "Asia"
df_1$region[df_1$country=="Denmark"] <- "Europe"
df_1$region[df_1$country=="France"] <- "Europe"
df_1$region[df_1$country=="Germany"] <- "Europe"
df_1$region[df_1$country=="India"] <- "Asia"
df_1$region[df_1$country=="Ireland"] <- "Europe"
df_1$region[df_1$country=="Japan"] <- "Asia"
df_1$region[df_1$country=="Netherlands"] <- "Europe"
df_1$region[df_1$country=="South Korea"] <- "Asia"
df_1$region[df_1$country=="Switzerland"] <- "Europe"
df_1$region[df_1$country=="United Kingdom"] <- "Europe"
df_1$region[df_1$country=="United States"] <- "North America"

df_1$region <- factor(df_1$region)
levels(df_1$region)


# creating region-wise ranking
df_1$asia = ifelse(df_1$region == "Asia",df_1$Rank,NA)
df_1$asia_rank <- rank(df_1$asia, ties.method = "min", na.last = "keep")

df_1$america = ifelse(df_1$region == "North America",df_1$Rank,NA)
df_1$america_rank <- rank(df_1$america, ties.method = "min", na.last = "keep")

df_1$europe = ifelse(df_1$region == "Europe",df_1$Rank,NA)
df_1$europe_rank <- rank(df_1$europe, ties.method = "min", na.last = "keep")

df_1$oceania = ifelse(df_1$region == "Oceania",df_1$Rank,NA)
df_1$oceania_rank <- rank(df_1$oceania, ties.method = "min", na.last = "keep")

df_1$region_rank = rowSums(df_1[,c("asia_rank", "america_rank", "europe_rank", "oceania_rank")], na.rm=TRUE)

df_1 <- df_1[,-c(8:15)]

# colour definations
color_pallate <- c("#466080", #asia
                   "#29265b", #europe
                   "#89141c", #north america
                   "#b16264") #oceiana

p1 <- ggplot(data = df_1, aes(x = reorder(Name, marketcap), y = marketcap, fill = region)) +
  
  # gg bar plot
  geom_bar(stat="identity") + 
  
  # adding world's rank to each company
  geom_text(aes(label = `Rank`, y = 0), 
            hjust = "top",
            fontface = "bold") +
  
  # adding company's market-capitalisation
  geom_text(aes(label = paste(marketcap)),
            nudge_y = 10, nudge_x = 0,
            fontface = "bold") +
  
  # adding region-wise rank to each company 
  geom_text(aes(label = paste(region_rank)),
            colour = "white",
            nudge_y = -10, nudge_x = 0,
            fontface = "bold") +
  
  # adding title, subtitle, caption, axis
  labs(title = "Top 50 Largest Pharmaceutical Companies in the world by Market Capitalization",
       subtitle  = "North America and Europe dominates the top hierarchy as they accounts 19 of the top 20 Pharmaceutical Companies",
       y = "Market Capitalization in Billion USD",
       caption = "The original source provides a list of healthcare companies that work closely with pharmaceuticals, including bioteck, pharmaceutical retailers, clinical laboratories, etc
       Data Source: https://companiesmarketcap.com/pharmaceuticals/largest-pharmaceutical-companies-by-market-cap/
       Visualization sources: Deshmukh,A.(2021, September 17). Visualizing the World's Biggest Pharmaceutical Companies.Retrieved from Visual Capitalist:https://www.visualcapitalist.com/worlds-biggest-pharmaceutical-companies/") + 
  
  # adding note for region-wise ranking
  annotate("text", x = 35, y = 300 , size = 5,
           label = "The White coloured digit on each bar denotes Company's rank in 
           respective region",
           fontface = "bold.italic") +
  
  # assigning colours to regions
  scale_fill_manual(values = color_pallate) +
  
  # describing theme
  theme_grey() +
  
  theme(plot.title = element_text(face = "bold",
                                  size = 20,
                                  hjust = 0.5),
        plot.subtitle = element_text(face = "bold", 
                                     size = 15),
        plot.caption = element_text(size = 15,
                                    hjust = 0),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 15,
                                   face = "bold"),
        axis.text.y = element_text(size = 10,
                                   face = "plain"),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(1,'cm'),
        legend.text = element_text(size = 15)) +
  
  # flipping the co-ordinates
  coord_flip()

# Showing Reconstructed Visualization
p1