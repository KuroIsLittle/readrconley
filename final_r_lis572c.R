### Final Project — Data/Computational Analysis
## LIS 572 C Introduction to Data Science
# Roland Conley

# Data consists of item transactions from Tayo Na Library's TinyCat catalog

tnl_df <- read.csv("https://raw.githubusercontent.com/Tayo-Na-Library/Tayo-Na-Library/main/final_LIS572C.csv", stringsAsFactors = FALSE)

# Load all necessary libraries
library(dygraphs)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(lubridate)
library(reshape2)
library(stringr)
library(tidytext)
library(tidyverse)

# Goal 1: Compare checkouts per language to titles available per language
## Purpose: To see if there's any large difference in what titles are available from Tayo Na Library versus titles that are checked out, i.e., are items in certain languages checked out more than items in those languages are available?

# Step 1: Filter data frame for checkouts and plot that data

# Filter data frame to only contain checkouts
checkouts_df <- tnl_df %>%
  filter(TransactionType == "Checked out")

# Convert EntryDate column from character type to date type
checkouts_df$EntryDate <- as.Date(checkouts_df$EntryDate)

# Check to make sure data type of each column
str(checkouts_df)

# Store language count per checkout 
count_cpl <- checkouts_df %>% 
  count(Language)

# Print language count per checkout to compare non visually
print(count_cpl)

# Plot checkouts per language in a bar plot
cpl_bplot <- ggplot(data = count_cpl)+
  geom_col(aes(
    x = reorder(Language, -n),
    y = n,
    fill = Language)
    )+
  scale_color_brewer(palette = "Set3")+
  labs(
    title = "Tayo Na Library: Checkouts Per Language",
    x = "Item Language",
    y = "Total Checkouts",
    color = "Language"
  )

# Edit bar plot for improved readability
cpl_bplot + theme(
  plot.title = element_text(color="black", size=14, face="bold.italic"),
  axis.title.x = element_text(color="blue", size=12, face="bold"),
  axis.title.y = element_text(color="red", size=12, face="bold"),
  axis.text.x = element_text(color="black", size=8, face="bold", angle = 10),
  legend.position = "bottom",
  legend.title = element_text(face = "bold"))

# Step 2: Create separate data frame that groups distinct titles by language
## This will show us how many individual titles we have per language

# Group original data frame by language and summarize to get object composed of titles available per language
titles_per_language <- tnl_df %>% 
  group_by(Language) %>% 
  summarize(titles_per_language = n_distinct(Title))

# Print count of titles per language to compare non visually
print(titles_per_language)

# Now create a bar plot for that object, the same as previously done for checkouts per language
tpl_bplot <- ggplot(data = titles_per_language)+
  geom_col(aes(
    x = reorder(Language, -titles_per_language),
    y = titles_per_language,
    fill = Language)
  )+
  scale_color_brewer(palette = "Set3")+
  labs(
    title = "Tayo Na Library: Distinct Titles Per Language",
    x = "Item Language",
    y = "Distinct Titles",
    color = "Language"
  )

# Edit bar plot for improved readability
tpl_bplot + theme(
  plot.title = element_text(color="black", size=14, face="bold.italic"),
  axis.title.x = element_text(color="blue", size=14, face="bold"),
  axis.title.y = element_text(color="red", size=14, face="bold"),
  axis.text.x = element_text(color="black", size=8, face="bold", angle=10),
  legend.position = "bottom",
  legend.title = element_text(face = "bold")
)

# We can now compare both bar plots
## Result: Plots are very similar. However, whereas checkouts per language is ranked number 1 category is Primary Tagalog, Secondary English, number 2 is Tagalog, number 3 is Primary English, Secondary Tagalog, and number 4 is English, for Distinct Titles Per Language number 1 is Primary Tagalog, Secondary English, number 2 is Primary English, Secondary Tagalog, number 3 is Tagalog, and number 4 is English. Essentially, ranks number 2 and number 3 are different. 
### There are more items available in Primary English, Secondary Tagalog than are being checked out in Primary English, Secondary Tagalog. This is no true surprise since these materials are easier to acquire. 
#### What might be interesting, however, is that more items are checked out in Tagalog than items are available in Tagalog. Furthermore, there are drastically more items available in English than those being checked out in English. We can also see that items in Primary Hiligaynon, Secondary English are being checked out more frequently than there are items available in this language category.

# Goal 2: Plot checkouts over time
## By looking at checkouts over time, we can see if there is a spike in checkouts during our previous in-person events. We expect there to be. We can also look for any other trends that we don't expect. 

# Group checkouts data frame by EntryDate and summarize 
checkouts_over_time <- checkouts_df %>% 
  group_by(EntryDate) %>% 
  summarize(total_checkouts = sum (NumberOfTransactions))

# Plot checkouts over time object on a connected scatterplot to visually check for trends
## We are not printing this data because there would be too many rows
checkouts_over_time %>%
  tail(10) %>%
  ggplot( aes(x=EntryDate,
              y=total_checkouts,
              group=1)) +
  geom_line(color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
  theme_ipsum() +
  ggtitle("Tayo Na Library: Checkouts Over Time") +
  labs(
    x = "Checkout Date",
    y = "Total Checkouts"
  )

# If we want to compare to events, we can create a data frame for this from multiple vectors 

# First, make a vector for the event dates
event_dates <- c("2024-01-20", "2024-02-18", "2024-03-16", "2024-04-06", "2024-05-04", "2024-05-05")

# Next, make a vector for the event names
event_names <- c("Children's Book Fair", "Feb-Ibig", "Van Nuys", "Encino", "FAB Long Beach", "Sama Ka Na") 

# Combine the two vectors into a single data frame
events_df <- data.frame(event_dates, event_names)

# Check variable type for the data frame
str(events_df)

# Convert event_dates column from character type to date type
## Technically, this doesn't really matter. But if we were to figure out how to plot this on the connected scatterplot, this would matter.
events_df$event_dates <- as.Date(events_df$event_dates)

# Print data frame compare to checkouts over time connected scatter plot
print(events_df)

# Result: There are major increases in checkouts during event dates based on the checkouts over time connected scatter plot.

# Goal 3: Compare checkouts by Audience
## Another thing we would like to check for is to see whether there are any trends in item's audience type of checkouts
### We expect there to be more checkouts in items whose audience is youth, i.e., not adult
#### We especially expect majority of checkouts to target an audience of 0+ or under age 3

# Group checkouts data frame by Audience and summarize to compare audience type of checkouts
checkouts_per_audience <- checkouts_df %>% 
  group_by(Audience) %>% 
  summarize(checkouts_per_audience = n())

# You can use print to get a non visual method of comparison, but there are too many rows to properly compare what audiences are checked out the most
print(checkouts_per_audience)

# Slice dataframe for only the top 10 audiences with the most checkouts
top_10_audiences <- checkouts_per_audience %>% 
  slice_max(n=10, order = checkouts_per_audience)

# We can print out the top ten audiences to get a non visual method of comparison
print(top_10_audiences)

# Now we can see what the top 10 audiences are for checkouts
## This allows us to answer our question regarding if there are any trends in item's audience type regarding checkouts
### Result: Number 1 audience type is 0+. Number 2 audience type is 8-12 years. Number 3 audience type is Adult. 
#### However, keep in mind that for adult there is a separate category entitled Adult, Mature Audience. So, if we added that to the rest of adult, that would bump the total number from 23 to 38. This would make adult the number 2 category and 8-12 would become the number 3 category.

# Goal 4: Compare checkouts by MaterialType
## This won't show us near as much as we might like because many items are simply labeled Paperback. However, we can use this method to see if comics are being checked out well or not, as well as board books. 
### The reason we are checking for this is that we expected board books and comics to be checked out in large numbers. 

# Group checkouts data frame by MaterialType and summarize to compare material type of checkouts
checkouts_per_material_type <- checkouts_df %>% 
  group_by(MaterialType) %>% 
  summarize(checkouts_per_material_type = n())

# We can print out the results to non visually compare the data
print(checkouts_per_material_type)

# Result: Paperbacks is, of course, the number 1 category. Hardly surprising since this encompasses both adult audiences and youth. Number 2 category is board books. Number 3 category is comic books. Hardcovers comes in last place.
## Looking at this data, comic books were not nearly checked out as we expected. Board books were checked out a lot, much better than comic books, but again, not near as much as expected if compared to the total number of paperbacks. However, again, we have to keep in mind that paperbacks is not divided in any way by audience type.