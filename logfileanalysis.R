log_files <- list.files(path = "/cloud/project/Log File Analysis/", pattern = "apache_wordpress.*\\.log.*", full.names = TRUE)

library(stringr)
library(dplyr)

all_logs_df <- data.frame()

for (file_name in log_files) {
  log_lines <- readLines(file_name)
  
  log_pattern <- "^([^ ]+) ([^ ]+) ([^ ]+) \\[([^]]+)\\] \"([A-Z]+) ([^\"]+) ([^\"]+)\" ([^ ]+) ([^ ]+)(?: \"([^\"]+)\")?(?: \"([^\"]+)\")?"
  parsed_logs <- str_match(log_lines, log_pattern)
  
  log_df <- data.frame(
    IPAddress = parsed_logs[, 2],
    TimeStamp = parsed_logs[, 5],
    RequestMethod = parsed_logs[, 6],
    URL = parsed_logs[, 7],
    Protocol = parsed_logs[, 8],
    Status = as.integer(parsed_logs[, 9]),
    Size = as.integer(parsed_logs[, 10]),
    Referer = parsed_logs[, 11],
    UserAgent = parsed_logs[, 12],
    stringsAsFactors = FALSE
  )
  
  log_df <- log_df %>%
    mutate(Crawler = case_when(
      grepl("Googlebot/2.1", UserAgent) ~ "Googlebot",
      grepl("Googlebot-Mobile/2.1", UserAgent) ~ "Googlebot Mobile",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(Crawler))
  
  all_logs_df <- rbind(all_logs_df, log_df)
}

install.packages("readr")
library(readr)

write.csv(all_logs_df, "CombinedLogFilesData.csv", row.names = FALSE)

#Code for Line Graph to show Search Engine Crawlers' Activity
install.packages("ggplot2")
library(ggplot2)

all_logs_df$TimeStamp <- as.POSIXct(all_logs_df$TimeStamp, format="%d/%b/%Y:%H:%M:%S", tz="UTC")
all_logs_df$Date <- as.Date(all_logs_df$TimeStamp)

daily_counts <- all_logs_df %>% 
  group_by(TimeStamp, Crawler) %>%
  summarise(Count = n(), .groups = 'drop')

ggplot(daily_counts, aes(x = TimeStamp, y = Count, color = Crawler, group = Crawler)) +
  geom_line() +
  labs(title = "Search Engine Crawlers' Activity",
       x = "Date",
       y = "Count")

#Code for Line Graph to show Search Engine Crawlers' Activity - ENDS

#Code for Bar Graph to Show Status Codes

all_logs_df %>%
  mutate(Status = factor(Status)) %>% 
  ggplot(aes(x = Status, fill = Status)) +
  geom_bar() +
  labs(title = "Counts of Status Codes", x = "Status Code", y = "Count") +
  scale_x_discrete(drop = FALSE)

#Code for Bar Graph to Show Status Codes - ENDS

#Code for Bar Graph to Show the File Types crawled

all_logs_df <- all_logs_df %>%
  mutate(File_Type = case_when(
    grepl("\\.html?$|\\.htm$|/$|\\?.*=", URL, ignore.case = TRUE) ~ "HTML/Web Page",
    grepl("\\.js$", URL, ignore.case = TRUE) ~ "JavaScript File",
    grepl("\\.css$", URL, ignore.case = TRUE) ~ "CSS File",
    grepl("\\.(jpeg|jpg|gif|png|bmp|svg)$", URL, ignore.case = TRUE) ~ "Image",
    TRUE ~ "Other File Type"  # Default case
  ))

all_logs_df %>%
  mutate(File_Type = factor(File_Type)) %>% 
  ggplot(aes(x = File_Type, fill = File_Type)) +
  geom_bar() +
  labs(title = "Counts of File Types", x = "File Type", y = "Count") +
  scale_x_discrete(drop = FALSE)

#Code for Bar Graph to Show the File Types crawled - ENDS

#Code for Table Chart to Show the Last Crawled Date and Days since Last Crawled for each URL

install.packages("lubridate")
library(lubridate)

last_crawled_data <- all_logs_df %>%
  group_by(URL) %>%                    # Group data by URL
  summarise(LastCrawledDate = max(TimeStamp)) %>%  # Get the latest timestamp for each group
  ungroup() %>%
  mutate(DaysSinceLastCrawl = as.integer(difftime(Sys.Date(), as.Date(LastCrawledDate), units = "days")))

install.packages("gt")
library(gt)

table_chart <- gt(last_crawled_data)
print(table_chart)

#Code for Table Chart to Show the Last Crawled timestamp for each URL - ENDS

#Code for No. of Hits on a Particular URL

no_of_hits_data <- all_logs_df %>%
  group_by(URL) %>%   
  summarise(NoOfHits = n()) %>%
  ungroup()        %>%                    
  arrange(desc(NoOfHits))            

table_chart_2 <- gt(no_of_hits_data)
print(table_chart_2)

#Code for No. of Hits on a Particular URL - ENDS

#Code for List of WebPages showing Status Codes other than 200

Not_Found_status_data <- all_logs_df %>%
  filter(Status != 200) %>%
  select(URL, Status)

table_chart_3 <- gt(Not_Found_status_data)
print(table_chart_3)

#Code for List of WebPages showing Status Codes other than 200 - ENDS

#Code for No. of bytes used for different file types

Bytes_By_File_Type <- all_logs_df %>%
  group_by(File_Type) %>%
  summarise(Total_Bytes = sum(Size, na.rm = TRUE), .groups = 'drop')

table_chart_5 <- gt(Bytes_By_File_Type)
print(table_chart_5)

#Code for No. of bytes used for different file types - ENDS

#Code for Checking Referer URLs

referers_data <- all_logs_df %>%
  filter(!is.na(Referer) & Referer != "-") %>%
  group_by(Referer) %>%   
  summarise(NoOfRefers = n()) %>%
  ungroup()        %>%                    
  arrange(desc(NoOfRefers))            

table_chart_6 <- gt(referers_data)
print(table_chart_6)

#Code for Checking Referer URLs - ENDS
