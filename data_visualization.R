
filter <- function(df) {
  value_counts <- table(df$Branch)
  
  values_to_keep <- names(value_counts[value_counts >= 500])
  
  filtered_df <- df[df$Branch %in% values_to_keep, ]
  
  setwd("C:/Users/sarth/OneDrive/Documents/SY-SEM2/DS")
  return(filtered_df)
}

tran_branch <- function(df) {
  library(ggplot2)
  value_counts <- as.data.frame(table(df$Branch))
  value_counts <- value_counts[value_counts$Freq>500,]
  colnames(value_counts)[1] <- "Branch"
  
  color_palette <- rainbow(length(value_counts$Branch))
  
  color_index <- setNames(color_palette, value_counts$Branch)
  
  bar_plot <- ggplot(value_counts, aes(x=Branch, y=Freq, fill=Branch)) +
    geom_bar(stat="identity") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white"),  # Set plot background color
          plot.margin = margin(20, 20, 20, 20),  # Adjust plot margins
          panel.background = element_rect(fill = "white"),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "right") +
    scale_fill_manual(values = color_index) +
    guides(fill=guide_legend(title="Branch"))
  
  bar_plot <- bar_plot + labs(title = "Frequency of Transactions Branch Wise")
  return(bar_plot)
}

tran_branch_pie <- function(df) {
  library(ggplot2)
  value_counts <- as.data.frame(table(df$Branch))
  value_counts <- value_counts[value_counts$Freq>500,]
  colnames(value_counts)[1] <- "Branch"
  
  color_palette <- rainbow(length(value_counts$Branch))
  
  color_index <- setNames(color_palette, value_counts$Branch)
  
  pie_plot <- ggplot(value_counts, aes(x = "", y = Freq, fill = Branch)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    scale_fill_manual(values = color_index) +
    guides(fill = guide_legend(title = "Branch")) +
    labs(title = "Frequency of Transactions Branch Wise")
  
  pie_plot <- pie_plot + labs(title = "Frequency of Transactions Branch Wise")

  return(pie_plot)
}


tran_type <- function(df) {
  tran_count <- as.data.frame(table(df$TranTypeDesc))
  colnames(tran_count)[1] <- "TransactionType"
  
  color_palette <- rainbow(length(tran_count$TransactionType))
  
  color_index <- setNames(color_palette, tran_count$TransactionType)
  
  bar_plot <- ggplot(tran_count, aes(x=TransactionType, y=Freq, fill=TransactionType)) +
    geom_bar(stat="identity") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white"),  # Set plot background color
          plot.margin = margin(20, 20, 20, 20),  # Adjust plot margins
          panel.background = element_rect(fill = "white"),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "right") +
    scale_fill_manual(values = color_index) +
    guides(fill=guide_legend(title="TransactionType"))
  
  bar_plot <- bar_plot + labs(title = "Frequency of Transactions Type Wise")

  return(bar_plot)
}

tran_type_pie <- function(df){
  tran_count <- as.data.frame(table(df$TranTypeDesc))
  colnames(tran_count)[1] <- "TransactionType"
  
  color_palette <- rainbow(length(tran_count$TransactionType))
  
  color_index <- setNames(color_palette, tran_count$TransactionType)
  
  pie_plot <- ggplot(tran_count, aes(x = "", y = Freq, fill = TransactionType)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    scale_fill_manual(values = color_index) +
    guides(fill = guide_legend(title = "Transaction Types")) +
    labs(title = "Frequency of Transactions Type Wise")
  
  pie_plot <- pie_plot + labs(title = "Frequency of Transactions Type Wise")
  
  return(pie_plot)
}


freq_tran <- function(df){
  library(dplyr)
  
  combined_freq <- df %>%
    group_by(Branch, TranTypeDesc) %>%
    summarise(Frequency = n()) %>%
    arrange(Branch, desc(Frequency))
  
  most_frequent <- combined_freq %>%
    group_by(Branch) %>%
    slice_max(Frequency)
  
  most_frequent_plot <- ggplot(most_frequent, aes(x = Branch, y = Frequency, fill = TranTypeDesc)) +
    geom_bar(stat = "identity") +
    labs(title = "Most Frequent Transaction Types by Branch", x = "Branch", y = "Frequency") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(most_frequent_plot)
}

freq_tran_pie <- function(df){
  library(dplyr)
  
  combined_freq <- df %>%
    group_by(Branch, TranTypeDesc) %>%
    summarise(Frequency = n()) %>%
    arrange(Branch, desc(Frequency))
  
  most_frequent <- combined_freq %>%
    group_by(Branch) %>%
    slice_max(Frequency)
  
  most_frequent_plot <- ggplot(most_frequent, aes(x = , y = Frequency, fill = TranTypeDesc)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    scale_fill_manual(values = color_index) +
    guides(fill = guide_legend(title = "Branch")) +
    labs(title = "Frequency of Transactions Type Wise")
  
  return(most_frequent_plot)
}

tran_category <- function(df) {
  library(ggplot2)
  cat_counts <- as.data.frame(table(df$CategoryID))
  cat_counts <- cat_counts[cat_counts$Freq>430,]
  colnames(cat_counts)[1] <- "Category"
  
  color_palette <- rainbow(length(cat_counts$Category))
  
  color_index <- setNames(color_palette, cat_counts$Category)
  
  bar_plot <- ggplot(cat_counts, aes(x=Category, y=Freq, fill=Category)) +
    geom_bar(stat="identity") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white"),  # Set plot background color
          plot.margin = margin(5, 20, 5, 20),  # Adjust plot margins
          panel.background = element_rect(fill = "white"),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "right") +
    scale_fill_manual(values = color_index) +
    guides(fill=guide_legend(title="Category"))
  bar_plot <- bar_plot + labs(title = "Frequency of Transactions category wise", x = "Branch", y = "Frequency")
  

  return(bar_plot)
}

tran_category_pie <- function(df){
  library(ggplot2)
  cat_counts <- as.data.frame(table(df$CategoryID))
  cat_counts <- cat_counts[cat_counts$Freq>430,]
  colnames(cat_counts)[1] <- "Category"
  
  color_palette <- rainbow(length(cat_counts$Category))
  
  color_index <- setNames(color_palette, cat_counts$Category)
  
  pie_plot <- ggplot(cat_counts, aes(x="", y=Freq, fill=Category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    scale_fill_manual(values = color_index) +
    guides(fill = guide_legend(title = "Category")) +
    labs(title = "Frequency of Transactions category wise")
  pie_plot <- pie_plot + labs(title = "Frequency of Transactions category wise", x = "Branch", y = "Frequency")
  
  return(pie_plot)
}

freq_month <- function(df){
  library(dplyr)
  library(ggplot2)
  
  monthly_freq <- df %>%
    mutate(Month = format(as.Date(EntryDateTime, format="%d-%m-%Y"), "%m")) %>%
    group_by(Month) %>%
    summarise(Frequency = n())
  
  monthly_freq$Month <- month.name[as.numeric(monthly_freq$Month)]
  
  color_palette <- rainbow(length(monthly_freq$Month))
  
  color_index <- setNames(color_palette, monthly_freq$Month)
  
  bar_plot <- ggplot(monthly_freq, aes(x=Month, y=Frequency, fill=Month)) +
    geom_bar(stat="identity") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white"),
          plot.margin = margin(20, 20, 20, 20),
          panel.background = element_rect(fill = "white"),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "right") +
    scale_fill_manual(values = color_index) +
    guides(fill=guide_legend(title="Month"))
  bar_plot <- bar_plot + labs(title = "Frequency of Transactions Month Wise")

  return(bar_plot)
}

freq_month_pie <- function(df){
  library(dplyr)
  library(ggplot2)
  
  monthly_freq <- df %>%
    mutate(Month = format(as.Date(EntryDateTime, format="%d-%m-%Y"), "%m")) %>%
    group_by(Month) %>%
    summarise(Frequency = n())
  
  monthly_freq$Month <- month.name[as.numeric(monthly_freq$Month)]
  
  color_palette <- rainbow(length(monthly_freq$Month))
  
  color_index <- setNames(color_palette, monthly_freq$Month)
  
  pie_plot <- ggplot(monthly_freq, aes(x="", y=Frequency, fill=Month)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    scale_fill_manual(values = color_index) +
    guides(fill = guide_legend(title = "Month"))
  
  pie_plot <- pie_plot + labs(title = "Frequency of Transactions Month Wise")
  
  return(pie_plot)
}


zero_fine_branch <- function(df){
  library(dplyr)
  library(ggplot2)
  
  fine_freq <- df %>%
    group_by(FineAmount, Branch) %>%
    summarise(Frequency = n()) %>%
    arrange(Branch, desc(Frequency))

  non_zero_fine <- fine_freq[fine_freq$FineAmount != 0,] %>%
    group_by(Branch) %>%
    summarise(TotalFrequency = sum(Frequency))
  
  color_palette <- rainbow(length(non_zero_fine$Branch))
  
  color_index <- setNames(color_palette, non_zero_fine$Branch)
  
  bar_plot <- ggplot(non_zero_fine, aes(x=Branch, y=TotalFrequency, fill=Branch)) +
    geom_bar(stat="identity") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white"),
          plot.margin = margin(20, 20, 20, 20),
          panel.background = element_rect(fill = "white"),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "right") +
    scale_fill_manual(values = color_index) +
    guides(fill=guide_legend(title="Branch"))
  
  bar_plot <- bar_plot + labs(title = "Frequency of Non Zero fines Branch Wise")

  return(bar_plot)
}

zero_fine_branch_pie <- function(df) {
  library(dplyr)
  library(ggplot2)
  
  fine_freq <- df %>%
    group_by(FineAmount, Branch) %>%
    summarise(Frequency = n()) %>%
    arrange(Branch, desc(Frequency))
  
  non_zero_fine <- fine_freq[fine_freq$FineAmount != 0,] %>%
    group_by(Branch) %>%
    summarise(TotalFrequency = sum(Frequency))
  
  color_palette <- rainbow(length(non_zero_fine$Branch))
  
  color_index <- setNames(color_palette, non_zero_fine$Branch)
  
  pie_plot <- ggplot(non_zero_fine, aes(x="", y=TotalFrequency, fill=Branch)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    scale_fill_manual(values = color_index) +
    guides(fill = guide_legend(title = "Branch"))
  
  pie_plot <- pie_plot + labs(title = "Frequency of Non Zero fines Branch Wise")

  return(pie_plot)
}

avg_tran <- function(df,year){
  library(ggplot2)
  library(dplyr)
  
  new_df <- df[substr(trimws(df$EntryDateTime), 
                               nchar(trimws(df$EntryDateTime)) - 3, nchar(trimws(df$EntryDateTime))) == year, ]

  value_counts <- as.data.frame(table(new_df$Branch))
  
  colnames(value_counts)[1] <- "Branch"
  
  value_counts <- subset(value_counts, !(Branch %in% c("DES", "MECH2", "IND", "INPROD", "ELEX2","MCA","MECHD","PROD","COMP2","LIB")))
  
  value_counts$avg <- sapply(value_counts$Branch, function(branch) length(unique(new_df$LastName[new_df$Branch == branch])))
  
  value_counts$avg <- (value_counts$Freq %/% (value_counts$avg))
  value_counts$avg <- value_counts$avg %/% 2
  
  value_counts <- subset(value_counts,Branch != "LIB")
  
  color_palette <- rainbow(length(value_counts$Branch))
  
  color_index <- setNames(color_palette, value_counts$Branch)
  
  bar_plot <- ggplot(value_counts, aes(x=Branch, y=avg, fill=Branch)) +
    geom_bar(stat="identity") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white"),  # Set plot background color
          plot.margin = margin(20, 20, 20, 20),  # Adjust plot margins
          panel.background = element_rect(fill = "white"),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "right") +
    scale_fill_manual(values = color_index) +
    guides(fill=guide_legend(title="Branch"))
  
  bar_plot <- bar_plot + labs(title = paste("Frequency of Average Transactions Branch Wise for year",year))

  return(bar_plot)
}

avg_tran_pie <- function(df,year){
  library(ggplot2)
  library(dplyr)
  
  new_df <- df[substr(trimws(df$EntryDateTime), 
                      nchar(trimws(df$EntryDateTime)) - 3, nchar(trimws(df$EntryDateTime))) == year, ]
  
  value_counts <- as.data.frame(table(new_df$Branch))
  value_counts <- value_counts[value_counts$Freq>100,]
  colnames(value_counts)[1] <- "Branch"
  
  value_counts$avg <- sapply(value_counts$Branch, function(branch) length(unique(new_df$LastName[new_df$Branch == branch])))
  
  value_counts$avg <- (value_counts$Freq %/% (value_counts$avg))
  value_counts$avg <- value_counts$avg %/% 2
  
  value_counts <- subset(value_counts,Branch != "LIB")
  
  color_palette <- rainbow(length(value_counts$Branch))
  
  color_index <- setNames(color_palette, value_counts$Branch)
  
  pie_plot <- ggplot(value_counts, aes(x="", y=avg, fill=Branch)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    scale_fill_manual(values = color_index) +
    guides(fill = guide_legend(title = "Branch"))
  
  pie_plot <- pie_plot + labs(title = paste("Frequency of Average Transactions Branch Wise for year",year))

  return(pie_plot)
}

avg_branch_line <- function(df){
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  new_df <- df %>%
    mutate(year = substr(EntryDateTime, nchar(EntryDateTime) - 4, nchar(EntryDateTime)))
  
  new_df <- subset(new_df, !(Branch %in% c("DES", "MECH2", "IND", "INPROD", "ELEX2","MCA","MECHD","PROD","COMP2","LIB")))
  
  branch_avg <- new_df %>%
    group_by(Branch, year) %>%
    summarise(avg = n() %/% n_distinct(LastName) %/% 2) %>%  
    pivot_wider(names_from = year, values_from = avg)
  
  branch_avg <- subset(branch_avg, select = -`2024 `)
  
  colnames(branch_avg) <- trimws(colnames(branch_avg))
  
  column_order <- as.character(2015:2023)
  
  branch_avg <- branch_avg %>%
    select(Branch, all_of(column_order))
  
  branch_avg_long <- branch_avg %>%
    pivot_longer(cols = -Branch, names_to = "year", values_to = "avg")
  
  branch_avg_long$year <- as.numeric(branch_avg_long$year)
  
  line_plot <- ggplot(branch_avg_long, aes(x = year, y = avg, color = Branch)) +
    geom_line(size = 1.5) +  # Adjust line thickness
    labs(title = "Yearly Average Transactions per Student by Branch",
         x = "Year",
         y = "Average Transactions per Student") +
    scale_color_manual(values = rainbow(length(unique(branch_avg_long$Branch)))) +  # Set colors for branches
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16),  # Adjust title size
      axis.title.x = element_text(size = 14),  # Adjust x-axis label size
      axis.title.y = element_text(size = 14),  # Adjust y-axis label size
      axis.text = element_text(size = 12),  # Adjust axis tick label size
      legend.title = element_text(size = 14),  # Adjust legend title size
      legend.text = element_text(size = 12),  # Adjust legend text size  # Adjust overall plot margin
      aspect.ratio = 0.5,  # Adjust plot aspect ratio (length * breadth)
      plot.background = element_rect(fill = "white", color = NA)  # Set plot background color
    )
  
  return(line_plot)
}

branch_month <- function(df,branch){
  library(dplyr)
  library(ggplot2)
  
  selected_department <- branch
  
  monthly_selected_dept_freq <- df[df$Branch == selected_department,] %>%
    mutate(Month = format(as.Date(EntryDateTime, format="%d-%m-%Y"), "%m")) %>%
    group_by(Month) %>%
    summarise(Frequency = n())

  monthly_selected_dept_freq$Month <- month.name[as.numeric(monthly_selected_dept_freq$Month)]
  
  color_palette <- rainbow(length(monthly_selected_dept_freq$Month))
  
  color_index <- setNames(color_palette, monthly_selected_dept_freq$Month)
  
  title_text <- paste("Frequency of Transactions Month Wise for", selected_department, "Deparment")
  
  bar_plot <- ggplot(monthly_selected_dept_freq, aes(x=Month, y=Frequency, fill=Month)) +
    geom_bar(stat="identity") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white"),
          plot.margin = margin(5, 40, 5, 40),
          panel.background = element_rect(fill = "white"),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "right") +
    scale_fill_manual(values = color_index) +
    guides(fill=guide_legend(title="Month"))
  bar_plot <- bar_plot + labs(title = title_text)

  return(bar_plot)
}

branch_month_pie <- function(df,branch){
  library(dplyr)
  library(ggplot2)
  
  selected_department <- branch
  
  monthly_selected_dept_freq <- df[df$Branch == selected_department,] %>%
    mutate(Month = format(as.Date(EntryDateTime, format="%d-%m-%Y"), "%m")) %>%
    group_by(Month) %>%
    summarise(Frequency = n())
  
  monthly_selected_dept_freq$Month <- month.name[as.numeric(monthly_selected_dept_freq$Month)]
  
  color_palette <- rainbow(length(monthly_selected_dept_freq$Month))
  
  color_index <- setNames(color_palette, monthly_selected_dept_freq$Month)
  
  title_text <- paste("Frequency of Transactions Month Wise for", selected_department, "Deparment")
  
  pie_plot <- ggplot(monthly_selected_dept_freq, aes(x="", y=Frequency, fill=Month)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    scale_fill_manual(values = color_index) +
    guides(fill = guide_legend(title = "Month"))
  
  pie_plot <- pie_plot + labs(title = title_text)

  return(pie_plot)
}


branch_book <- function(df,branch,head) {
  library(dplyr)
  library(ggplot2)
  
  selected_department <- branch
  
  book_dept_freq <- df[df$Branch == selected_department,] %>%
    group_by(CardTitle) %>%
    summarise(Frequency = n()) %>%
    arrange(desc(Frequency)) %>%
    head(head)
  
  color_palette <- rainbow(length(book_dept_freq$CardTitle))
  
  color_index <- setNames(color_palette, book_dept_freq$CardTitle)
  
  bar_plot <- ggplot(book_dept_freq, aes(x=CardTitle, y=Frequency, fill=CardTitle)) +
    geom_bar(stat="identity") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white"),
          plot.margin = margin(5, 40, 5, 40),
          panel.background = element_rect(fill = "white"),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "right") +
    scale_fill_manual(values = color_index) +
    guides(fill=guide_legend(title="Books"))
  bar_plot <- bar_plot + labs(title = paste("Top ", head ," books for ", branch ," department "))

  return(bar_plot)
}

branch_book_pie <- function(df,branch,head){
  library(dplyr)
  library(ggplot2)
  
  selected_department <- branch
  
  book_dept_freq <- df[df$Branch == selected_department,] %>%
    group_by(CardTitle) %>%
    summarise(Frequency = n()) %>%
    arrange(desc(Frequency)) %>%
    head(head)
  
  color_palette <- rainbow(length(book_dept_freq$CardTitle))
  
  color_index <- setNames(color_palette, book_dept_freq$CardTitle)
  
  pie_plot <- ggplot(book_dept_freq, aes(x="", y=Frequency, fill=CardTitle)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    scale_fill_manual(values = color_index) +
    guides(fill = guide_legend(title = "Books"))
  
  pie_plot <- pie_plot + labs(title = paste("Top ", head ," books for ", branch ," department "))

  return(pie_plot)
}





