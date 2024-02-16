library(dplyr)
library(stringr)
library(readr)

rm(list = ls())
gc()

scientist_pub <- read.csv("/Users/wangzifang/Desktop/应用经济学中的大数据分析/assignment/data/assignment_idaccuracy/scientist_pub.csv")

treat_file <- function(filename){
  aminer_id <- read.csv(filename)
  
  #从每个aminer文件的文件名中提取出科学家的unique-ID（形式为0_xxx）
  unique_ID <- str_extract(filename,"0_[0-9]+")
  #从scientist_pub文件中筛选出对应科学家的论文
  scientist_id <- filter(scientist_pub, unique_ID == uniqueID)
  
  #英文字符型变量首字母大写，便于匹配
  scientist_id$doi_u <- toupper(scientist_id$doi)
  scientist_id$title_u <- toupper(scientist_id$title)
  scientist_id$journal_u <- toupper(scientist_id$journal)
  aminer_id$doi_u <- toupper(aminer_id$doi)
  aminer_id$标题_u <- toupper(aminer_id$标题)
  aminer_id$期刊_u <- toupper(aminer_id$期刊)
  
  #内链接，保留同一科学家同时存在于aminer和scientist_pub中的论文条目
  match_paper <- inner_join(aminer_id, scientist_id, by = c("doi_u" = "doi_u", "标题_u" = "title_u","期刊_u" = "journal_u", "年份" = "pub_year"))
  
  #计算精确率和查全率
  accuracy_rate <- nrow(match_paper) / nrow(aminer_id)
  recall_rate <- nrow(match_paper) / nrow(scientist_id)
  
  return(data.frame(unique_ID = unique_ID, accuracy_rate = accuracy_rate, recall_rate = recall_rate))
}

#读取每个科学家的aminer文件，应用treat_file函数
filename_list <- list.files(path = "/Users/wangzifang/Desktop/应用经济学中的大数据分析/assignment/data/assignment_idaccuracy/Aminer/", pattern = "\\.csv$", full.names = TRUE)
results <- lapply(filename_list, treat_file)
all_results <- bind_rows(results) #rbind和bind_rows的区别?为什么rbind输出结果仍为一个包含100元素的列表，并且csv文件将所有数据整理在一行中？
write.csv(all_results, "/Users/wangzifang/Desktop/应用经济学中的大数据分析/assignment/Assignment4_idaccuracy2/all_results.csv")

#计算平均精确率和查全率
average_accuracy <- mean(all_results$accuracy_rate)
average_recall <- mean(all_results$recall_rate)

print(paste("Average Accuracy Rate: ", average_accuracy))
print(paste("Average Recall Rate: ", average_recall))
