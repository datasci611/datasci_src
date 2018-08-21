library(dplyr); library(magrittr)
setwd("/Users/hycho/Documents/1STAT_Courses/611 Data Science (TA)")

### Master table
master = data.frame(
  chapter = c(3, 5, 7, 10, 11, 13, 14, 15, 19, 20, 21, 28, 23, 24),
  link = paste0("https://raw.githubusercontent.com/hadley/r4ds/master/",
                c("visualize", "transform", "EDA", "tibble", "import", "relational-data",
                  "strings", "factors", "functions", "vectors", "iteration", 
                  "communicate-plots", "model-basics", "model-building"),
                ".Rmd"),
  stringsAsFactors = FALSE)

### For each chapter
for (m in 1:dim(master)[1]) {
  chapter = master[m, 1]
  rmd = readLines(master[m, 2])
  
  ### identifying problem numbers and chuncks
    # Finding all pounds #
    grep("^\\#", rmd) -> pound.all
    c(pound.all, length(rmd)+1) -> pound.all
    
    grep("^.*\\# Exercise", rmd) -> pound.exercise
    
    # Title and headline
    rmd[pound.all[1]] %>% gsub("\\#","", .) %>% gsub("\\{.*\\}", "", .) %>% trimws -> title
    
    headline = c(
      "---",
      paste0("  title: \"BIOS 611 HW", m, " ", title, " (Chapter ", chapter, ")\""),
      "  author: \"(Your full name here)\"",
      "  date: \"`r format(Sys.time(), '%m/%d/%Y')`\"",
      "  output: html_document",
      "---",
      "",  
      "  This set of exercise is mostly taken from R for Data Science by Garrett Grolemund and Hadley Wickham.",
      "")
    
    exercises = list(begin = pound.exercise,
                     end = pound.all[which (pound.all %in% pound.exercise) + 1] - 1,
                     numbers = vector("list", length(pound.exercise)))
  
    # outer loop: i = exercise
    for (i in 1:length(pound.exercise)) {
      k1 = exercises$begin[i]
      k2 = exercises$end[i]
      tmp = data_frame(begin = grep("^[[:space:]]*[0-9]*\\.[[:space:]]", rmd[k1:k2]) + k1 - 1,
                       end = c(begin[-1] - 1, k2),
                       chunk = FALSE)
      tmp$no = cumsum(tmp$begin %in% (grep("^[0-9]*\\.[[:space:]]", rmd[k1:k2]) + k1 - 1))
      
      # inner loop: j = problem
      for (j in 1:length(tmp$begin)) {
        k5 = tmp$begin[j]
        k6 = tmp$end[j]
        if (length(grep(".* \\`\\`\\`", rmd[k5:k6]))) {
          tmp$chunk[j] = TRUE
        }
      }
      tmp = rbind(data_frame(begin = k1, end = tmp$begin[1]-1, chunk=FALSE, no = 0),
                  tmp)
      exercises$numbers[[i]] = tmp
    }
  
  ### stack up
    rmd.new = headline
    exercise.long = do.call(rbind, exercises$numbers)
    chunk.null = c("    Answer: ", "", "    ```{r}", "", "    ```", "")
    exno = 1
    
    for (j in 1:dim(exercise.long)[1]) {
      k5 = exercise.long$begin[j]
      k6 = exercise.long$end[j]
      
      if (!exercise.long$no[j]) {
        # replace exercise numbers
        rmd.tmp = rmd[k5:k6] %>% gsub("^\\#* Exercise(s| )", paste0("# Exercise ", exno), .)
        exno = exno + 1
      } else {
        # replace problem numbers
        rmd.tmp = rmd[k5:k6] %>% gsub("^[0-9]*\\.[[:space:]]", paste0(exercise.long$no[j], ". "), .)  
      }
      
      # update rmd.new
      rmd.new = c(rmd.new, rmd.tmp)
      
      # if r chunk does not exist, add it
      #if (!exercise.long$chunk[j] & exercise.long$no[j]) {
      if (exercise.long$no[j]) {
        #print(c(j=j, chunk = exercise.long$chunk[j]))
        #print(c(before=length(rmd.new)))
        rmd.new = c(rmd.new, chunk.null)
        #print(c(after=length(rmd.new)))
      }
    }
  
  fileConn<-file(paste0("Homework_",m,"_",gsub("[[:space:]]","_", title),".Rmd"))
  writeLines(rmd.new, fileConn)
  close(fileConn)
}
