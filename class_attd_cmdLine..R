library(shiny)
library(data.table)
library(stringr)
library(dplyr)
library(readxl)

fun1 <- function(str1, list1){
  L = length(list1)
  k = rep(0,L)
  
  for(i in 1:L){
    k[i] = as.integer(str_detect(list1[i],str1))
  }
  
  if(sum(k) == 1){
    return (1)
  }else{
    return (0)
  }
  
}


data_proc <-function(fName_attdn, fName_roster, str1){
  
  col_name = str_split(basename(fName_attdn),".txt")[[1]][1]
  atd = readLines(fName_attdn)
  
  in_class_rid_tmp = atd[which(atd == "avatar")+1]
  if(str1 == ""){
    str1 = "Debajit Goswami"
  }
  in_class_rid = in_class_rid_tmp[-which(in_class_rid_tmp == str1)]
  
  #df = fread(fName_roster)
  df = readxl::read_excel(path = fName_roster, sheet=1)
  df$attdn1 = as.integer(unlist(lapply(df$`REGISTER NO`, function(x){fun1(x, in_class_rid)})))
  
  h = dim(df)[2]
  df1 = df %>% filter(attdn1 == 1) 
  
  colnames(df)[h] = col_name
  df1 = df1[, c(2,3)]
  
  out_basename = sprintf("modified_%s.csv", col_name)
  out_fName = file.path(dirname(fName_attdn), out_basename)
  
  fwrite(df, file = out_fName, sep = ",")
  
  return(df1)
}

#args <- commandArgs(trailingOnly = TRUE)
#print(args)
args=NULL
args[1] = "~/Desktop/debu_da_code/Enrolled_List_Report_01-05-2020_F1.xlsx"
args[2] = "~/Desktop/debu_da_code/05.05.F1slot.txt"
args[3] = ""

cat(length(args),"\n")
if(length(args) !=3) {
  stop("Please input: Rscript <Roster file> <Attendance file> <Host Participant ID>\n")
}
df1 = data_proc(args[2], args[1], args[3])
sprintf("Attendee List (%d Students)", dim(df1)[1])
print(df1)
 
  
 