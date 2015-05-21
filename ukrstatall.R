ukrstatall <- function(file="data/4. IV. Ãîòîâ³ õàð÷îâ³ ïðîäóêòè.xls") {
  library(xlsx); library(stringr); library(dplyr); library(zoo); library(tidyr)
  
  stats <- read.xlsx2(file, sheetIndex=1, startRow=7, header=F) 
  names(stats) <- c("country", "ei", "export_kol", "export_amount", "import_kol", "import_amount") ##names for new dataset
  stats$country[stats$country==""] <- NA ## replace all "" to NAs
  stats$country <- str_trim(na.locf(stats$country)) ##fill NA with data from above and trim spaces
  
  period <- read.xlsx2(file, sheetIndex=1, startRow=3, endRow=3, header=F) ##read inf about period
  period <- as.character(period[[1]])
  print(period)
  
  for(i in 3:6){ ##change class of column
    stats[,i] <- as.numeric(as.character(stats[,i]))
  }
  
  ##replace old ukr names with eng
  stats$ei <- sub("ë 100 % ñïèðòó", "100%spirit",stats$ei)
  stats$ei <- sub("êã", "kg",stats$ei)
  stats$ei <- sub("ë", "L",stats$ei)
  stats$ei <- sub("òèñ. øò", "x1000pcs",stats$ei)
  stats$ei <- str_trim(stats$ei) ##remove spaces
  stats$ei <- as.character(stats$ei) ##change class
  
  kodesindex <- grep("^[0-9]{10}", stats$country) ##entry 10-digits codes
  
  stats$ukt <- NA ##new column
  for(i in kodesindex){
    stats$ukt[i] <- stats$country[i] ##assigned to new column dates from 10-digits codes ukt
  }
  stats <- stats[-1,] ##remove first row
  stats$ukt <- na.locf(stats$ukt) ##fill NA with value above  
  
  ##delete summarizing rows 
  todel <- c("ÂÑÜÎÃÎ", "ÊÐÀ¯ÍÈ ÑÍÄ", "IÍØI ÊÐÀ¯ÍÈ ÑÂIÒÓ", "ªÂÐÎÏÀ", "ÀÇ²ß", "ÀÔÐÈÊÀ", "ÀÌÅÐÈÊÀ", "ÀÂÑÒÐÀË²ß ² ÎÊÅÀÍ²ß", "²ÍØ²") ## ýëåìåíòû íà óäàëåíèå
  del_others <- NULL
  for(i in 1:length(todel)){
    del_others <- c(del_others, grep(todel[i], stats$country)) ## íàõîæó ¹ âõîæäåíèÿ âñåõ ýëåìåíòîâ íà óäàëåíèå è ôîðìèðóþ èç íèõ âåêòîð
  }
  stats <- stats[-del_others,] ## óäàëÿþ èç òàáëèöû âñå ñòðîêè (âõîæäåíèÿ) ýëåìåíòîâ òèïà "ÀÇ²ß", "ÀÔÐÈÊÀ" è ò.ï. èç âåêòîðà
  
  kodesindex2 <- grep("^[0-9]{10}", stats$country) ##new code ukt index
  stats <- stats[-kodesindex2,] ##remove rows with codes. now i use stats$ukt
  
  stats$country <- as.factor(stats$country)
  stats$ukt <- as.factor(stats$ukt)
  
  stats <- gather(stats, destination_izmerenie, summa, -c(country, ei, ukt)) ##change columns to rows. new col names dest_izmer, exept column countr, ei etc
  stats <- separate(stats, col=destination_izmerenie, into = c("dest", "mera")) ##separate new column to 2 column
  stats <- stats[!stats$ei=="" & stats$summa>0,] ##del rows w/o ei and summa
  
  stats$mera <- as.character(stats$mera)
  stats$dest <- as.factor(stats$dest)
  nmera <- stats$mera=="amount"
  stats$ei[nmera] <- "USDthnds"
  stats <- stats[!is.na(stats$country), -5]
  
  per <- substr(period,nchar(period)-23,nchar(period)) ##extract some inf about period for filename
  write.xlsx2(stats, paste(per,".xlsx"), row.names=F)
  
  
}