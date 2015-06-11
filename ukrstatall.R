ukrstatall <- function(file="data") {
        ##options(java.parameters = "-Xmx3500m") ##to avoid java out of memory error, must be out of function
        library(xlsx); library(stringr); library(dplyr); library(zoo); library(tidyr)
        allstats <- NULL
        
        dir_list <- list.dirs(recursive = F, full.names = F) ##read all dirs from wd
        
        for(dirperiod in dir_list){
                print(dirperiod)
                
                file_list <- list.files(dirperiod, pattern="*.xls*", full.names=1) ##read all xls files from dirs
                nomer <- 0
                for(ustats in file_list) {
                        
                        nomer <- nomer+1
                        a1 <- substr(file_list[nomer], 15, 999)
                        a2 <- substr(file_list[nomer+1], 15, 999)
                        a3 <- substr(file_list[nomer-1], 15, 999)
                        
                        if(identical(a1, a2)|identical(a1, a3)) startROW <- 8 ##for valid read data in 2 files
                        else startROW <- 7
                        
                        print(ustats); 
                        
                        stats <- read.xlsx2(ustats, sheetIndex=1, startRow=startROW, header=F) 
                        ##startROW <- 7
                        names(stats) <- c("country", "ei", "export_kol", "export_amount", "import_kol", "import_amount") ##names for new dataset
                        stats$country[stats$country==""] <- NA ## replace all "" to NAs
                        stats$country <- str_trim(na.locf(stats$country)) ##fill NA with data from above and trim spaces
                        
                        ##period <- read.xlsx2(ustats, sheetIndex=1, startRow=3, endRow=3, header=F) ##read inf about period
                        ##period <- as.character(period[[1]])
                        ##print(period)
                        
                        for(i in 3:6){ ##change class of column
                                stats[,i] <- as.numeric(as.character(stats[,i]))
                        }
                        
                        ##replace old ukr names with eng
                        ##stats$ei <- sub("л 100 % спирту", "100%spirit",stats$ei)
                        stats$ei <- sub("кг", "kg",stats$ei)
                        ##stats$ei <- sub("л", "L",stats$ei)
                        ##stats$ei <- sub("тис. шт", "x1000pcs",stats$ei)
                        stats$ei <- str_trim(stats$ei) ##remove spaces
                        #######stats$ei <- as.character(stats$ei) ##change class
                        
                        kodesindex <- grep("^[0-9]{6}", stats$country) ##entry 10-digits codes, some file include less-difit number
                        
                        stats$ukt <- NA ##new column
                        for(i in kodesindex){
                                stats$ukt[i] <- stats$country[i] ##assigned to new column dates from 10-digits codes ukt
                        }
                        stats <- stats[-1,] ##remove first row
                        stats$ukt <- na.locf(stats$ukt) ##fill NA with value above  
                        
                        ##delete summarizing rows. может без этого блока можно обойтись?
                        ##удалив в конце строки не прошедшие джойн с группами стран
                        todel <- c("ВСЬОГО", "КРАЇНИ СНД", "IНШI КРАЇНИ СВIТУ", "ЄВРОПА", "АЗІЯ", "^АФРИКА$", "АМЕРИКА", "АВСТРАЛІЯ І ОКЕАНІЯ", "ІНШІ") ## элементы на удаление
                        del_others <- NULL
                        for(i in 1:length(todel)){
                                del_others <- c(del_others, grep(todel[i], stats$country)) ## нахожу № вхождения всех элементов на удаление и формирую из них вектор
                        }
                        stats <- stats[-del_others,] ## удаляю из таблицы все строки (вхождения) элементов типа "АЗІЯ", "АФРИКА" и т.п. из вектора
                        
                        kodesindex2 <- grep("^[0-9]{6}", stats$country) ##new code ukt index
                        stats <- stats[-kodesindex2,] ##remove rows with codes. now i use stats$ukt
                        
                        ##realise with sapply
                        stats$country <- as.factor(stats$country)
                        stats$ukt <- as.factor(stats$ukt)
                        
                        stats <- gather(stats, destination_izmerenie, thUSD, -c(country, ei, ukt)) ##change columns to rows. new col names dest_izmer, exept column countr, ei etc
                        stats <- separate(stats, col=destination_izmerenie, into = c("dest", "mera")) ##separate new column to 2 column
                        stats <- stats[!stats$ei=="" & stats$thUSD>0,] ##del rows w/o ei and thUSD
                        
                        stats$mera <- as.character(stats$mera)
                        stats$dest <- as.factor(stats$dest)
                        nmera <- stats$mera=="amount"
                        stats$ei[nmera] <- "USDthnds"
                        stats <- stats[!is.na(stats$country), -5]
                        stats <- separate(stats, col=ukt, into=c("ukt", "group"), sep="\n") ##separate kode from groupe
                        ##ukt must be a factor
                        ##найти вхождения "-" перенести их в другой столбец, обнулить в "группа",
                        ##если сумма всех групп без "-" будет сходиться с общей суммой 
                        
                        stats <- stats[stats$ei=="USDthnds",] ##nrows decrease from 370k to 164k
                        mainukt <- grep("0{6}$", stats$ukt) ##find ukt number which ends with 6 zeros
                        stats <- stats[mainukt, -2] ##decrease to 53k, dek column ei
                        stats$period <- dirperiod ##add column with name of dir, must be period
                        allstats <- rbind(allstats, stats)
                        
                }
        }
        gC <- read.csv("groupCountry.csv", sep=";")
        allstats$country <- as.character(allstats$country)
        gC$country <- as.character(gC$country)
        print("start joining")
        allstats <- left_join(allstats, gC, by="country")
        ##Amain <<- allstats
        write.xlsx2(allstats, "Allstats.xlsx", row.names=F)
        
}