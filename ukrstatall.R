ukrstatall <- function() {
        library(xlsx); library(stringr); library(dplyr); library(zoo); 
        library(tidyr); library(ggplot2); library(RPostgreSQL); library(reshape)
        a <- Sys.time()
        allstats <- NULL
        
        setwd("~/ukrstat")
        
        dir_list <- list.dirs(recursive = F, full.names = F ) ##read all dirs from wd
        
        for(dirperiod in dir_list){
                print(dirperiod)
                
                file_list <- list.files(dirperiod, pattern="*.xls*", full.names=1) ##read all xls files from dirs
                nomer <- 0
                
                for(ustats in file_list) {
                        nomer <- nomer+1
                        a1 <- substr(file_list[nomer], 20, 200)
                        a2 <- substr(file_list[nomer+1], 20, 200)
                        a3 <- substr(file_list[nomer-1], 20, 200)
                        
                        twofiles <- min(nchar(file_list))
                        if(sum(nchar(file_list)==twofiles)<2) twofiles <- 1
                        
                        
                        if(identical(a1, a2)|identical(a1, a3)|nchar(ustats)==twofiles) startROW <- 8 ##for valid read data in 2 files
                        else startROW <- 7
                        
                        print(ustats); 
                        
                        stats <- read.xlsx2(ustats, sheetIndex=1, startRow=startROW, header=F, colClasses = c("character", "character", "character", "character", "character", "character")) 
                        names(stats) <- c("country", "ei", "export_kol", "export_amount", "import_kol", "import_amount") ##names for new dataset
                        stats$country[stats$country==""] <- NA ## replace all "" to NAs
                        
                        if(grepl("Р.зне.*$", ustats)) {
                                stats$country <- paste("00000000", "prochie", sep="\n")
                                stats$ei <- "кг"
                                stats <- rbind(stats, stats, stats, stats)
                                stats[2,1] <- "0000000_ІНШІ"
                                stats[4,1] <- "OTHERS"
                        }
                        
                        stats$country <- str_trim(na.locf(stats$country)) ##fill NA with data from above and trim spaces
                        
                        stats[,3:6] <- lapply(stats[,3:6], as.character)
                        stats[,3:6] <- lapply(stats[,3:6], as.numeric)
                        
                        
                        ##replace old ukr names with eng
                        stats$ei <- sub("кг", "kg",stats$ei)
                        stats$ei <- str_trim(stats$ei) ##remove spaces
                        
                        kodesindex <- grep("^[0-9]{6}", stats$country) ##entry 10-digits codes, some file include less-digit number
                        
                        stats$ukt <- NA ##new column
                        stats$ukt[kodesindex] <- stats$country[kodesindex] ##assigning to new column from 10-digits codes ukt
                        
                        stats <- stats[-1,] ##remove first row
                        stats$ukt <- na.locf(stats$ukt) ##fill NA with value above  
                        
                        todel <- c("ВСЬОГО", "КРАЇНИ СНД", "IНШI КРАЇНИ СВIТУ", "ЄВРОПА", "АЗІЯ", "^АФРИКА$", "АМЕРИКА", "АВСТРАЛІЯ І ОКЕАНІЯ", "ІНШІ") ## элементы на удаление
                        del_others <- grep(paste(todel, collapse = "|"), stats$country) ## find all string that must be deleted
                        
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
                        
                        stats <- stats[stats$ei=="USDthnds",] ##nrows decrease from 370k to 164k
                        ##mainukt <- grep("0{6}$", stats$ukt) ##find ukt number which ends with 6 zeros
                        ##stats <- stats[mainukt, -2] ##decrease to 53k, del column ei, 20 Mb csv file
                        ##stats <- stats[, -2] ## del column ei, leave all ukt codes. 40 Mb csv file!
                        
                        stats$period <- dirperiod ##add column with name of dir, must be period
                        allstats <- rbind(allstats, stats)
                        
                }
        }
        
        ##add countries
        gC <- read.csv("groupCountry.csv", sep=";")
        allstats$country <- as.character(allstats$country)
        gC$country <- as.character(gC$country)
        
        print(paste("start joining at:", Sys.time()))
        
        A <<- allstats
        allstats <- merge(x=allstats, y=gC, by="country", all.x = T) ##work faster than left_join
        
        ##add UKT razdel and groups
        Ucodes <- read.csv("UKTcodes.csv", sep=";")
        Ucodes$cod <- as.character(Ucodes$cod)
        allstats$cod <- 0
        allstats$cod <- substr(allstats$ukt, 1,(nchar(allstats$ukt)-8))
        
        allstats <- merge(x=allstats, y=Ucodes, by="cod", all.x = T)
        allstats$ukt <- as.numeric(allstats$ukt)
        
        print(paste("start pivoting data. Begin at:", Sys.time()))
        ##level placement
        allstats$level <- 0
        for(i in 0:3){
          b <- grepl(paste("(^-){",1,"} +(- ){", i,"}", sep = ""), allstats$group)
          allstats$level[b] <- i+1
        }
        Amain <- allstats[,c(-1,-3)]
        
        ##-----------удаление суммарных строк, для сводной таблицы. долго, переделать
        #Amain <- Amain[order(Amain$ukt),]
        #Amain$pivot <- Amain$group
        #Amain$pivot <- ifelse(Amain$level%%1000000==0, Amain$pivot, NA)
        #Amain$pivot <- na.locf(Amain$pivot)
        #lvls <- aggregate(level ~ pivot, Amain, sum)
        #lvls$del <- lvls$level!=0 ##найти где сумма уровней не равна 0, т.е. 0-ой уровень в которых можно удалить
        #lvls <- lvls[lvls$del==TRUE,-2]
        #print(paste("merging for pivot. Begin at:", Sys.time()))
        #Amain <- merge(x=Amain, y=lvls, by =  "pivot", all.x = T) #по текстовому полю долго мержить
        #print("удаление лишней информации")
        #Amain <- Amain[Amain$level+Amain$del,]
        ##-------------
        Amain <- unique(Amain)
        B <<- Amain
        ##separate months
        print(paste("separate months. Begin at:", Sys.time()))
        per <- levels(factor(Amain$period))
        Amain <- spread(Amain, period, thUSD)
        for(i in per){
          Amain[i][is.na(Amain[i])] <- 0
        }
        
        for(i in length(per):2){
          Amain[per[i]] <- Amain[per[i]]-Amain[per[i-1]]
        }
        print(paste("gather months. Begin at:", Sys.time()))
        Amain <- Amain[,-9] ##delete first period. only if it is not january
        Amain <- gather(Amain, "period", "thUSD", 9:length(names(Amain)))
        Amain <- Amain[Amain$thUSD!=0,]
        
        
        print(paste("start writing data. Begin at:", Sys.time()))
        print(object.size(Amain), units="Mb")
        Sys.time()
        ##write.csv2(Amain, "1q2015.csv", row.names = F, append = T) ## then open with MS excel and save as xlsx
        ##write.table(Amain, file = "1q2015.csv", append = T, sep = ";", dec = ",", row.names = F, col.names = F, qmethod = "double")
        print(paste("End at:", Sys.time()))
        b <- Sys.time()
        return(b-a)
        
        
        ##A <- Amain[order(Amain$ukt),] ##если без (-) добавить значение в новый столбец перенести значение ниже удалить строку
        ##write.xlsx2(Amain, "Allstats.xlsx", row.names=F) ##out of memory2
        
        ##plotting
        ##q <- ggplot(Amain, aes(x=dest, y=thUSD/1000000, fill=dest)) +
        ##        geom_bar(stat="identity") + ##использую сумму а не кол-во 
        ##        facet_wrap(~period) + 
        ##        labs(title = "Экспорт и Импорт за 2013-2014 гг.", x = "", y = "млрд. долларов США") + 
        ##        coord_cartesian(ylim = c(0, 80))
        ##a <- xtabs(thUSD ~ groupCountry + dest,data=Amain)
        ##plot(a, col=Amain$groupCountry, main="Соотношение импорта/экспорта по группам стран")
        
        ##write to postgresql
        ##drv <- dbDriver("PostgreSQL")
        ##con <- dbConnect(drv, dbname = "postgres",
        ##                 host = "localhost", port = 5432,
        ##                 user = "postgres", password = pw)
        ##dbWriteTable(con, "import_export_3q", value = Amain, append = TRUE, row.names = FALSE)

}