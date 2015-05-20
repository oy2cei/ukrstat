ukrstat <- function(file="4. IV. Ãîòîâ³ õàğ÷îâ³ ïğîäóêòè.xls", ukt="2208600000") { ##ğàáîòàåò òîëüêî ñ âîäêàìè, ãäå ÅÈ óêàçàíû "ë 100 % ñïèğòó"
        library(xlsx); library(stringr)
        stats <- read.xlsx2(file, sheetIndex=1, startRow=7, header=F) ## 
        kods <- read.xlsx2("UKT_ved.xlsx", sheetIndex=1, colClasses=c("numeric","character"), stringsAsFactors=F)
        
        names(stats) <- c("country", "ei", "export_kol", "export+sum", "import_kol", "import_sum")
        
        period <- read.xlsx2(file, sheetIndex=1, startRow=3, endRow=3, header=F)
        period <- as.character(period[[1]])
        print(period)
        
        print(kods[kods$kod==ukt,])
        
        vodkastart <- grep(ukt, stats$country) ##âõîæäåíèå êîäà âîäêè
        vodkaend <- grep("2208700000", stats$country)-1 ##âõîæäåíèå ñëåäóşùåãî êîäà
        stats <- stats[vodkastart:vodkaend,]
        for(i in 3:6){ ##change class of column
                stats[,i] <- as.numeric(as.character(stats[,i]))
        }
        stats$ei <- sub("ë 100 % ñïèğòó", "purespiritlitr",stats$ei)
        stats$ei <- sub("êã", "kilo",stats$ei)
        stats$ei <- str_trim(stats$ei)
        stats <- stats[-1,]
        
        ## çàïîëíÿş çíà÷luåíèÿìè èç ÿ÷åéêè ñâåğõó. îñîáåííîñòü òîëüêî ıòîãî îò÷åòà. ïîòîì óäàëÿş ñòğîêè ñ êã.
        for(i in 1:length(stats$country)){
                if(stats$country[i]=="") stats$country[i] <- stats$country[i-1] 
        }
        for(i in 1:length(stats$exportsum)){
                if(is.na(stats$exportsum[i])) stats$exportsum[i] <- stats$exportsum[i-1] 
        }
        for(i in 1:length(stats$importsum)){
                if(is.na(stats$importsum[i])) stats$importsum[i] <- stats$importsum[i-1] 
        }
        
        stats <- stats[stats$ei=="purespiritlitr", ] ## îñòàâëÿş òîëüêî ñ íóæíûìè ÅÈ
        
        todel <- c("ÂÑÜÎÃÎ", "ÊĞÀ¯ÍÈ ÑÍÄ", "IÍØI ÊĞÀ¯ÍÈ ÑÂIÒÓ", "ªÂĞÎÏÀ", "ÀÇ²ß", "ÀÔĞÈÊÀ", "ÀÌÅĞÈÊÀ", "ÀÂÑÒĞÀË²ß ² ÎÊÅÀÍ²ß", "²ÍØ²") ## ıëåìåíòû íà óäàëåíèå
        del_others <- NULL
        for(i in 1:length(todel)){
                del_others <- c(del_others, grep(todel[i], stats$country)) ## íàõîæó ¹ âõîæäåíèÿ âñåõ ıëåìåíòîâ íà óäàëåíèå è ôîğìèğóş èç íèõ âåêòîğ
        }
        
        stats <- stats[-del_others,-2] ## óäàëÿş èç òàáëèöû âñå ñòğîêè (âõîæäåíèÿ) ıëåìåíòîâ òèïà "ÀÇ²ß", "ÀÔĞÈÊÀ" è ò.ï. èç âåêòîğà
        
        per <- substr(period,nchar(period)-23,nchar(period))
        write.xlsx2(stats, paste(per,".xlsx"), row.names=F)
        
}
