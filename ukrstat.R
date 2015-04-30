library(xlsx); library(stringr)
stats <- read.xlsx2("stat01.xls", sheetIndex=1, startRow=7, header=F) ##
names(stats) <- c("name", "ei", "exportkol", "exportsum", "importcol", "importsum")

period <- read.xlsx2("stat01.xls", sheetIndex=1, startRow=3, endRow=3, header=F)
period <- period[[1]]


vodkastart <- grep("2208600000", stats$name) ##âõîæäåíèå êîäà âîäêè
vodkaend <- grep("2208700000", stats$name)-1 ##âõîæäåíèå ñëåäóþùåãî êîäà
stats <- stats[vodkastart:vodkaend,]
for(i in 3:6){ ##change class of column
  stats[,i] <- as.numeric(as.character(stats[,i]))
}
stats$ei <- sub("ë 100 % ñïèðòó", "purespiritlitr",stats$ei)
stats$ei <- sub("êã", "kilo",stats$ei)
stats$ei <- str_trim(stats$ei)
stats <- stats[-1,]

for(i in 1:length(stats$name)){
  if(stats$name[i]=="") stats$name[i] <- stats$name[i-1] 
}

for(i in 1:length(stats$exportsum)){
  if(is.na(stats$exportsum[i])) stats$exportsum[i] <- stats$exportsum[i-1] 
}

for(i in 1:length(stats$importsum)){
  if(is.na(stats$importsum[i])) stats$importsum[i] <- stats$importsum[i-1] 
}

stats <- stats[stats$ei=="purespiritlitr", ]

todel <- c("ÂÑÜÎÃÎ", "ÊÐÀ¯ÍÈ ÑÍÄ", "IÍØI ÊÐÀ¯ÍÈ ÑÂIÒÓ", "ªÂÐÎÏÀ", "ÀÇ²ß", "ÀÔÐÈÊÀ", "ÀÌÅÐÈÊÀ", "ÀÂÑÒÐÀË²ß ² ÎÊÅÀÍ²ß", "²ÍØ²")
del_others <- NULL
for(i in 1:length(todel)){
  del_others <- c(del_others, grep(todel[i], stats$name))
}

stats <- stats[-del_others,-2]

write.xlsx2(stats, "stats_clean.xlsx", row.names=F)

