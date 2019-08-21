library(RMySQL)  
library(RPostgreSQL)

killDbConnections <- function () {
  
  all_cons <- dbListConnections(PostgreSQL())
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}

killDbConnections()


drv=dbDriver("PostgreSQL")

con=dbConnect(drv,dbname="postgres",host="68.183.29.138",port=5432,user="ben",password="Charlene81")