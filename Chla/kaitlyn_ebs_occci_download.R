#BS chla for Kaitlyn

library(RJDBC)
library(DBI)
library(tidyverse)
library(keyring)

options(java.parameters="-Xmx12g")

# connect to AKFIN
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="../../snippets/dbconnect/java/ojdbc8.jar")


con_j <- dbConnect(jdbcDriver, 
                   "jdbc:oracle:thin:@//tiger:2045/akfin.psmfc.org", 
                   key_list("akfin_oracle_db")$username, 
                   keyring::key_get("akfin_oracle_db", keyring::key_list("akfin_oracle_db")$username))



start<-Sys.time()
data<-dbFetch(dbSendQuery(con_j, "select a.read_date, a.chlorophyll, b.*
from env_data.occci_chla a
left join env_data.occci_spatial_lookup b on a.occci_id=b.occci_id
where ecosystem_area = ('Eastern Bering Sea')"))%>%
  rename_with(tolower)
end<-Sys.time()
end-start
 saveRDS(data, "data/EBS_occci_for_kaitlyn.RDS")
 