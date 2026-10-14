# Sablefish EBS ESP indicator
library(tidyverse)
library(httr)
library(jsonlite)
library(keyring)

####public####

get_all_esp <- function() {
  jsonlite::fromJSON(
    httr::content(
      httr::GET("https://apex.psmfc.org/akfin/data_marts/akmp/esp_indicators?"),
      as="text", encoding="UTF-8")) %>%
    dplyr::bind_rows() %>%
    rename_with(tolower)
}

esp <-get_all_esp()

jens<- esp %>% filter(indicator_name=="Spring_Chlorophylla_Peak_SEBS_Satellite")
unique(jens$description)

# lets see if this changed when we switched products
jens %>%
  ggplot()+
  geom_line(aes(x=year, y=indicator_value))

# This looks like May peak bloom averaged by jens area

# connect to AKFIN
# load packages
library(RJDBC)
library(tidyverse)
library(janitor)
library(keyring)
library(getPass)
library(odbc)

# open RJDBC connection
# set memory, not necessary unless you're transferring a lot of data.
options(java.parameters="-Xmx8g")

# 
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="../../snippets/dbconnect/java/ojdbc8.jar")

con_j <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//tiger:2045/akfin.psmfc.org", key_list("akfin_oracle_db")$username, keyring::key_get("akfin_oracle_db", keyring::key_list("akfin_oracle_db")$username))



oc3 <- dbGetQuery(con_j, "WITH avg_jens AS (
    SELECT 
        EXTRACT(YEAR FROM TO_DATE(read_date, 'YYYY-MM-DD') + 4) AS year,
        TO_NUMBER(TO_CHAR(TO_DATE(read_date, 'YYYY-MM-DD') + 4, 'DDD')) AS doy,
        b.jens_grid,
        AVG(a.chlorophyll) AS meanchla
    FROM env_data.occci_chla a
    LEFT JOIN env_data.occci_spatial_lookup b 
        ON a.occci_id = b.occci_id
    WHERE EXTRACT(MONTH FROM TO_DATE(read_date, 'YYYY-MM-DD') + 4) in (4,5,6)
      AND b.ecosystem_subarea = 'Southeastern Bering Sea'
      AND b.depth <= -10
    GROUP BY 
        EXTRACT(YEAR FROM TO_DATE(read_date, 'YYYY-MM-DD') + 4),
        TO_NUMBER(TO_CHAR(TO_DATE(read_date, 'YYYY-MM-DD') + 4, 'DDD')),
        b.jens_grid
),
avg_ebs AS (
    SELECT 
        year, 
        doy, 
        ROUND(AVG(meanchla), 2) AS meanchla
    FROM avg_jens
    GROUP BY year, doy
),        
ranked_data AS (
    SELECT
        year,
        doy,
        meanchla,
        ROW_NUMBER() OVER (PARTITION BY year ORDER BY meanchla DESC) AS rn
    FROM avg_ebs
)
SELECT
    year,
    'Spring_Chlorophylla_Peak_SEBS_Satellite' AS indicator_name,
    doy AS indicator_value
FROM ranked_data
WHERE rn = 1")  %>% rename_with(tolower)

# oc has just May

# oc2 has April and June too
# oc3 will have a depth filter of -200

ggplot()+
  geom_line(data=jens,aes(x=year, y=indicator_value), color="red")+
  #geom_line(data=oc2, aes(x=year, y=indicator_value), lty=2)+
  #geom_line(data=oc, aes(x=year, y=data_value))+
  geom_line(data=oc3, aes(x=year, y=indicator_value), lty=3)

j<-jens %>%
  left_join(oc3, by=)
  
ggplot()+
  geom_point()


# OK new plan: run chl indicators as I do for the GOA chl and EBS SST
# ecosystem_sub = 'Southeastern Bering Sea' and a -200 depth filter for pcod
# lets explore spatial averaging further- get record counts for jens grid



jg <- dbGetQuery(con_j, "select count(*) n, jens_grid from env_data.occci_spatial_lookup 
                 where ecosystem_subarea='Southeastern Bering Sea'
                 group by jens_grid
                 order by count(*)")  %>% rename_with(tolower)
# 58 (29%) are not the same size as the remaining 142
# 30 (15%) have half or less as many cells

# now let's try with the depth filter
jgd <- dbGetQuery(con_j, "select count(*) n, jens_grid from env_data.occci_spatial_lookup 
                 where ecosystem_subarea='Southeastern Bering Sea'
                 and depth <= -10
                 and depth >= -200
                 group by jens_grid
                 order by count(*)")  %>% rename_with(tolower)
# this drops the count from 200 to 162
# 64 out of 162 (40%) are partially cut out
# 24 (19%) have half or less as many cells

# I think I am going to skip the intermediate spatial averaging step to align with the GOA contributions.

