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
con_j <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@akfin", key_list("akfin_oracle_db")$username, keyring::key_get("akfin_oracle_db", keyring::key_list("akfin_oracle_db")$username))



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
                 and depth <= -10
                 group by jens_grid
                 order by count(*)")  %>% rename_with(tolower)
# 59 (30%) are not the same size as the remaining 142
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
# But let's compare first...
# for no depth filter
sebs_jg <- dbGetQuery(con_j, "WITH avg_jens AS (
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
    doy AS avg_by_jens_grid
FROM ranked_data
WHERE rn = 1")  %>% rename_with(tolower)

# no intermediate averaging
sebs_njg <- dbGetQuery(con_j, "WITH ranked_data AS (
    SELECT
         year,
         doy,
        meanchla,
        ROW_NUMBER() OVER (PARTITION BY year ORDER BY meanchla DESC) AS rn
        from (select extract(year from to_date(read_date,'YYYY-MM-DD')+4) year,
        to_number(to_char(to_date(read_date,'YYYY-MM-DD')+4,'DDD')) doy,
        round(avg(chlorophyll),2) meanchla
    from env_data.occci_chla a
left join env_data.occci_spatial_lookup b on a.occci_id=b.occci_id
where extract(month from to_date(read_date,'YYYY-MM-DD')+4) in (4, 5, 6)
AND b.ecosystem_subarea = 'Southeastern Bering Sea'
      AND b.depth <= -10
        group by  extract(year from to_date(read_date,'YYYY-MM-DD')+4),
        to_number(to_char(to_date(read_date,'YYYY-MM-DD')+4,'DDD'))
))
SELECT
    year,
    'Spring_Chlorophylla_Peak_GOA_Satellite' as indicator_name,
    doy no_intermediate_avg
FROM ranked_data
WHERE rn = 1" )  %>% rename_with(tolower)

jd<-sebs_jg %>%
  left_join(sebs_njg, by="year")

jd %>%
  ggplot()+
  geom_point(aes(x=avg_by_jens_grid, y=no_intermediate_avg))+
  geom_abline(slope=1, intercept=0, color="red")

jd %>%
  ggplot()+
  geom_line(aes(x=year, y=avg_by_jens_grid), color="red")+
  geom_line(aes(x=year, y=no_intermediate_avg))

# huh, now let's try it with the partial ones removed and see if that makes it more aligned
# let's use a 192 cut off - that's 2/3 covered - for inclusion
excludeme<-(jg %>%
  filter(n<=192))$jens_grid

sebs_jge <- dbGetQuery(con_j, paste0("WITH avg_jens AS (
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
      and b.jens_grid not in (", paste0("'",excludeme,"'", collapse=","), ")
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
    doy AS avg_by_jens_grid_exclusive
FROM ranked_data
WHERE rn = 1"))  %>% rename_with(tolower)

jd<-sebs_jg %>%
  left_join(sebs_njg, by="year") %>%
  left_join(sebs_jge, by="year")

jd %>%
  ggplot()+
  geom_point(aes(x=avg_by_jens_grid, y=no_intermediate_avg))+
  geom_point(aes(x=avg_by_jens_grid, y=avg_by_jens_grid_exclusive), color="maroon")+
  geom_abline(slope=1, intercept=0, lty=2)

jd %>%
  ggplot()+
  geom_line(aes(x=year, y=avg_by_jens_grid), color="red", size=3)+
  geom_line(aes(x=year, y=avg_by_jens_grid_exclusive), color="orange", size=1.5)+
  geom_line(aes(x=year, y=no_intermediate_avg), color="blue")+
  ggtitle("Red = avg by jens grid \nOrange = avg by jens grid with partial grids removed\nBlue = no intermediate averaging")+
  ylab("peak bloom date")

# concentration
sebs_nsa <- dbGetQuery(con_j, paste0("select extract(year from to_date(read_date,'YYYY-MM-DD')+4) year, 
'Spring_Chlorophylla_Biomass_GOA_Satellite' as indicator_name, round(avg(chlorophyll),2) data_value
from env_data.occci_chla a
left join env_data.occci_spatial_lookup b on a.occci_id=b.occci_id
where extract(month from to_date(read_date,'YYYY-MM-DD')+4) in (4, 5, 6)
AND b.ecosystem_subarea = 'Southeastern Bering Sea'
AND b.depth <= -10
group by extract(year from to_date(read_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(read_date,'YYYY-MM-DD')+4) asc"))  %>% rename_with(tolower)
sebs_nsa <-sebs_nsa %>% mutate(conc_no_avg=data_value)


sebs_sa1 <- dbGetQuery(con_j, paste0("WITH avg_jens AS (
    SELECT 
        EXTRACT(YEAR FROM TO_DATE(read_date, 'YYYY-MM-DD') + 4) AS year,
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
        b.jens_grid
),
avg_ebs AS (
    SELECT 
        year, 
        ROUND(AVG(meanchla), 2) AS conc_jens_grid
    FROM avg_jens
    GROUP BY year
)
SELECT * from avg_ebs"))  %>% rename_with(tolower)

sebs_sa2 <- dbGetQuery(con_j, paste0("WITH avg_jens AS (
    SELECT 
        EXTRACT(YEAR FROM TO_DATE(read_date, 'YYYY-MM-DD') + 4) AS year,
        b.jens_grid,
        AVG(a.chlorophyll) AS meanchla
    FROM env_data.occci_chla a
    LEFT JOIN env_data.occci_spatial_lookup b 
        ON a.occci_id = b.occci_id
    WHERE EXTRACT(MONTH FROM TO_DATE(read_date, 'YYYY-MM-DD') + 4) in (4,5,6)
      AND b.ecosystem_subarea = 'Southeastern Bering Sea'
      AND b.depth <= -10
      And b.jens_grid not in (", paste0("'",excludeme,"'", collapse=","), ")
    GROUP BY 
        EXTRACT(YEAR FROM TO_DATE(read_date, 'YYYY-MM-DD') + 4),
        b.jens_grid
),
avg_ebs AS (
    SELECT 
        year, 
        ROUND(AVG(meanchla), 2) AS conc_jens_grid_exclusive
    FROM avg_jens
    GROUP BY year
)
SELECT * from avg_ebs"))  %>% rename_with(tolower)

jc<-sebs_nsa %>%
  left_join(sebs_sa1, by="year") %>%
  left_join(sebs_sa2, by="year")


jc %>%
  ggplot()+
  geom_line(aes(x=year, y=conc_jens_grid_exclusive), color="red", size=3)+
  geom_line(aes(x=year, y=conc_jens_grid_exclusive), color="orange", size=1.5)+
  geom_line(aes(x=year, y=conc_no_avg), color="blue")+
  ggtitle("Red = avg by jens grid \nOrange = avg by jens grid with partial grids removed\nBlue = no intermediate averaging")+
  ylab("avg chlorophyll concentration")
