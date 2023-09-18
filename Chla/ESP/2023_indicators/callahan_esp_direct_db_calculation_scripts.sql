-- globcolour indicator calculation code 2023
-- Matt Callahan

/*
This code modifies the globcolour data so I can join it with the lookup table
create table globcolour_2023 as
(select ROUND(to_number(GLOB_ID),0) GLOB_ID,
START_DATE,
ROUND(TO_NUMBER(CHLA),2) CHLA
FROM GLOBCOLOUR);

commit;
*/

--Crab indicators
-- AMJ_Chlorophylla_Biomass_SEBS_Satellite
select extract(year from to_date(start_date,'YYYY-MM-DD')+4) year, 'AMJ_Chlorophylla_Biomass_SEBS_Satellite' as indicator_name, round(avg(chla),2) data_value
from env_data.globcolour_2023 a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and bsierp_region_name in ('St. Matthew','Pribilofs')
group by extract(year from to_date(start_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(start_date,'YYYY-MM-DD')+4) asc;

-- Spring_Chlorophylla_Biomass_SEBS_Inner_Shelf_Satellite
select extract(year from to_date(start_date,'YYYY-MM-DD')+4) year, 'Spring_Chlorophylla_Biomass_SEBS_Inner_Shelf_Satellite' as indicator_name, round(avg(chla),2) data_value
from env_data.globcolour_2023 a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and bs_king='BBRKC'
group by extract(year from to_date(start_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(start_date,'YYYY-MM-DD')+4) asc;

--Spring_Chlorophylla_Biomass_SMBKC_Satellite
select extract(year from to_date(start_date,'YYYY-MM-DD')+4) year, 'Spring_Chlorophylla_Biomass_SMBKC_Satellite' as indicator_name, round(avg(chla),2) data_value
from env_data.globcolour_2023 a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and bs_king='StMattBKC'
group by extract(year from to_date(start_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(start_date,'YYYY-MM-DD')+4) asc;


-- We will submit the following using a text file to test the parser



--Groundfish Indicators
--Spring_Chlorophylla_Biomass_GOA_Satellite
select extract(year from to_date(start_date,'YYYY-MM-DD')+4) year, 'Spring_Chlorophylla_Biomass_GOA_Satellite' as indicator_name, round(avg(chla),2) data_value
from env_data.globcolour_2023 a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and NMFS_REP_AREA in ('610', '620', '630', '640', '650')
and depth <= (-10)
group by extract(year from to_date(start_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(start_date,'YYYY-MM-DD')+4) asc;

--Spring_Chlorophylla_Peak_GOA_Satellite

WITH ranked_data AS (
    SELECT
         year,
         doy,
        meanchla,
        ROW_NUMBER() OVER (PARTITION BY year ORDER BY meanchla DESC) AS rn
        from (select extract(year from to_date(start_date,'YYYY-MM-DD')+4) year,
        to_number(to_char(to_date(start_date,'YYYY-MM-DD')+4,'DDD')) doy, 
        round(avg(chla),2) meanchla 
    FROM env_data.globcolour_2023 a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and NMFS_REP_AREA in ('610', '620', '630', '640', '650')
and depth <= (-10)
        group by  extract(year from to_date(start_date,'YYYY-MM-DD')+4),
        to_number(to_char(to_date(start_date,'YYYY-MM-DD')+4,'DDD'))
))
SELECT
    year,
    'Spring_Chlorophylla_Peak_GOA_Satellite' as indicator_name,
    doy data_value
FROM ranked_data
WHERE rn = 1;


--Spring_Chlorophylla_Biomass_WCGOA_Satellite
select extract(year from to_date(start_date,'YYYY-MM-DD')+4) year, 'Spring_Chlorophylla_Biomass_WCGOA_Satellite' as indicator_name, round(avg(chla),2) data_value, count(*) n_values
from env_data.globcolour_2023 a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) = 5 
and NMFS_REP_AREA in ('610', '620', '630')
and depth <= (-10)
and depth > (-200)
group by extract(year from to_date(start_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(start_date,'YYYY-MM-DD')+4) asc;

--Spring_Chlorophylla_Peak_WCGOA_Satellite
WITH ranked_data AS (
    SELECT
         year,
         doy,
        meanchla,
        ROW_NUMBER() OVER (PARTITION BY year ORDER BY meanchla DESC) AS rn
        from (select extract(year from to_date(start_date,'YYYY-MM-DD')+4) year,
        to_number(to_char(to_date(start_date,'YYYY-MM-DD')+4,'DDD')) doy, 
        round(avg(chla),2) meanchla 
    FROM env_data.globcolour_2023 a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and NMFS_REP_AREA in ('610', '620', '630')
and depth <= (-10)
and depth >= (-200)
        group by  extract(year from to_date(start_date,'YYYY-MM-DD')+4),
        to_number(to_char(to_date(start_date,'YYYY-MM-DD')+4,'DDD'))
))
SELECT
    year,
    'Spring_Chlorophylla_Peak_GOA_Satellite' as indicator_name,
    doy data_value
FROM ranked_data
WHERE rn = 1;

--ESR query
select round(avg(chla),2) meanchla, to_date(start_date,'YYYY-MM-DD')+4 mid_date, ecosystem_subarea
from env_data.globcolour_2023 a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and ecosystem_area = ('Gulf of Alaska')
and waters_cod = 'FED'
and depth>(-200)
and depth<(-10)
group by to_date(start_date,'YYYY-MM-DD')+4, ecosystem_subarea;
