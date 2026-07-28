--Crab
-- AMJ_Chlorophylla_Biomass_SEBS_Satellite
select extract(year from to_date(read_date,'YYYY-MM-DD')+4) year, 'AMJ_Chlorophylla_Biomass_SEBS_Satellite' as indicator_name, round(avg(chlorophyll),2) data_value
from env_data.OCCCI_CHLA a
left join env_data.OCCCI_SPATIAL_LOOKUP b on a.occci_id=b.occci_id
where extract(month from to_date(read_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and bsierp_name in ('St. Matthew','Pribilofs')
group by extract(year from to_date(read_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(read_date,'YYYY-MM-DD')+4) asc;

-- Spring_Chlorophylla_Biomass_SEBS_Inner_Shelf_Satellite
select extract(year from to_date(read_date,'YYYY-MM-DD')+4) year, 'Spring_Chlorophylla_Biomass_SEBS_Inner_Shelf_Satellite' as indicator_name, round(avg(chlorophyll),2) data_value
from env_data.OCCCI_CHLA a
left join env_data.OCCCI_SPATIAL_LOOKUP b on a.occci_id=b.occci_id
where extract(month from to_date(read_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and bbrkc='Bristol Bay RKC'
group by extract(year from to_date(read_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(read_date,'YYYY-MM-DD')+4) asc;

-- AMJ_Chlorophylla_Concentration_Tanner_OCCCI
-- dropping July since it will always be missing in current year
select extract(year from to_date(read_date,'YYYY-MM-DD')+4) year, 'AMJ_Chlorophylla_Concentration_Tanner_OCCCI' as indicator_name, round(avg(chlorophyll),2) data_value
from env_data.OCCCI_CHLA a
left join env_data.OCCCI_SPATIAL_LOOKUP b on a.occci_id=b.occci_id
where extract(month from to_date(read_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and bsierp_id in (3,4,5,6,8)
group by extract(year from to_date(read_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(read_date,'YYYY-MM-DD')+4) asc;

-- Groundfish
-- Spring_Chlorophylla_Biomass_GOA_Satellite
select extract(year from to_date(read_date,'YYYY-MM-DD')+4) year, 'Spring_Chlorophylla_Biomass_GOA_Satellite' as indicator_name, round(avg(chlorophyll),2) data_value
from env_data.occci_chla a
left join env_data.occci_spatial_lookup b on a.occci_id=b.occci_id
where extract(month from to_date(read_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and NMFS_AREA in ('610', '620', '630', '640', '650')
and depth <= (-10)
group by extract(year from to_date(read_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(read_date,'YYYY-MM-DD')+4) asc;

-- Spring_Chlorophylla_Peak_GOA_Satellite
WITH ranked_data AS (
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
and NMFS_AREA in ('610', '620', '630', '640', '650')
and depth <= (-10)
        group by  extract(year from to_date(read_date,'YYYY-MM-DD')+4),
        to_number(to_char(to_date(read_date,'YYYY-MM-DD')+4,'DDD'))
))
SELECT
    year,
    'Spring_Chlorophylla_Peak_GOA_Satellite' as indicator_name,
    doy data_value
FROM ranked_data
WHERE rn = 1;

-- Spring_Chlorophylla_Biomass_WCGOA_Satellite
select extract(year from to_date(read_date,'YYYY-MM-DD')+4) year, 'Spring_Chlorophylla_Biomass_WCGOA_Satellite' as indicator_name, round(avg(chlorophyll),2) data_value, count(*) n_values
from env_data.occci_chla a
left join env_data.occci_spatial_lookup b on a.occci_id=b.occci_id
where extract(month from to_date(read_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and NMFS_AREA in ('610', '620', '630')
and depth <= (-10)
and depth > (-200)
group by extract(year from to_date(read_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(read_date,'YYYY-MM-DD')+4) asc;

-- Spring_Chlorophylla_Peak_WCGOA_Satellite
WITH ranked_data AS (
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
and NMFS_AREA in ('610', '620', '630')
        and depth <= (-10)
        and depth >= (-200)
        group by  extract(year from to_date(read_date,'YYYY-MM-DD')+4),
        to_number(to_char(to_date(read_date,'YYYY-MM-DD')+4,'DDD'))
))
SELECT
    year,
    'Spring_Chlorophylla_Peak_WCGOA_Satellite' as indicator_name,
    doy data_value
FROM ranked_data
WHERE rn = 1;
