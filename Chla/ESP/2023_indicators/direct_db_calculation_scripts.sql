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
select extract(year from to_date(start_date,'YYYY-MM-DD')) year, 'AMJ_Chlorophylla_Biomass_SEBS_Satellite' as indicator_name, round(avg(chla),2) data_value
from globcolour_2023 a
left join globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')) in (4, 5, 6)
and bsierp_region_name in ('St. Matthew','Pribilofs')
group by extract(year from to_date(start_date,'YYYY-MM-DD'))
order by extract(year from to_date(start_date,'YYYY-MM-DD')) asc;

-- Spring_Chlorophylla_Biomass_SEBS_Inner_Shelf_Satellite
select extract(year from to_date(start_date,'YYYY-MM-DD')) year, 'Spring_Chlorophylla_Biomass_SEBS_Inner_Shelf_Satellite' as indicator_name, round(avg(chla),2) data_value
from globcolour_2023 a
left join globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')) in (4, 5, 6)
and bs_king='BBRKC'
group by extract(year from to_date(start_date,'YYYY-MM-DD'))
order by extract(year from to_date(start_date,'YYYY-MM-DD')) asc;

--Spring_Chlorophylla_Biomass_SMBKC_Satellite
select extract(year from to_date(start_date,'YYYY-MM-DD')) year, 'Spring_Chlorophylla_Biomass_SMBKC_Satellite' as indicator_name, round(avg(chla),2) data_value
from globcolour_2023 a
left join globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')) in (4, 5, 6)
and bs_king='StMattBKC'
group by extract(year from to_date(start_date,'YYYY-MM-DD'))
order by extract(year from to_date(start_date,'YYYY-MM-DD')) asc;

--Groundfish Indicators
--Spring_Chlorophylla_Biomass_GOA_Satellite
select extract(year from to_date(start_date,'YYYY-MM-DD')) year, 'Spring_Chlorophylla_Biomass_GOA_Satellite' as indicator_name, round(avg(chla),2) data_value
from globcolour_2023 a
left join globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')) in (4, 5, 6)
and NMFS_REP_AREA in ('610', '620', '630', '640', '650')
and depth <= (-10)
group by extract(year from to_date(start_date,'YYYY-MM-DD'))
order by extract(year from to_date(start_date,'YYYY-MM-DD')) asc;

--Spring_Chlorophylla_Biomass_WCGOA_Satellite
select extract(year from to_date(start_date,'YYYY-MM-DD')) year, 'Spring_Chlorophylla_Biomass_WCGOA_Satellite' as indicator_name, round(avg(chla),2) data_value, count(*) n_values
from globcolour_2023 a
left join globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')) = 5 
and NMFS_REP_AREA in ('610', '620', '630')
and depth <= (-10)
and depth > (-200)
group by extract(year from to_date(start_date,'YYYY-MM-DD'))
order by extract(year from to_date(start_date,'YYYY-MM-DD')) asc;

--ESR query
select round(avg(chla),2) meanchla, start_date, ecosystem_subarea
from env_data.globcolour_2023 a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')) in (4, 5, 6)
and ecosystem_area = ('Gulf of Alaska')
and waters_cod = 'FED'
and depth>(-200)
and depth<(-10)
group by start_date, ecosystem_subarea;

