--Code to automate 2022 ESP Satellite SST indicators within the AKFIN database
--Matt Callahan
--10/5/2022

--Spring_Temperature_Surface_WCGOA_Satellite
select extract(year from read_date) as indictator_year, 'Spring_Temperature_Surface_WCGOA_Satellite' as indicator_name, round(avg(temp),2) as data_value
from afsc.erddap_crw_sst a
inner join afsc.erddap_crw_sst_spatial_lookup b
on a.crw_id=b.id
--Season: Spring defined here as April and May
where extract(month from read_date) in (4,5)
--Area WCGOA defined as NMFS 610, 620, and 630
and nmfsarea in (610, 620, 630)
--Depth filter applied
and depth < -10
and depth >= -200
group by extract(year from read_date)
order by extract(year from read_date) asc;

--Spring_Temperature_Surface_SEBS_Satellite
select extract(year from read_date) as indicator_year, 'Spring_Temperature_Surface_SEBS_Satellite' as indicator_name, round(avg(temp),2) as data_value
from afsc.erddap_crw_sst a
inner join afsc.erddap_crw_sst_spatial_lookup b
on a.crw_id=b.id
--Season: Spring defined as May and June
where extract(month from read_date) in (5,6)
--Area: Southeastern Bering Sea ecosystem status report subregion
and ecosystem_sub='Southeastern Bering Sea'
--Depth deeper than -10m
and depth < -10
group by extract(year from read_date)
order by extract(year from read_date) asc;

--Spring_Summer_Temperature_Surface_SEBS_Satellite
select extract(year from read_date) as indictator_year, 'Spring_Summer_Temperature_Surface_SEBS_Satellite' as indicator_name, round(avg(temp),2) as data_value
from afsc.erddap_crw_sst a
inner join afsc.erddap_crw_sst_spatial_lookup b
on a.crw_id=b.id
where extract(month from read_date) in (4,5,6)
and ecosystem_sub='Southeastern Bering Sea'
and depth < -10
and depth > -200
group by extract(year from read_date)
order by extract(year from read_date) asc;

--Spring_Temperature_Surface_EGOA_Satellite
--Changed to Gulf wide (Formally NMFS 640 & 650, now 610 620 630 640 and 650)
--Change name to Spring_Temperature_Surface_GOA_Satellite?
select extract(year from read_date) as indicator_year, 'Spring_Temperature_Surface_EGOA_Satellite' as indicator_name, round(avg(temp),2) as data_value
--join sst and lookup tables
from afsc.erddap_crw_sst a
inner join afsc.erddap_crw_sst_spatial_lookup b
on a.crw_id=b.id
--Season: Spring definted as May and June
where extract(month from read_date) in (5,6)
--Area: GOA outside waters NMFS 640 and 650
and nmfsarea in (610, 620, 630, 640, 650)
--No depth filter
group by extract(year from read_date)
order by extract(year from read_date) asc;

