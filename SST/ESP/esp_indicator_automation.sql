--Code to automate ESP Satellite SST indicators within the AKFIN database
--Matt Callahan
--6/9/2022

--Spring_Temperature_Surface_EGOA_Satellite
--query
--two columns: year and value
--decomissioned (?)
select extract(year from read_date) indictator_year, round(avg(temp),2) as mean_sst
--join sst and lookup tables
from afsc.erddap_crw_sst a
inner join afsc.erddap_crw_sst_spatial_lookup b
on a.crw_id=b.id
--Season: Spring definted as May and June
where extract(month from read_date) in (5,6)
--Area: EGOA defined as NMFS 640 and 650
and nmfsarea in (640, 650)
--No depth filter
group by extract(year from read_date)
order by extract(year from read_date) asc;

--compare to previous year
--2021 differs by 0.04, all others are within 0.01 :)
select
c.year,
c.new_sst,
d.data_value old_sst,
c.new_sst-d.data_value diff
from (
--same query as above
select extract(year from read_date) year, round(avg(temp),2) new_sst
from afsc.erddap_crw_sst a
inner join afsc.erddap_crw_sst_spatial_lookup b
on a.crw_id=b.id
where extract(month from read_date) in (5,6)
and nmfsarea in (640, 650)
group by extract(year from read_date)) c
--indicator output table from BYOD tool
left join afsc_host.spr_sst_egoa d
on c.year=d.year
order by c.year desc
;

--UPDATED GOA SST indicator
--Spring_Temperature_Surface_GOA_Satellite
select extract(year from read_date) indictator_year, round(avg(temp),2) as mean_sst
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

--Pull SST data for heatwave indicator
--this will be slow to run...       
select read_date, round(avg(temp),2) as mean_sst
--join sst and lookup tables
from afsc.erddap_crw_sst a
inner join afsc.erddap_crw_sst_spatial_lookup b
on a.crw_id=b.id
--Season: Spring definted as May and June
--where extract(month from read_date) in (5,6)
--Area: GOA outside waters NMFS 640 and 650
where nmfsarea in (610, 620, 630, 640, 650)
--No depth filter
group by read_date
order by read_date asc;


--Spring_Temperature_Surface_WCGOA_Satellite
select extract(year from read_date) indictator_year, round(avg(temp),2) as mean_sst
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

--COMPARE TO PREVIOUS
--No difference
select
c.year,
c.new_sst,
d.data_value old_sst,
c.new_sst-d.data_value diff
from (
--same query as above
select extract(year from read_date) year, round(avg(temp),2) new_sst
from afsc.erddap_crw_sst a
inner join afsc.erddap_crw_sst_spatial_lookup b
on a.crw_id=b.id
where extract(month from read_date) in (4,5)
and nmfsarea in (610, 620, 630)
and depth < -10
and depth >= -200
group by extract(year from read_date)) c
--indicator output table from BYOD tool
left join afsc_host.spr_sst_wcgoa d
on c.year=d.year
order by c.year desc
;


--Spring_Temperature_Surface_SEBS_Satellite
select extract(year from read_date) indictator_year, round(avg(temp),2) as mean_sst
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

--compare to previous
--2021 had 0.06 difference. All other differences were 0. Phew!
select
c.year,
c.new_sst,
d.data_value old_sst,
c.new_sst-d.data_value diff
from (
--same query as above
select extract(year from read_date) year, round(avg(temp),2) new_sst
from afsc.erddap_crw_sst a
inner join afsc.erddap_crw_sst_spatial_lookup b
on a.crw_id=b.id
where extract(month from read_date) in (5,6)
and ecosystem_sub='Southeastern Bering Sea'
and depth < -10
group by extract(year from read_date)) c
--indicator output table from BYOD tool
left join afsc_host.spr_sst_sebs d
on c.year=d.year
order by c.year desc
;

--Spring_Summer_Temperature_Surface_SEBS_Satellite
select extract(year from read_date) indictator_year, round(avg(temp),2) as mean_sst
from afsc.erddap_crw_sst a
inner join afsc.erddap_crw_sst_spatial_lookup b
on a.crw_id=b.id
where extract(month from read_date) in (4,5,6)
and ecosystem_sub='Southeastern Bering Sea'
and depth < -10
and depth > -200
group by extract(year from read_date)
order by extract(year from read_date) asc;

--compare to previous
--2021 was off by 0.05, the rest were 0.01 or less. 
select
c.year,
c.new_sst,
d.data_value old_sst,
c.new_sst-d.data_value diff
from (
select extract(year from read_date) year, round(avg(temp),2) new_sst
from afsc.erddap_crw_sst a
inner join afsc.erddap_crw_sst_spatial_lookup b
on a.crw_id=b.id
where extract(month from read_date) in (4,5,6)
and ecosystem_sub='Southeastern Bering Sea'
and depth < -10
and depth > -200
group by extract(year from read_date)) c
left join afsc_host.spr_sum_sst_sebs d
on c.year=d.year
order by c.year desc
;