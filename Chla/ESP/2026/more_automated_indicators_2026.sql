-- Automated SEBS chla indicators.
-- 10/15/2026
-- Inheritied contributions from Jens Nielsen. These were not reproducible as the product had changed since they were last calculated.
-- At the request of the ESP team (Kalei) I redesigned them to match the GOA Chla depth filters and the SEBS SST spatial extents.
-- Note that these do not include the intermediate averaging that we do for the ESR and I think Jens used to do for these.
-- The effect on biomass is slight, but some years have much different peak bloom timing, as this methodology may affect which of multiple blooms is strongest. 


--Spring_Chlorophylla_Biomass_SEBS_Satellite
--	Here we use ocean color data, a measure of surface phytoplankton concentration, obtained from the European Space agency Climate Change Initiative Ocean Colour Product (OC-CCI) and covering the years 1998-2026. OC-CCI is a merged product that combines information from multiple sensors, using 8-day composites at a 4km x 4km resolution. Data were accessed via the AKFIN database. We estimate the phytoplankton biomass in the surface layer via average spring (Apr--Jun)chlorophyll-a concentrations (Chl-a, ng mL -1 ) for the Southeastern Bering Sea ESR subregion. Areas with bottom depth shallower than 10m are excluded.
select extract(year from to_date(read_date,'YYYY-MM-DD')+4) year, 
'Spring_Chlorophylla_Biomass_SEBS_Satellite' as indicator_name, round(avg(chlorophyll),2) indicator_value
from env_data.occci_chla a
left join env_data.occci_spatial_lookup b on a.occci_id=b.occci_id
where extract(month from to_date(read_date,'YYYY-MM-DD')+4) in (4, 5, 6)
AND b.ecosystem_subarea = 'Southeastern Bering Sea'
AND b.depth <= -10
group by extract(year from to_date(read_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(read_date,'YYYY-MM-DD')+4) asc;

--Spring_Chlorophylla_Peak_SEBS_Satellite
--	Here we use ocean color data, a measure of surface phytoplankton concentration, obtained from the European Space agency Climate Change Initiative Ocean Colour Product (OC-CCI) and covering the years 1998-2026. OC-CCI is a merged product that combines information from multiple sensors, using 8-day composites at a 4km x 4km resolution. Data were accessed via the AKFIN database. We estimate the phytoplankton biomass in the surface layer via average spring (Apr--Jun) peak timing of the spring bloom for the Southeastern Bering Sea ESR subregion. Areas with bottom depth shallower than 10m are excluded.
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
AND b.ecosystem_subarea = 'Southeastern Bering Sea'
      AND b.depth <= -10
        group by  extract(year from to_date(read_date,'YYYY-MM-DD')+4),
        to_number(to_char(to_date(read_date,'YYYY-MM-DD')+4,'DDD'))
))
SELECT
    year,
    'Spring_Chlorophylla_Peak_SEBS_Satellite' as indicator_name,
    doy indicator_value
FROM ranked_data
WHERE rn = 1;
-- Spring_Chlorophylla_Peak_SEBS_Satellite_Pacific_Cod
-- Here we use ocean color data, a measure of surface phytoplankton concentration, obtained from the European Space agency Climate Change Initiative Ocean Colour Product (OC-CCI) and covering the years 1998-2026. OC-CCI is a merged product that combines information from multiple sensors, using 8-day composites at a 4km x 4km resolution. Data were accessed via the AKFIN database. We estimate the phytoplankton biomass in the surface layer via average spring (Apr--Jun) peak timing of the spring bloom for the Southeastern Bering Sea ESR subregion. Areas with bottom depth shallower than 10m and deeper than 200m are excluded.
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
AND b.ecosystem_subarea = 'Southeastern Bering Sea'
      AND b.depth <= -10
      AND b.depth >= -200
        group by  extract(year from to_date(read_date,'YYYY-MM-DD')+4),
        to_number(to_char(to_date(read_date,'YYYY-MM-DD')+4,'DDD'))
))
SELECT
    year,
    'Spring_Chlorophylla_Peak_SEBS_Satellite_Pacific_Cod' as indicator_name,
    doy indicator_value
FROM ranked_data
WHERE rn = 1;