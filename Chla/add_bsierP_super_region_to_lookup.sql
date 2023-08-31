--add bsierp super region to globcolour lookup table

ALTER TABLE env_data.globcolour_spatial_lookup
add bsierp_super_region varchar2(38);


UPDATE env_data.globcolour_spatial_lookup
SET bsierp_super_region = CASE
    WHEN bsierp_region_name in ('AK peninsula', 'South middle shelf', 'Pribilofs', 'Central middle shelf') THEN 'South middle shelf'
    WHEN bsierp_region_name in ('South inner shelf', 'South inner shelf') THEN 'South inner shelf'
    WHEN bsierp_region_name = 'South outer shelf' THEN 'South outer shelf'
    WHEN bsierp_region_name = 'Central inner shelf' THEN 'Central inner shelf'
    WHEN bsierp_region_name = 'North outer shelf' THEN 'North outer shelf'
    WHEN bsierp_region_name in ('St. Matthew', 'North middle shelf') THEN 'North middle shelf'
    WHEN bsierp_region_name = 'North inner shelf' THEN 'North inner shelf'
    WHEN bsierp_region_name in ('St. Lawrence',  'South Bering Strait') THEN 'Bering Strait & St Lawrence'
    WHEN bsierp_region_name = 'Norton Sound' THEN 'Norton Sound'
    WHEN bsierp_region_name in ('Off-shelf north', 'Off-shelf southeast', 'Off-shelf north (E of 180)') THEN 'Offshelf'
    ELSE NULL
  END;
  
select * from env_data.globcolour_spatial_lookup
where bsierp_super_region is not null;

select distinct(bsierp_super_region) from env_data.globcolour_spatial_lookup;

select distinct(bsierp_region_name), bsierp_super_region 
from env_data.globcolour_spatial_lookup
order by bsierp_super_region;

commit;



