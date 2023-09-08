--raw data
with new as (select ecosystem_sub, count (*) new_pixels
from afsc.erddap_crw_sst_spatial_lookup
group by ecosystem_sub),
old as (select ecosystem_sub, count (*) old_pixels
from afsc.erddap_crw_sst_spatial_lookup_20210810
group by ecosystem_sub),
mid as (select ecosystem_sub, count (*) mid_pixels
from afsc.erddap_crw_sst_spatial_lookup_20220412
group by ecosystem_sub)
select new.ecosystem_sub, new.new_pixels, mid.mid_pixels, old.old_pixels
from new
left join old on new.ecosystem_sub=old.ecosystem_sub
left join mid on new.ecosystem_sub=mid.ecosystem_sub;

--with depth <0
with new as (select ecosystem_sub, count (*) new_pixels
from afsc.erddap_crw_sst_spatial_lookup
where depth<0
group by ecosystem_sub),
old as (select ecosystem_sub, count (*) old_pixels
from afsc.erddap_crw_sst_spatial_lookup_20210810
where depth<0
group by ecosystem_sub),
mid as (select ecosystem_sub, count (*) mid_pixels
from afsc.erddap_crw_sst_spatial_lookup_20220412
where depth<0
group by ecosystem_sub)
select new.ecosystem_sub, new.new_pixels, mid.mid_pixels, old.old_pixels
from new
left join old on new.ecosystem_sub=old.ecosystem_sub
left join mid on new.ecosystem_sub=mid.ecosystem_sub;

--GOA with depth filter
with new as (select ecosystem_sub, count (*) new_pixels
from afsc.erddap_crw_sst_spatial_lookup
where ecosystem = 'Gulf of Alaska' and depth between -200 and -10
group by ecosystem_sub),
old as (select ecosystem_sub, count (*) old_pixels
from afsc.erddap_crw_sst_spatial_lookup_20210810
where ecosystem = 'Gulf of Alaska' and depth between -200 and -10
group by ecosystem_sub),
mid as (select ecosystem_sub, count (*) mid_pixels
from afsc.erddap_crw_sst_spatial_lookup_20220412
where ecosystem = 'Gulf of Alaska' and depth between -200 and -10
group by ecosystem_sub)
select new.ecosystem_sub, new.new_pixels, mid.mid_pixels, old.old_pixels
from new
left join old on new.ecosystem_sub=old.ecosystem_sub
left join mid on new.ecosystem_sub=mid.ecosystem_sub;

