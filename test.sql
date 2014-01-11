select * from trials;
delete from trials;

select *, 
       (cast(high as double precision) + cast(low as double precision))/2.0 simprice,
       (cast(high as double precision) + cast(low as double precision))/2.0 * 376 simtotal,
       (cast(high as double precision) + cast(low as double precision))/2.0 + 0.3 * (cast(high as double precision) - (cast(high as double precision) + cast(low as double precision))/2.0) fillprice,
       ((cast(high as double precision) + cast(low as double precision))/2.0 + 0.3 * (cast(high as double precision) - (cast(high as double precision) + cast(low as double precision))/2.0)) * 376 + 7 filltotal
from eod_bars where security_id = 27136 order by start_time limit 10;
