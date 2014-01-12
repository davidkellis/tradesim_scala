select * from corporate_actions
select * from securities where id = 284
select * from exchanges where id = 1957
select * from strategies
select * from trial_sets
select * from securities_trial_sets
select * from trials limit 1
delete from trials;

select *, 
       (cast(high as double precision) + cast(low as double precision))/2.0 simprice,
       (cast(high as double precision) + cast(low as double precision))/2.0 * 376 simtotal,
       (cast(high as double precision) + cast(low as double precision))/2.0 + 0.3 * (cast(high as double precision) - (cast(high as double precision) + cast(low as double precision))/2.0) buyfillprice,
       ((cast(high as double precision) + cast(low as double precision))/2.0 + 0.3 * (cast(high as double precision) - (cast(high as double precision) + cast(low as double precision))/2.0)) * 376 + 7 buyfilltotal,
       (cast(high as double precision) + cast(low as double precision))/2.0 + 0.3 * (cast(low as double precision) - (cast(high as double precision) + cast(low as double precision))/2.0) sellfillprice,
       ((cast(high as double precision) + cast(low as double precision))/2.0 + 0.3 * (cast(low as double precision) - (cast(high as double precision) + cast(low as double precision))/2.0)) * 376 - 7 sellfilltotal
from eod_bars where security_id = 284 order by start_time limit 260;
