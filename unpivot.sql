select
	a."year", 
	b.*
from data1 a
	cross join lateral (
		values
			(c."Amanda", 'Amanda'),
			(c."Ashley", 'Ashley'),
			(c."Betty", 'Betty'),
			(c."Deborah", 'Deborah')
		) as b("spend", "name");
		
		
select
	"year"
	unnest(
		array[
			'Amanda', 
			'Ashley', 
			'Betty', 
			'Deborah']
		) as "name",
	unnest(
		array[
			"Amanda", 
			"Ashley",
			"Betty",
			"Deborah"]
		) as "spend"
from data1;


select *
from
(
  select "year", "Amanda" as value, 'Amanda' as name
  from "data1"
  union all
  select "year", "Ashley" as value, 'Ashley' as name
  from "data1"
  union all
  select "year", "Betty" as value, 'Betty' as name
  from "data1"
  union all
  select "year", "Deborah" as value, 'Deborah' as name
  from "data1"
) src;