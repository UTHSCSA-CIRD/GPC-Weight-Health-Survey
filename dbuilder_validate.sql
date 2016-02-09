-- existance of tables and uniqueness of their contents
select 'MODIFIER_DIMENSION' tbl, count(*) N, count(distinct modifier_cd) `N Distinct` from modifier_dimension
union all
select 'CONCEPT_DIMENSION' tbl, count(*) N, count(distinct concept_cd) `N Distinct` from concept_dimension
union all
select 'PATIENT_DIMENSION' tbl, count(*) N, count(distinct patient_num) `N Distinct` from patient_dimension
union all
select 'OBSERVATION_FACT' tbl, count(*) N, '' `N Distinct` from observation_fact
union all
select 'VARIABLE' tbl, count(*) N, count(distinct item_key) from variable
;

select '' `   `;
-- referntial integrity of tables
select 'Patients that have observations' ``, count(*) `` from
patient_dimension where patient_num in (select patient_num from observation_fact)
union all
select 'Observations that have patients' ``, count(*) `` from 
observation_fact where patient_num in (select patient_num from patient_dimension)
;


select '' `   `;
-- how many useless data elements are included
select count(*) `Data elements redundant with selection criteria` 
from variable where 
lower(concept_path) like '%bmi%' or
lower(concept_path) like '%eight%' or
lower(concept_path) like '%age%' or
lower(concept_path) like '%gender%' or
lower(concept_path) like '%sex%' or
lower(concept_path) like '%vital%'
;

-- the create/drop of tmptab below is because SQLite didn't support 
-- WITH clauses until version 3.8.3

select 'Non redundant data elements' `   `;

create temporary table tmptab as 
select concept_path,name_char from variable where 
lower(concept_path||name_char) not like '%bmi%' and
lower(concept_path||name_char) not like '%eight%' and
lower(concept_path||name_char) not like '%age%' and
lower(concept_path||name_char) not like '%gender%' and
lower(concept_path||name_char) not like '%sex%' and
lower(concept_path||name_char) not like '%vital%'
;

select * from tmptab
union all
select 'TOTAL ELEMENTS' concept_path, count(*) name_char from tmptab
;

drop table tmptab;

select 'Non redundant data elements actually found in OBSERVATION_FACT' `   `;

create temporary table tmptab as 
select v.concept_path,v.name_char from variable v join concept_dimension cd
on v.concept_path = cd.concept_path
where concept_cd in (select concept_cd from concept_dimension) and
lower(v.concept_path||v.name_char) not like '%bmi%' and
lower(v.concept_path||v.name_char) not like '%eight%' and
lower(v.concept_path||v.name_char) not like '%age%' and
lower(v.concept_path||v.name_char) not like '%gender%' and
lower(v.concept_path||v.name_char) not like '%sex%' and
lower(v.concept_path||v.name_char) not like '%vital%'
;

select * from tmptab
union all
select 'TOTAL ELEMENTS' concept_path, count(*) name_char from tmptab
;

drop table tmptab;


