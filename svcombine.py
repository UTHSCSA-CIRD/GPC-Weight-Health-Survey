# for file listing
import sqlite3 as sq,pdb,os,subprocess,re;
# to create rxs object, where order matters
from collections import OrderedDict;

# TODO: move the shared definitions to a separate file
# TODO: simple way to distinguish non-respondents, respondents, and long-form respondents
# TODO: MDY -> YMD for MCRF
# TODO: examine the unique text values in fields that have them, insure no Identifiers
# TODO: .csv file with the svconsensus columns and site indicator
# TODO: import into R, examine

"""
Stuff to move to a config file if we keep using this script
"""

# where data dictionary files are to be found
dddir = '.';
# match repsective file patterns
ddmatch = '_dd.csv'; svmatch = '_survey.csv'; dbmatch = '.db';
# replace data dictionary string to get site name
ddnmrep = ddmatch; svnmrep = svmatch; dbnmrep = dbmatch;
# prefixs for sqlite data dictionary and survey tables
ddprfx = 'dd_'; svprfx = 'sv_';
# name of sqlite script created
ddsqlscr = 'dd_sqlscript.sql';
pthddsqlscr = dddir+"/"+ddsqlscr;
# name of sqlite output database
ddsqldb = 'ddcheck.db';
pthddsqldb = dddir+"/"+ddsqldb;
# REDcap columns that need to be used
rccols = ["`Variable / Field Name`","`Field Type`"
,"`Field Label`","`Choices, Calculations, OR Slider Labels`"
,"`Field Note`","`Text Validation Type OR Show Slider Number`"
,"`Identifier?`","`Branching Logic (Show field only if...)`"];

"""
These are the fields that are consistent between site surveys in every way 
we could find so far
"""
svconsensus = ['bloodsugar_diabetes_self','bp_hypten_self','cancer_anytype','cancer_anytype_self','children_in_home'
	       ,'children_research','chole_trig_hyperlip','chole_triigl_hyperlip_self','deid_data','education'
	       ,'elev_bs_diabetes','height_req','household','hpb_hprtnsn','income','insurance','invite_response_nature'
	       ,'language','latino_origin','possible_research','preferred_contact_method','race','res_talk_family'
	       ,'research','research_accept_dec_child','research_accept_decisions','research_feeling','sex'
	       ,'survey_contact_method','weight_req','age','city','cmp_address','comment_1','comment_2','comment_3'
	       ,'contact_email','contact_fname','contact_fullname','contact_lname','contact_type','family_id'
	       ,'first_con_atmpt_res','height_feet','height_in','height_value_cm','invite_date1','invite_date2'
	       ,'invite_date3','invite_response_date','kic_note_date','mailaddress','mailaddress2','match_type'
	       ,'other_insurance','other_language','other_race','other_schooling','other_sex','pat_age','pat_bmi_pct'
	       ,'pat_bmi_raw','pat_sex','patient_num','proj_bin','proj_id','proj_id_number','q6_ans6_response'
	       ,'q7_ans6_response','record_id','res_dep_why2_child','research_depends_why','research_types2_child'
	       ,'research_types_adult','state','survey_access_code','survey_complete_date','survey_contact_person'
	       ,'survey_date1','survey_date2','survey_date3','survey_link','usps_address','wave','weight_value_kg'
	       ,'weight_value_lbs','zipcode'];

"""
regexps
"""
rxs = OrderedDict([
  ('Empty' , '^\s{0,}$'),
  ('Digit' , '^[0-9]$'),
  ('Numbr' , '^[0-9,. ]{1,}$'),
  ('YMD'   , '^201[0-9]-[0-9]{2}-[0-9]{2}$'),
  ('MDY'   , '^[0-9]{2}-[0-9]{2}-201[0-9]$'),
  ('YMDTS' , '^201[0-9]-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$'),
  ('MDYTS' , '^[0-9]{2}-[0-9]{2}-201[0-9] [0-9]{2}:[0-9]{2}:[0-9]{2}$'),
  ('Text'  , '^.*$')
  ]);

"""
rxoneint = '^[0-9]$'; rxbignum = '^[0-9,. ]{1,}$';
rxymd = '^201[0-9]-[0-9]{2}-[0-9]{2}$';
rxmdy = '^[0-9]{2}-[0-9]{2}-201[0-9]$';
rxymdts = '^201[0-9]-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$';
rxmdyts = '^[0-9]{2}-[0-9]{2}-201[0-9] [0-9]{2}:[0-9]{2}:[0-9]{2}$';
rxempty = '^\s{0,}$';
"""
"""
SQL UDFs
"""
class typect:
  def __init__(self):
    self.xxvals = {};
  def step(self,xx):
    if xx is None: xx = '';
    mtype=[ii for ii in rxs.keys() if re.compile(rxs[ii]).match(xx) is not None][0];
    if mtype in self.xxvals.keys(): self.xxvals[mtype] += 1
    else: self.xxvals[mtype] = 1;
  def finalize(self):
    return (str(self.xxvals)[1:-1]).replace(' ','').replace("'",'');
  
class valct:
  def __init__(self):
    self.xxvals = {};
  def step(self,xx):
    if xx is None: xx = 'None'
    else: xx = str(xx).lower();
    if xx in self.xxvals.keys(): self.xxvals[xx] += 1
    else: self.xxvals[xx] = 1;
  def finalize(self):
    return (str(sorted(self.xxvals.items(), key=lambda tt: tt[1]))).replace("','",':').replace("(",'').replace(")",'').replace("'",'').replace(' ','')
  

"""
    if 'aa' not in foo.keys(): foo['aa'] = 1
... else: foo['aa'] += 1
"""

# mdy2ymd

"""
From http://stackoverflow.com/a/480227/945039
By Markus Jaderot, January 26, 2009
Returns a set of unique list elements while preserving order
"""
def unq(seq):
    seen = set()
    seen_add = seen.add
    return [xx for xx in seq if not (xx in seen or seen_add(xx))]
