# for file listing
import sqlite3 as sq,pdb,os,subprocess,re;
# to create rxs object, where order matters
from collections import OrderedDict;

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
functions
"""

# TODO: mdy2ymd

"""
From http://stackoverflow.com/a/480227/945039
By Markus Jaderot, January 26, 2009
Returns a set of unique list elements while preserving order
"""
def unq(seq):
    seen = set()
    seen_add = seen.add
    return [xx for xx in seq if not (xx in seen or seen_add(xx))]

