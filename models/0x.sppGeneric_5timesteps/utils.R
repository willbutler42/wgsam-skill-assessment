## convenience functions
von_b_formula <- function(a,linf='Linf',k='k',recl='recl'){
  a %>% 
    map(~infuser::infuse("{{linf}} * (1 - exp(-1 * (0.01 * {{k}}) * ({{a}} - (1 + log(1 - {{recl}}/{{linf}})/(0.01 * {{k}})))))",
                         a=.,linf=linf,k=k,recl=recl)) %>% 
    map(~parse(text=.) %>% 
          map(to.gadget.formulae)) %>% 
    unlist()
}

init_guess <- function(dat,pattern, value,  lower, upper, optimise){
  dat[grepl(pattern,dat$switch),'value'] <- value
  dat[grepl(pattern,dat$switch),'upper'] <- upper
  dat[grepl(pattern,dat$switch),'lower'] <- lower
  dat[grepl(pattern,dat$switch),'optimise'] <- optimise
  return(dat)
}

ageList <- list(
    "WHB" = mfdb_group('age0'=0,'age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8,'age9'=9,'age10'=10),
    "CAP" = mfdb_group('age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5),
    ## "CAP" = mfdb_group('age0'=0,'age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5),
    "GLH" = mfdb_group('age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8,'age9'=9,'age10'=10,
                       'age11'=11,'age12'=12,'age13'=13,'age14'=14,'age15'=15,'age16'=16,'age17'=17,'age18'=18,'age19'=19,'age20'=20),
    "HAD" = mfdb_group('age0'=0,'age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8,'age9'=9,'age10'=10,
                       'age11'=11,'age12'=12,'age13'=13,'age14'=14,'age15'=15,'age16'=16,'age17'=17,'age18'=18,'age19'=19,'age20'=20),
    "PLA" = mfdb_group('age0'=0,'age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8,'age9'=9,'age10'=10,
                       'age11'=11,'age12'=12,'age13'=13,'age14'=14,'age15'=15,'age16'=16,'age17'=17,'age18'=18,'age19'=19,'age20'=20),
    "MAC" = mfdb_group('age0'=0,'age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8,'age9'=9,'age10'=10,
                       'age11'=11,'age12'=12,'age13'=13,'age14'=14,'age15'=15,'age16'=16,'age17'=17,'age18'=18,'age19'=19,'age20'=20),
    "COD" = mfdb_group('age0'=0,'age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8,'age9'=9,'age10'=10,
                       'age11'=11,'age12'=12,'age13'=13,'age14'=14,'age15'=15,'age16'=16,'age17'=17,'age18'=18,'age19'=19,'age20'=20),
    "POC" = mfdb_group('age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8,'age9'=9,'age10'=10,
                       'age11'=11,'age12'=12,'age13'=13,'age14'=14,'age15'=15,'age16'=16,'age17'=17,'age18'=18,'age19'=19,'age20'=20),
    "RED" = mfdb_group('age0'=0,'age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8,'age9'=9,'age10'=10,
                       'age11'=11,'age12'=12,'age13'=13,'age14'=14,'age15'=15,'age16'=16,'age17'=17,'age18'=18,'age19'=19,'age20'=20,
                       'age21'=21,'age22'=22,'age23'=23,'age24'=24,'age25'=25,'age26'=26,'age27'=27,'age28'=28,'age29'=29,'age30'=30,
                       'age31'=31,'age32'=32,'age33'=33,'age34'=34,'age35'=35,'age36'=36,'age37'=37,'age38'=38,'age39'=39,'age40'=40),
    "SAI" = mfdb_group('age0'=0,'age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8,'age9'=9,'age10'=10,
                       'age11'=11,'age12'=12,'age13'=13,'age14'=14,'age15'=15,'age16'=16,'age17'=17,'age18'=18,'age19'=19,'age20'=20),
    "HER" = mfdb_group('age0'=0,'age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8,'age9'=9,'age10'=10,
                       'age11'=11,'age12'=12,'age13'=13,'age14'=14,'age15'=15,'age16'=16,'age17'=17,'age18'=18,'age19'=19,'age20'=20)
)

ageClsList <- list(
    "WHB" = mfdb_group('age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8,'age9'=9,'age10'=10),    
    "CAP" = mfdb_group('age1'=1,'age2'=2,'age3'=3,'age4'=4),
    "GLH" = mfdb_group('age1'=1:2,'age3'=3:4,'age5'=5:6,'age7'=7:8,'age9'=9:10,'age11'=11:12,'age13'=13:14,'age15'=15:16,'age17'=17:18,'age19'=19:20),
    "HAD" = mfdb_group('age1'=1:2,'age3'=3:4,'age5'=5:6,'age7'=7:8,'age9'=9:10,'age11'=11:12,'age13'=13:14,'age15'=15:16,'age17'=17:18,'age19'=19:20),
    "PLA" = mfdb_group('age1'=1:2,'age3'=3:4,'age5'=5:6,'age7'=7:8,'age9'=9:10,'age11'=11:12,'age13'=13:14,'age15'=15:16,'age17'=17:18,'age19'=19:20),
    "MAC" = mfdb_group('age1'=1:2,'age3'=3:4,'age5'=5:6,'age7'=7:8,'age9'=9:10,'age11'=11:12,'age13'=13:14,'age15'=15:16,'age17'=17:18,'age19'=19:20),
    "COD" = mfdb_group('age1'=1:2,'age3'=3:4,'age5'=5:6,'age7'=7:8,'age9'=9:10,'age11'=11:12,'age13'=13:14,'age15'=15:16,'age17'=17:18,'age19'=19:20),
    "POC" = mfdb_group('age1'=1:2,'age3'=3:4,'age5'=5:6,'age7'=7:8,'age9'=9:10,'age11'=11:12,'age13'=13:14,'age15'=15:16,'age17'=17:18,'age19'=19:20),
    "RED" =  mfdb_group('age3'=1:4,'age7'=5:8,'age11'=9:12,'age15'=13:16,'age19'=17:20,'age23'=21:24,'age27'=25:28,'age31'=29:32,'age35'=33:36,'age39'=37:40),
    "SAI" = mfdb_group('age1'=1:2,'age3'=3:4,'age5'=5:6,'age7'=7:8,'age9'=9:10,'age11'=11:12,'age13'=13:14,'age15'=15:16,'age17'=17:18,'age19'=19:20),
    "HER" = mfdb_group('age1'=1:2,'age3'=3:4,'age5'=5:6,'age7'=7:8,'age9'=9:10,'age11'=11:12,'age13'=13:14,'age15'=15:16,'age17'=17:18,'age19'=19:20)
)
