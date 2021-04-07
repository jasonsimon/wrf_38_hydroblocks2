# jss all of the stuff i've added


state    real  shread0              ij      misc        1         -      rh       "shread0"                   "sh in file 0"                      "W m-2"      
state    real  shread1              ij      misc        1         -      rh       "shread1"                   "sh in file 1"                      "W m-2"      
state    real  lhread0              ij      misc        1         -      rh       "lhread0"                   "lh in file 0"                      "W m-2"      
state    real  lhread1              ij      misc        1         -      rh       "lhread1"                   "lh in file 1"                      "W m-2"      
state    real  lwread0              ij      misc        1         -      rh       "lwread0"                   "lw up in file 0"                      "W m-2"      
state    real  lwread1              ij      misc        1         -      rh       "lwread1"                   "lw up in file 1"                      "W m-2"      
state    real  cdread0              ij      misc        1         -      rh       "cdread0"                   "cd in file 0"                      "-"      
state    real  cdread1              ij      misc        1         -      rh       "cdread1"                   "cd in file 1"                      "-"      
state    real  alread0              ij      misc        1         -      rh       "alread0"                   "albedo in file 0"                      "-"      
state    real  alread1              ij      misc        1         -      rh       "alread1"                   "albedo in file 1"                      "-"      
state    real  emread0              ij      misc        1         -      rh       "emread0"                   "emissivity in file 0"                      "-"      
state    real  emread1              ij      misc        1         -      rh       "emread1"                   "emissivity in file 1"                      "-"      

state    real  lwuphb               ij      misc        1         -      rh       "lwuphb"                   "surface LW up in HydroBlocks"                      "-"      


rconfig   integer   jss_moistopt       namelist,dynamics     1               0    h    "" ""
rconfig   integer   jss_heatopt        namelist,dynamics     1               0    h    "" ""
rconfig   integer   jss_dragopt        namelist,dynamics     1               0    h    "" ""

rconfig   integer   jss_atmwrap        namelist,dynamics     1               0    h    "" ""

rconfig   integer   jss_sfcwrap        namelist,dynamics     1               0    h    "" "wrap cd, hfx and qfx (and lh)"
rconfig   integer   jss_landwrap       namelist,dynamics     1               0    h    "" "wrap albedo, emiss, and tsk"

rconfig   integer   jss_starthour        namelist,dynamics     1               0    h    "" ""
rconfig   integer   jss_startday         namelist,dynamics     1               0    h    "" ""
rconfig   integer   jss_startmonth       namelist,dynamics     1               0    h    "" ""

rconfig   real      jss_minhfx         namelist,dynamics     1               0    h    "" ""
rconfig   real      jss_minwind        namelist,dynamics     1               0    h    "" ""

rconfig   integer   jss_altiso         namelist,dynamics     1               0    h    "" ""
rconfig   integer   jss_kv31           namelist,dynamics     1               0    h    "" ""

rconfig   integer   jss_crmcor         namelist,dynamics     1               0    h    "" ""
rconfig   integer   jss_fulllat         namelist,dynamics     1               0    h    "" ""

rconfig   real    jss_qfx_hardval      namelist,dynamics     1               0    h    "general debugging real" ""
rconfig   real    jss_hfx_hardval      namelist,dynamics     1               0    h    "general debugging real" ""
rconfig   real    jss_cd0_hardval      namelist,dynamics     1               0    h    "general debugging real" ""

rconfig   real    jss_debug_r1         namelist,dynamics     1               0    h    "general debugging real" ""
rconfig   real    jss_debug_r2         namelist,dynamics     1               0    h    "general debugging real" ""
rconfig   real    jss_debug_r3         namelist,dynamics     1               0    h    "general debugging real" ""
rconfig   real    jss_debug_r4         namelist,dynamics     1               0    h    "general debugging real" ""
rconfig   real    jss_debug_r5         namelist,dynamics     1               0    h    "general debugging real" ""
rconfig   real    jss_debug_r6         namelist,dynamics     1               0    h    "general debugging real" ""
rconfig   real    jss_debug_r7         namelist,dynamics     1               0    h    "general debugging real" ""
rconfig   real    jss_debug_r8         namelist,dynamics     1               0    h    "general debugging real" ""

rconfig   integer jss_debug_i1         namelist,dynamics     1               0    h    "general debugging int" ""
rconfig   integer jss_debug_i2         namelist,dynamics     1               0    h    "general debugging int" ""
rconfig   integer jss_debug_i3         namelist,dynamics     1               0    h    "general debugging int" ""
rconfig   integer jss_debug_i4         namelist,dynamics     1               0    h    "general debugging int" ""
rconfig   integer jss_debug_i5         namelist,dynamics     1               0    h    "general debugging int" ""
rconfig   integer jss_debug_i6         namelist,dynamics     1               0    h    "general debugging int" ""
rconfig   integer jss_debug_i7         namelist,dynamics     1               0    h    "general debugging int" ""
rconfig   integer jss_debug_i8         namelist,dynamics     1               0    h    "general debugging int" ""
