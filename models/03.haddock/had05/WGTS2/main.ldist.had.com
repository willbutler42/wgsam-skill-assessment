; main file for gadget - created in Rgadget
; WGTS2/main.ldist.had.com - Fri Apr 14 17:48:59 2023
timefile Modelfiles/time
areafile Modelfiles/area
printfiles ; no printfile supplied
[stock]
stockfiles had
[tagging]
[otherfood]
[fleet]
fleetfiles Modelfiles/fleet_had
[likelihood]
likelihoodfiles WGTS2/likelihood.ldist.had.com
