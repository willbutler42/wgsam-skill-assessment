; main file for gadget - created in Rgadget
; WGTS2/main.ldist.cod.com - Sat Apr 15 12:59:08 2023
timefile Modelfiles/time
areafile Modelfiles/area
printfiles ; no printfile supplied
[stock]
stockfiles cod
[tagging]
[otherfood]
[fleet]
fleetfiles Modelfiles/fleet_cod
[likelihood]
likelihoodfiles WGTS2/likelihood.ldist.cod.com
