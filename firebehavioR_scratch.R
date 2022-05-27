# testing out firebehavioR
# 5/6/22

library(firebehavioR)

fc = fireChart('fire',hpua = 15000, ros = 50)
print(fc)

#Example using RAWS meteorological station data
data(rrRAWS)
rrRAWS.daily =   rrRAWS[format(strptime(rrRAWS$dateTime, "%m/%d/%Y %H:%M"), "%H:%M")=="14:35",]
indices<-fireIndex(temp=rrRAWS.daily$temp_c, u= rrRAWS.daily$windSpeed_kmh, rh = rrRAWS.daily$rh)
