#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("lubridate")
library('dplyr')
library('tidyverse')
library('lubridate')
df1<-read.csv(file = "INSITE report 20171024 (all tables).csv",header=TRUE, sep=",")

df <- df1 %>% dplyr:: select(-grep("units", names(df1)), -grep("Unit", names(df1)))

#colnames(df) <- c("ESN",	"Date",	"Timestamp",	"Engine Model",	"ECM Name",	"ECM code",	"CalibrationSoftwarePhase",	"VIN",	"PCID",	"advertised power at rpm",	"advertised power rpm",	"control parts list",	"customer location",	"customer name",	"engine build date",	"governed speed",	"low idle speed",	"oem name",	"oem vehicle or equipment model",	"peak torque at rpm",	"peak torque rpm",	"rear axle ratio",	"vehicle or equipment year",	"aftertreatment diesel particulate filter ash adjustment factor",	"ECM Time (Key On Time)",	"Engine Distance",	"engine hours",	"exhaust gas pressure",	"exhaust gas temperature (calculated)",	"exhaust mass flowrate",	"intake manifold air temperature",	"intake manifold pressure",	"Aftertreatment Diesel Exhaust Fluid Used",	"Average Engine Load",	"Average Engine Speed",	"Average Fuel Economy",	"Average Fuel Rate",	"Average Vehicle Speed",	"Coast Fuel Used",	"Coast Time",	"Cruise Control Fuel Used",	"Cruise Control Time",	"Drive Average Engine Speed",	"Drive Average Fuel Economy",	"Drive Average Load",	"Drive Average Power",	"Drive Fuel Used",	"Drive Time",	"ECM Time",	"ECM Time (Key On Time)",	"Engine Brake Activations",	"Engine Brake Distance",	"Engine Brake Time",	"Engine Brakes Percent Time",	"Engine Distance",	"Engine Run Time",	"Fan On Time",	"Fan Time Due To Engine Conditions",	"Fuel Consumed for Aftertreatment Injection",	"Fuel Used",	"Full Load Operation Time",	"Gear Down Distance",	"Gear Down Fuel Used",	"Gear Down Time",	"Idle Fuel Used",	"Idle Time",	"Keyswitch Cycles",	"Maximum Accelerator Vehicle Speed",	"Maximum Accelerator Vehicle Speed Distance",	"Maximum Accelerator Vehicle Speed Fuel Used",	"Maximum Accelerator Vehicle Speed Time",	"Maximum Diesel Particulate Filter Differential Pressure",	"Maximum Engine Speed",	"Maximum Vehicle Speed",	"Number of Complete Regenerations",	"Number of Idle Shutdowns",	"Number of Incomplete Regenerations",	"Number of Sudden Decelerations",	"Number Of Trip Resets",	"Out of Gear Coasts",	"Percent Trip Time At Idle",	"PTO Drive Fuel Used",	"PTO Drive Time",	"Service Brake Actuations",	"Service Brake Time",	"Top Gear Distance",	"Top Gear Fuel Used",	"Top Gear Time",	"Total PTO Time",	"Vehicle Overspeed 1 Fuel Used",	"Vehicle Overspeed 2 Fuel Used")

#names(df) <- gsub(" ", ".", names(df))

#df$perc.complete.regens <- 100*(df$Number.of.Complete.Regenerations/(df$Number.of.Complete.Regenerations+df$Number.of.Incomplete.Regenerations))

df$perc.complete.regens <- 100*(df$trip.values.Number.of.Complete.Regenerations/(df$trip.values.Number.of.Complete.Regenerations+df$trip.values.Number.of.Incomplete.Regenerations))

#df$perc.incomplete.regens <- 100*(df$Number.of.Incomplete.Regenerations/(df$Number.of.Complete.Regenerations+df$Number.of.Incomplete.Regenerations))
df$perc.incomplete.regens <-100*(df$trip.values.Number.of.Incomplete.Regenerations/(df$trip.values.Number.of.Complete.Regenerations+df$trip.values.Number.of.Incomplete.Regenerations))

#df$perc.def.used<- 100*(df$Aftertreatment.Diesel.Exhaust.Fluid.Used/df$Fuel.Used)
df$perc.def.used<- 100*(df$trip.values.Aftertreatment.Diesel.Exhaust.Fluid.Used/df$trip.values.Fuel.Used)

#df$perc.brake.distance <- 100*(df$Engine.Brake.Distance/df$Engine.Distance)
df$perc.brake.distance <- 100*(df$trip.values.Engine.Brake.Distance/df$trip.values.Engine.Distance)

#df$perc.gear.down.distance <- 100*(df$Gear.Down.Distance/df$Engine.Distance)
df$perc.gear.down.distance <- 100*(df$trip.values.Gear.Down.Distance/df$trip.values.Engine.Distance)

#df$perc.max.acc.veh.speed.distance <- 100*(df$Maximum.Accelerator.Vehicle.Speed.Distance/df$Engine.Distance)
df$perc.max.acc.veh.speed.distance <- 100*(df$trip.values.Maximum.Accelerator.Vehicle.Speed.Distance/df$trip.values.Engine.Distance)

#df$perc.top.gear.distance <- 100*(df$Top.Gear.Distance/df$Engine.Distance)
df$perc.top.gear.distance <- 100*(df$trip.values.Top.Gear.Distance/df$trip.values.Engine.Distance)

#df$perc.aft.fuel.consumed <- 100*(df$Fuel.Consumed.for.Aftertreatment.Injection/df$Fuel.Used)
df$perc.aft.fuel.consumed <- 100*(df$trip.values.Fuel.Consumed.for.Aftertreatment.Injection/df$trip.values.Fuel.Used)

#df$perc.coast.fuel.used <- 100*(df$Coast.Fuel.Used/df$Drive.Fuel.Used)
df$perc.coast.fuel.used <- 100*(df$trip.values.Coast.Fuel.Used/df$trip.values.Drive.Fuel.Used)

#df$perc.coast.fuel.used.wrt.total <- 100*(df$Coast.Fuel.Used/df$Fuel.Used)
df$perc.coast.fuel.used.wrt.total <- 100*(df$trip.values.Coast.Fuel.Used/df$trip.values.Fuel.Used)

#df$perc.drive.fuel.used <- 100*(df$Drive.Fuel.Used/df$Fuel.Used)
df$perc.drive.fuel.used <- 100*(df$trip.values.Drive.Fuel.Used/df$trip.values.Fuel.Used)

#df$perc.cruise.fuel.used<-100*(df$Cruise.Control.Fuel.Used/df$Drive.Fuel.Used)
df$perc.cruise.fuel.used<-100*(df$trip.values.Cruise.Control.Fuel.Used/df$trip.values.Drive.Fuel.Used)

#df$perc.cruise.fuel.used.wrt.total <- 100*(df$Cruise.Control.Fuel.Used/df$Fuel.Used)
df$perc.cruise.fuel.used.wrt.total <- 100*(df$trip.values.Cruise.Control.Fuel.Used/df$trip.values.Fuel.Used)

#df$perc.gear.down.fuel.used <- 100*(df$Gear.Down.Fuel.Used/df$Drive.Fuel.Used)
df$perc.gear.down.fuel.used <- 100*(df$trip.values.Gear.Down.Fuel.Used/df$trip.values.Drive.Fuel.Used)

#df$perc.gear.down.fuel.used.wrt.total <- 100*(df$Gear.Down.Fuel.Used/df$Fuel.Used)
df$perc.gear.down.fuel.used.wrt.total <- 100*(df$trip.values.Gear.Down.Fuel.Used/df$trip.values.Fuel.Used)

#df$perc.idle.fuel.used <- 100*(df$Idle.Fuel.Used/df$Fuel.Used)
df$perc.idle.fuel.used <- 100*(df$trip.values.Idle.Fuel.Used/df$trip.values.Fuel.Used)

#df$perc.max.acc.veh.speed.fuel.used <- 100*(df$Maximum.Accelerator.Vehicle.Speed.Fuel.Used/df$Drive.Fuel.Used)
df$perc.max.acc.veh.speed.fuel.used <- 100*(df$trip.values.Maximum.Accelerator.Vehicle.Speed.Fuel.Used/df$trip.values.Drive.Fuel.Used)

#df$perc.max.acc.veh.speed.fuel.used.wrt.total <- 100*(df$Maximum.Accelerator.Vehicle.Speed.Fuel.Used/df$Fuel.Used)
df$perc.max.acc.veh.speed.fuel.used.wrt.total <- 100*(df$trip.values.Maximum.Accelerator.Vehicle.Speed.Fuel.Used/df$trip.values.Fuel.Used)

#df$perc.pto.drive.fuel.used<-100*(df$PTO.Drive.Fuel.Used/df$Drive.Fuel.Used)
df$perc.pto.drive.fuel.used<-100*(df$trip.values.PTO.Drive.Fuel.Used/df$trip.values.Drive.Fuel.Used)

#df$perc.pto.drive.fuel.used.wrt.total<- 100*(df$PTO.Drive.Fuel.Used/df$Fuel.Used)
df$perc.pto.drive.fuel.used.wrt.total<- 100*(df$trip.values.PTO.Drive.Fuel.Used/df$trip.values.Fuel.Used)

##df$perc.pto.fuel.used <- 100*(df$PTO.Drive.Fuel.Used/df$Fuel.Used)

#df$perc.top.gear.fuel.used<- 100*(df$Top.Gear.Fuel.Used/df$Drive.Fuel.Used)
df$perc.top.gear.fuel.used<- 100*(df$trip.values.Top.Gear.Fuel.Used/df$trip.values.Drive.Fuel.Used)

#df$perc.top.gear.fuel.used.wrt.total<- 100*(df$Top.Gear.Fuel.Used/df$Fuel.Used)
df$perc.top.gear.fuel.used.wrt.total<- 100*(df$trip.values.Top.Gear.Fuel.Used/df$trip.values.Fuel.Used)

#df$perc.coast.time<- 100*(df$Coast.Time/df$Drive.Time)
df$perc.coast.time<- 100*(df$trip.values.Coast.Time/df$trip.values.Drive.Time)

#df$perc.coast.time.wrt.engineruntime<-100*(df$Coast.Time/df$Engine.Run.Time)
df$perc.coast.time.wrt.engineruntime<-100*(df$trip.values.Coast.Time/df$trip.values.Engine.Run.Time)

#df$perc.cruise.time <- 100*(df$Cruise.Control.Time/df$Drive.Time)
df$perc.cruise.time <- 100*(df$trip.values.Cruise.Control.Time/df$trip.values.Drive.Time)

#df$perc.cruise.time.wrt.enginetime <- 100*(df$Cruise.Control.Time/df$Engine.Run.Time)
df$perc.cruise.time.wrt.enginetime <- 100*(df$trip.values.Cruise.Control.Time/df$trip.values.Engine.Run.Time)

#df$perc.drive.time <- 100*(df$Drive.Time/df$Engine.Run.Time)
df$perc.drive.time <- 100*(df$trip.values.Drive.Time/df$trip.values.Engine.Run.Time)

#df$perc.drive.time.wrt.ecmruntime <- 100*(df$Drive.Time/df$`ECM.Time.(Key.On.Time)`)
df$perc.drive.time.wrt.ecmruntime <- 100*(df$trip.values.Drive.Time/df$trip.values.ECM.Time..Key.On.Time.)

#df$perc.engine.run.time <- 100*(df$Engine.Run.Time/df$`ECM.Time.(Key.On.Time)`)
df$perc.engine.run.time <- 100*(df$trip.values.Engine.Run.Time/df$trip.values.ECM.Time..Key.On.Time.)

#df$perc.fullload.operation.time <- 100*(df$Full.Load.Operation.Time/df$Drive.Time)
df$perc.fullload.operation.time <- 100*(df$trip.values.Full.Load.Operation.Time/df$trip.values.Drive.Time)

#df$perc.fullload.operation.time.wrt.eng.runtime <- 100*(df$Full.Load.Operation.Time/df$Engine.Run.Time)
df$perc.fullload.operation.time.wrt.eng.runtime <- 100*(df$trip.values.Full.Load.Operation.Time/df$trip.values.Engine.Run.Time)

#df$perc.gear.down.time<-100*(df$Gear.Down.Time/df$Drive.Time)
df$perc.gear.down.time<-100*(df$trip.values.Gear.Down.Time/df$trip.values.Drive.Time)

#df$perc.gear.down.time.wrt.engine.runtime <- 100*(df$Gear.Down.Time/df$Engine.Run.Time)
df$perc.gear.down.time.wrt.engine.runtime <- 100*(df$trip.values.Gear.Down.Time/df$trip.values.Engine.Run.Time)

#df$perc.idle.time <- 100*(df$Idle.Time/df$Engine.Run.Time)
df$perc.idle.time <- 100*(df$trip.values.Idle.Time/df$trip.values.Engine.Run.Time)

#df$perc.max.acc.veh.speed.time <- 100*(df$Maximum.Accelerator.Vehicle.Speed.Time/df$Drive.Time)
df$perc.max.acc.veh.speed.time <- 100*(df$trip.values.Maximum.Accelerator.Vehicle.Speed.Time/df$trip.values.Drive.Time)

#df$perc.max.acc.veh.speed.time.wrt.eng.runtime <- 100*(df$Maximum.Accelerator.Vehicle.Speed.Time/df$Engine.Run.Time)
df$perc.max.acc.veh.speed.time.wrt.eng.runtime <- 100*(df$trip.values.Maximum.Accelerator.Vehicle.Speed.Time/df$trip.values.Engine.Run.Time)

#df$perc.service.brake.time <- 100*(df$Service.Brake.Time/df$Drive.Time)
df$perc.service.brake.time <- 100*(df$trip.values.Service.Brake.Time/df$trip.values.Drive.Time)

#df$perc.service.brake.time.wrt.eng.runtime <- 100*(df$Service.Brake.Time/df$Engine.Run.Time)
df$perc.service.brake.time.wrt.eng.runtime <- 100*(df$trip.values.Service.Brake.Time/df$trip.values.Engine.Run.Time)

#df$perc.top.gear.time <- 100*(df$Top.Gear.Time/df$Drive.Time)
df$perc.top.gear.time <- 100*(df$trip.values.Top.Gear.Time/df$trip.values.Drive.Time)

#df$perc.top.gear.time.wrt.eng.runtime <- 100*(df$Top.Gear.Time/df$Engine.Run.Time)
df$perc.top.gear.time.wrt.eng.runtime <- 100*(df$trip.values.Top.Gear.Time/df$trip.values.Engine.Run.Time)

#df$perc.total.pto.time <- 100*(df$Total.PTO.Time/df$Engine.Run.Time)
df$perc.total.pto.time <- 100*(df$trip.values.Total.PTO.Time/df$trip.values.Engine.Run.Time)

#df$perc.pto.drive.time <- 100*(df$PTO.Drive.Time/df$Drive.Time)
df$perc.pto.drive.time <- 100*(df$trip.values.PTO.Drive.Time/df$trip.values.Drive.Time)

#df$Keyswitch.Cycles.per.mile<-(df$Keyswitch.Cycles/df$Engine.Distance)
df$Keyswitch.Cycles.per.mile<-(df$trip.values.Keyswitch.Cycles/df$trip.values.Engine.Distance)

#df$perc.fan.on.time <- 100*(df$Fan.On.Time/df$`ECM.Time.(Key.On.Time)`)
df$perc.fan.on.time <- 100*(df$trip.values.Fan.On.Time/df$trip.values.ECM.Time..Key.On.Time.)

#df$perc.fan.time.eng.conditions <- 100*(df$Fan.Time.Due.To.Engine.Conditions/df$`ECM.Time.(Key.On.Time)`)
df$perc.fan.time.eng.conditions <- 100*(df$trip.values.Fan.Time.Due.To.Engine.Conditions/df$trip.values.ECM.Time..Key.On.Time.)

#df$drive.FE.wrt.total.FE <- (df$Drive.Average.Fuel.Economy/df$Average.Fuel.Economy)
df$drive.FE.wrt.total.FE <- (df$trip.values.Drive.Average.Fuel.Economy/df$trip.values.Average.Fuel.Economy)

#df$avg.veh.speed.wrt.max.veh.speed <- (df$Average.Vehicle.Speed/df$Maximum.Vehicle.Speed)
df$avg.veh.speed.wrt.max.veh.speed <- (df$trip.values.Average.Vehicle.Speed/df$trip.values.Maximum.Vehicle.Speed)

write.csv(df, file = "imagedata_add_columns.csv", row.names = FALSE)



###Fixing the time format in the df

rm(list=(ls()[ls()!="df"]))

#df2 <- df %>% dplyr:: select(grep("Time", names(df)))

d<-df$dm.values.engine.hours
#need_modification <- grepl(pattern = ":",x = d)
#c<-d[need_modification==TRUE]
new<-data.frame(d)

library(lubridate)
res <- hms(d)        # format to 'hours:minutes:seconds'
new$hr<-(hour(res)*60 + minute(res))/60


time.processed <- function(d) {
  need_modification <- grepl(pattern = ":",x = d)
  c<-d[need_modification==TRUE]
  res <- hms(c)        # format to 'hours:minutes:seconds'
  new<-((hour(res)*60 + minute(res))/60)
  d[need_modification==TRUE] <- new
  return(new)
  
  y <- hour(hms(d[need_modification==TRUE]))*60+minute(hms(d[need_modification==TRUE]))*60
  

  
}

temp <- time.processed(d)

df$trip.values.ECM.Time..Key.On.Time.= as.POSIXct(df$trip.values.ECM.Time..Key.On.Time. , "%H:%M:%S") 

data <- apply(df2, 2, function(x) {x <- recode(((hour(hms(x))*60 + minute(hms(x)))/60),"1=777; 0=888"); x})

time.data<- apply(df2, MARGIN=2, FUN=function(x) time.processed(x))


indx<-need_modification==TRUE



d<-df$dm.values.ECM.Time..Key.On.Time.
need_modification <- grepl(pattern = ":",x = d)
c<-d[need_modification==TRUE]
res <- hms(c)        # format to 'hours:minutes:seconds'
new<-factor(((hour(res)*60 + minute(res))/60))
d[which(need_modification==TRUE)] <- new
new<-data.frame(d)
new$hr<-(hour(res)*60 + minute(res))/60

(hour(hms(d[which(need_modification==TRUE)]))*60+minute(hms(d[which(need_modification==TRUE)])))/60

a<-(hour(hms(d[which(grepl(pattern = ":",x = d)==TRUE)]))*60+minute(hms(d[which(grepl(pattern = ":",x = d)==TRUE)])))/60
b<-as.numeric(d[which(grepl(pattern = ":",x = d)==FALSE)])
