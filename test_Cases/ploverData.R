plover<-read.csv("ncPipingPlover.csv")
plot(plover$NumberByPartyHours~plover$Count_yr)

plover_bkpts = bai_perron(plover$Count_yr, plover$NumberByPartyHours, "ar", "order=1", 4, 0.1, 3)