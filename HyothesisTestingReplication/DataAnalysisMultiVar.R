#Run DataGeneration script prior to running this one because I'm lazily reusing defined 
#dataframes instead of importing the outputted data

library("ICSNP")

library("Hotelling")

HotellingsT2(MonoData1, MonoData2)
#Hotelling's two sample T2-test
#data:  MonoData1 and MonoData2
#T.2 = 11.056, df1 = 20, df2 = 179, p-value < 2.2e-16
#alternative hypothesis: true location difference is not equal to 
#c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

class(MonoData1)

class(PolyModel1)

HotellingsT2(PolyModel1, PolyModel2)

HotellingsT2(CyclicData1, CyclicData2)
#Hotelling's two sample T2-test
#data:  CyclicData1 and CyclicData2
#T.2 = 2197.2, df1 = 20, df2 = 179, p-value < 2.2e-16
#alternative hypothesis: true location difference is not equal to 
#c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

HotellingsT2(UnimodalData1, UnimodalData2)
#Hotelling's two sample T2-test
#data:  UnimodalData1 and UnimodalData2
#T.2 = 13.081, df1 = 20, df2 = 179, p-value < 2.2e-16
#alternative hypothesis: true location difference is not equal to 
#c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

