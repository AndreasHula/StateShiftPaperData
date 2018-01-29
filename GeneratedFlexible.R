
n = 93;
fid =file(paste0("FlexibleGenerated",".bin"),"rb");
FlexiInvestor = matrix(0, n, 10)
FlexiTrustee = matrix(0, n, 10)
#fid =fopen(['FiatRepair' '.bin'],'r'); %SampleGenerative7 21Output Averse7
for( i in 1:n){
 for( t in 1:10){    
    FlexiInvestor[i,t]=readBin(fid,integer(),endian = "little");
    FlexiTrustee[i,t]=readBin(fid,integer(),endian = "little");
 }
}

close(fid)

FlexiInvestor = 5*FlexiInvestor
FlexiTrustee = FlexiInvestor*3/6*FlexiTrustee

