
n = 93;
fid =file(paste0("BrooksToM4GenerativeAv2",".bin"),"rb");
AverseInvestor = matrix(0, n, 10)
AverseTrustee = matrix(0, n, 10)
#fid =fopen(['FiatRepair' '.bin'],'r'); %SampleGenerative7 21Output Averse7
for( i in 1:n){
 for( t in 1:10){    
    AverseInvestor[i,t]=readBin(fid,integer(),endian = "little");
    AverseTrustee[i,t]=readBin(fid,integer(),endian = "little");
 }
}

close(fid)

AverseInvestor = 5*AverseInvestor
AverseTrustee = AverseInvestor*3/6*AverseTrustee
