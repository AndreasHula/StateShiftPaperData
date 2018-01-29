
n = 93;
fid =file(paste0("BrooksToM4GenerativeOld",".bin"),"rb");
OldInvestor = matrix(0, n, 10)
OldTrustee = matrix(0, n, 10)
#fid =fopen(['FiatRepair' '.bin'],'r'); %SampleGenerative7 21Output Averse7
for( i in 1:n){
 for( t in 1:10){    
    OldInvestor[i,t]=readBin(fid,integer(),endian = "little");
    OldTrustee[i,t]=readBin(fid,integer(),endian = "little");
 }
}

close(fid)


OldInvestor = 5*OldInvestor
OldTrustee = OldInvestor*3/6*OldTrustee