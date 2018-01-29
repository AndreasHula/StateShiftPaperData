

n = 400;
for( j in 1:1){
fid =file(paste0("FiatBreakShiftData",".bin"),"rb");
#fid =fopen(['FiatRepairShift' '.bin'],'r'); 
BreakInvestor = matrix(0, n, 10)
BreakTrustee = matrix(0, n, 10)
BreakShift = matrix(0, n, 10)
for( i in 1:n){
 for( t in 1:10){    
    BreakInvestor[i+(j-1)*n,t]=readBin(fid,integer(),endian = "little");
    BreakTrustee[i+(j-1)*n,t]=readBin(fid,integer(),endian = "little");
    BreakShift[i+(j-1)*n,t] = readBin(fid,double(),endian = "little");     
    }
}
    close(fid);
}