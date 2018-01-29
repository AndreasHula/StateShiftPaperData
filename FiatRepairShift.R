

n = 400;
for( j in 1:1){
fid =file(paste0("FiatRepairShiftData",".bin"),"rb");
#fid =fopen(['FiatRepairShift' '.bin'],'r'); 
RepairInvestor = matrix(0, n, 10)
RepairTrustee = matrix(0, n, 10)
Shift = matrix(0, n, 10)
for( i in 1:n){
 for( t in 1:10){    
    RepairInvestor[i+(j-1)*n,t]=readBin(fid,integer(),endian = "little");
    RepairTrustee[i+(j-1)*n,t]=readBin(fid,integer(),endian = "little");
    Shift[i+(j-1)*n,t] = readBin(fid,double(),endian = "little");    
    }
}
    close(fid);
}