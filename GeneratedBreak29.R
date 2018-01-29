#X = [0];% 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23];

#IDisapp = zeros(1,4);
n = 200;
#for j = 1:#(length(X))
fid =file(paste0("Break29",".bin"),"rb"); #%SampleGenerative7 21Output Averse7
FinalInvestor = matrix(0, n, 10)
FinalTrustee = matrix(0, n, 10)
for( t in 1:10){ 
  for( i in 1:n){
    FinalInvestor[i,t]=readBin(fid,integer(),endian = "little");
  }
  for( i in 1:n){
    FinalTrustee[i,t]=readBin(fid,integer(),endian = "little");
  }
}

close(fid)

fid =file(paste0("AvBreak29",".bin"),"rb"); #%SampleGenerative7 21Output Averse7
AversionInvestor = matrix(0, n, 10)
AversionTrustee = matrix(0, n, 10)
for( t in 1:10){ 
  for( i in 1:n){
    AversionInvestor[i,t]=readBin(fid,integer(),endian = "little");
  }
  for( i in 1:n){
    AversionTrustee[i,t]=readBin(fid,integer(),endian = "little");
  }
}

close(fid)

fid =file(paste0("OBreak29",".bin"),"rb"); #%SampleGenerative7 21Output Averse7
ClassicInvestor = matrix(0, n, 10)
ClassicTrustee = matrix(0, n, 10)
for( t in 1:10){ 
  for( i in 1:n){
    ClassicInvestor[i,t]=readBin(fid,integer(),endian = "little");
  }
  for( i in 1:n){
    ClassicTrustee[i,t]=readBin(fid,integer(),endian = "little");
  }
}

close(fid)
#end

#for j = 1:(length(X))
#fid =fopen(['AvBreak29' '.bin'],'r'); %SampleGenerative7 21Output Averse7

# for t = 1:10 
#for i = 1:n     
#    XInvestor(i+(j-1)*n,t)=fread(fid,1,'int32');
#end
#for i = 1:n
#    XTrustee(i+(j-1)*n,t)=fread(fid,1,'int32');
# end
#end
#    fclose(fid);
#end

#for j = 1:(length(X))
#fid =fopen(['OBreak29' '.bin'],'r'); %SampleGenerative7 21Output Averse7

# for t = 1:10 
#for i = 1:n     
#    YInvestor(i+(j-1)*n,t)=fread(fid,1,'int32');
#end
#for i = 1:n
#    YTrustee(i+(j-1)*n,t)=fread(fid,1,'int32');
# end
#end
#    fclose(fid);
#end
