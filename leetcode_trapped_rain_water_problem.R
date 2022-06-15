
#My solution in R to https://leetcode.com/problems/trapping-rain-water/

#example 1

hgt <- c(0,1,0,2,1,0,1,3,2,1,2,1)

#example 2
hgt <- c(4,2,0,3,2,5)


hgtn<-hgt

#maximum height
mxhgt<-max(hgt)

# initiate water space count
w<-0

for(i in 1:mxhgt)
{
  hgtb<-as.logical(hgt)
  for(l in 1:length(hgtb)){
    if (hgtb[l]==TRUE) hgtn[l]<-1
    else if (hgtb[l]==FALSE) hgtn[l]<-0
  }
  #print(paste('i=', i))
  #print(hgtn)
  dips<-hgtn
  for (r in 1:length(hgtn)){
    if (dips[1]==0) dips<-dips[-1]
    #print(dips)
  }
  dipsrev<-rev(dips)
  for (r in 1:length(dipsrev)){
    if (dipsrev[1]==0) dipsrev<-dipsrev[-1]
    #print(dipsrev)
  }
  kz<-dipsrev
  for(y in 1:length(kz)){
    if (kz[y]==0) w<-w+1
  }

  hgt<-hgt-hgtn
}

# trapped rainwater final 
print(w)
