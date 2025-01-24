######   ?????????????? ?? ?????????????? ???? ?????????? ???? 
#### p 231
N=5
sigmaaz1taN = 3/2+2+1+2+3/2
meanjamee = 1/N*(sigmaaz1taN)
meanjamee
##
meanYIha = 1/N*(sigmaaz1taN)
meanYIha
##
Yijha = (4-2)^2 + (5-2)^2 + (0-2)^2+((1-2)^2)*3
M= 6
S2i = 1/(M-1)*(Yijha)
S2i
##
meanS2iha = (27/10+16/5+4/5+4+3/2)
Sbar2w = 1/N*(meanS2iha)
Sbar2w
##
m = meanjamee
sigmaha <-c((2-m)^2,(1-m)^2,(3/2-m)^2,(2-m)^2,(3/2-m)^2)   
s2bb= 1/(N-1)*sigmaha
s2b = sum(s2bb)
s2b
vahedha<- c(0,1,1,3,4,0,1,1,3,5,0,2,2,1,0,0,1,2,4,5,0,1,1,1,3,2,1,2,0,1)
YI2 = sum(vahedha^2)
sigmaYi_YBAR = YI2-(N*M)*(meanjamee)^2
S2 = 1/(N*M-1)*(sigmaYi_YBAR)
S2

## now S2 ra sabet mikonim
M=6
N=5
new_s2 = (M*(N-1)*s2b + N*(M-1)*Sbar2w )*1/(N*M-1)
new_s2


## ex 4.5 p243
# Ybar = Ybar n
# ?????? 
Ybar = (1/(5*50)*(96+99.2+100.8+101.+99.5))
print(Ybar)
# ??
s2b = (1/4*((96/50-1.986)^2)+((99.2/50-1.986)^2)+((100.8/50-1.986)^2)+((101/50-1.986)^2)+((99.5/50-1.986)^2))
print(s2b)       

# or for imporove your speed type 
s2bb = (1/4*((96/50-Ybar)^2)+((99.2/50-Ybar)^2)+((100.8/50-Ybar)^2)+((101/50-Ybar)^2)+((99.5/50-Ybar)^2))
print(s2bb)       

# V(Ybarn)
Yhadn = (1/5-1/200)*s2b
Yhadn
M = 50
s2w = M/(5*49)*(0.8+1+0.6+0.8+0.7)
s2w
N= 200

shad2 = (M*(N-1)*(s2b)+N*(M-1)*s2w)/(M*N-1)
shad2

# ???????? ???????????? ???? ???????? ?????? ?????????????? ???????? ?????????? ?????? ?? ???? ?????????? ???? ?????????? ???????? ?????????? 
sigma2b = ((N-1)/N)*s2b
sigma2b

sigmahad2 = M*N-1/M*N*(shad2)
sigmahad2
m = 5
Roc= ((M*sigma2b/sigmahad2)-1*(1/m-1))
Roc

## 
## ex 7.5
## p 253
N = 20
M0 = 200
n  = 50 
M1 =10
M2=8
M3=12
M4=8
M5=15
YB10 = 80/10
YB10
YB20=64/8
YB20
YB30=90/12
YB30
YB40=50/8
YB40
YB50=72/15
YB50
MBAR = M0/20
MBAR

A <-c(M1*YB10,M2*YB20,M3*YB30,M4*YB40,M5*YB50)
A
a = A/MBAR
a
YBARi = a[1]+a[2]+a[3]+a[4]+a[5]
YBARi
YHADBAR = 1/5*YBARi
YHADBAR

# Create table or dataframe 
as.character("Total size of cluster units") <- c(10,20,30,40)
b <- c('book', 'pen', 'textbook', 'pencil_case')
c <- c(TRUE,FALSE,TRUE,FALSE)
d <- c(2.5, 8, 10, 7)
# Join the variables to create a data frame
df <- data.frame(a,b,c,d)
df

df <- starwars %>% 
  select(1:4) %>% 
  convert(fct(name),
          chr(height:mass),
          fct(hair_color)) %>% 
  print()