program pts
integer i,n,x(100)
real x0,x1,df1,df2,df3,df4,h
real::
f =(/2408,6570,14652,28574,60640/)
x0=2007
x1=2011
n=5
h=2
h1=1

df1=((1/2*h)*(-3*f(x0)+4*f(x0+h)-f(x0+2*h)))
df2=((1/2*h)*(f(x1+h)-f(x1-h)))
df3=((1/12*h)*(f(x1-2*h)-f(x1+2*h)+8*f(x1+h)-8*f(x1-h)))
df4=((1/12*h)*(-25*f(x0)+48*f(x0+h)-36*f(x0+2*h)+16*f(x0+3*h)-3*f(x0+4*h)))
write(*,*)"Three point end point formula",df1
write(*,*)"3mid",df2
write(*,*)"5mid",df3
write(*,*)"5end",df4
end