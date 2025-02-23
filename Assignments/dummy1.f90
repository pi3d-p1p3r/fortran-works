Program date_time
Implicit None
Character(len=8)::yyyymmdd
Character(len=4)::Year
Character(len=5)::Month,Datee
Read(*,*)yyyymmdd
Year=yyyymmdd(1:4)
Month=yyyymmdd(5:6)
Datee=yyyymmdd(7:8)
Write(*,*)"Dt=",yyyymmdd
Write(*,*)"Year=",Year
Write(*,*)"Month=",Month
Write(*,*)"Datee=",Datee
End Program