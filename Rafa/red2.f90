integer*4, parameter :: n=90
real*8 v(4),vec(4,n),dum,xg,xd,pasx,ys,ispline,sol,bbin,randomio
integer*4 io,io2,b2,reg,col,row
real*8, dimension (n) :: b(n), c(n), d(n)
integer*4 :: rate,begino,endo,atr,atc
character(len=24) dir(225),fil(225),path,aa,bb
!write(*,*)"Holiiiii"

print *, "So it begins..."
print *, ""
print *, ""
call system_clock(begino,rate)

!open(unit=50,file="./FULL_THRESHOLD_SCAN/ThresholdScan_151119_1420.dat",status="old")

call srand(42)
!Se abre el archivo con las direcciones
open(unit=40,file="./Total/test2.dat",status="old")

!Se leen las direcciones
do l=1,225
read(40,*,IOSTAT=io2)dir(l),fil(l)
!write(*,*)".\"//TRIM(dir)//"ScanConfig_"//TRIM(fil)//".cfg"
if(io2.ne.0)then
write(*,*)"io:",io2,"Breaaak"
exit
endif
enddo

write(*,*) l
!Se abre el archivo para el output
open(unit=53,file=".\datasample1y3.dat",status="replace")
!Header del output
write(53,"(A4X1A4X1A12X1A4X1A4X1A4X1A4)")"col,","fil,","volt,","tmp,","vbb,","vcs,","reg"


do jj=1,l-1

open(unit=52,file=".\Total\"//TRIM(dir(jj))//"ScanConfig_"//TRIM(fil(jj))//".cfg",status="old")
open(unit=50,file=".\Total\"//TRIM(dir(jj))//"ThresholdScanByRandom_PixelsPerReg_500_"//TRIM(fil(jj))//".dat",status="old")
open(unit=51,file=".\Total\"//TRIM(dir(jj))//"ThreshOut_"//TRIM(fil(jj))//".dat",status="replace")

!Se extraen las variables
write(51,*)"VBB  ",dir(jj)(13:13)
write(51,*)"TEMP  ",dir(jj)(6:7)
do i=1,20
read(52,*)aa,bb
if(aa.eq."VCASN")then
!write(*,*)aa,bb
read(bb,"(i2)")b2
write(51,*)"VCASN ",b2
exit
endif
enddo
close(52)
!open(unit=50,file="./test.dat",status="old")
dum=1
!i=1
v=0
vec=0
j=1

!Se procesan las lineas de la data
read(50,*,IOSTAT=io)v(:)
dum=v(1)*10000+v(2)


do while(1.eq.1) 


	!Se implementa un spline y se usa biseccion para encontrar el flanco
	if((v(1)*10000+v(2)).ne.dum)then
	write(51,*)dum
		!write(*,*)vec(:,90)
		!write(*,*)v(:)
		j=1	
			call spline (vec(3,:), vec(4,:), b, c, d,n) 
		xg = 10  
		xd = 70  
		pasx = 0.04
		do while (xd.le.(89-pasx))
			xd=xd+pasx			
			if ((25-ispline(xg,vec(3,:),vec(4,:),b,c,d,n))*(25-ispline(xd,vec(3,:),vec(4,:),b,c,d,n)).lt.0) then
			sol=bbin(xg,xd,1.0d-8,vec,b,c,d,n)
			xg=xd
			exit
			endif                     
		enddo
		col=atc(int(vec(1,1)),int(vec(2,1)))
		row=atr(int(vec(2,1)))
		write(51,*)col,row,sol*7,ispline(xg,vec(3,:),vec(4,:),b,c,d,n)
		randomio=rand()
		if(col.lt.256)reg=1
		if((col.gt.512).AND.(col.lt.768))reg=3
		!Se eligen datos al azar
		if((randomio.lt.(0.08)).AND.((col.lt.256).OR.((col.gt.512).AND.(col.lt.768))) )then
		write(53,"(I4A1I4A1F12.4A1A4A1A4A1I4A1I4)")col,",",row,",",sol*7,",",dir(jj)(6:7),",",dir(jj)(13:13),",",b2,",",reg
		endif
		
		! if((col.gt.256).AND.(col.lt.512))reg=2
		! if((col.gt.512).AND.(col.lt.768))reg=3
		! if((randomio.lt.(0.08)).AND.(((col.gt.256).AND.(col.lt.512)).OR.((col.gt.512).AND.(col.lt.768))) )then
		! write(53,"(I4A1I4A1F12.4A1A4A1A4A1I4A1I4)")col,",",row,",",sol*7,",",dir(jj)(6:7),",",dir(jj)(13:13),",",b2,",",reg
		! endif
		
		
		vec=0
	endif
	
if(io.ne.0)then
write(*,*)"io:",io,"Breaaak"
exit
endif

	dum=v(1)*10000+v(2)
	vec(:,j)=v
	read(50,*,IOSTAT=io)v(:)
	j=j+1
	!i=i+1
enddo

	write(51,*)dum
		!write(*,*)vec(:,90)
		!write(*,*)v(:)
		j=1	
			call spline (vec(3,:), vec(4,:), b, c, d,n) 
		xg = 10  
		xd = 70  
		pasx = 0.04
		do while (xd.le.(89-pasx))
			xd=xd+pasx			
			if ((25-ispline(xg,vec(3,:),vec(4,:),b,c,d,n))*(25-ispline(xd,vec(3,:),vec(4,:),b,c,d,n)).lt.0) then
			sol=bbin(xg,xd,1.0d-8,vec,b,c,d,n)
			xg=xd
			exit
			endif                     
		enddo
		col=atc(int(vec(1,1)),int(vec(2,1)))
		row=atr(int(vec(2,1)))
		write(51,*)col,row,sol*7,ispline(xg,vec(3,:),vec(4,:),b,c,d,n)
		randomio=rand()
		if(col.lt.256)reg=1
		if((col.gt.512).AND.(col.lt.768))reg=3
		if((randomio.lt.(0.08)).AND.((col.lt.256).OR.((col.gt.512).AND.(col.lt.768))) )then
		write(53,"(I4A1I4A1F12.4A1A4A1A4A1I4A1I4)")col,",",row,",",sol*7,",",dir(jj)(6:7),",",dir(jj)(13:13),",",b2,",",reg
		endif
		vec=0



! do i=1,95

! read(50,*,IOSTAT=io) vec(:,i)
! if(io.ne.0)then
! write(*,*)"io:",io,"Breaaak"
! exit
! endif
! !write(*,*) vec(:,i),io

! enddo
close(50)
close(51)

enddo


call system_clock(endo)
print *, ""
print *, ""
print *, "The deed is done."
print *, "time:",real(endo-begino)/real(rate)

close(40)
close(53)


end
























function atc(col, add)
integer*4 col,add, atc
atc=col*2
if((mod(add,4)).lt.2)atc=atc+1
end function



function atr(add)
integer*4 add, atr
atr=add/2
if((mod(add,4)).eq.3)atr=atr-1
if((mod(add,4)).eq.0)atr=atr+1
end function


function bbin(a,b,pres,vec,bs,cs,ds,n)
integer*4 n
real*8 vec(4,n),ispline
real*8, dimension (n) :: bs(n), cs(n), ds(n)
real*8 ya,yb,ym,pres,m,a,b,bbin

    ya = 25-ispline(a,vec(3,:),vec(4,:),bs,cs,ds,n)
    yb = 25-ispline(b,vec(3,:),vec(4,:),bs,cs,ds,n)

    do while(abs(a-b).gt.pres)
        m = 0.5*(a+b)
        ym = 25-ispline(m,vec(3,:),vec(4,:),bs,cs,ds,n)
        if(ym == 0) bbin=m
        if(ya*ym < 0)then
            b = m
            yb = ym
        else
            a = m
            ya = ym
        endif
      enddo
    bbin=(0.5*(a+b))

end function




   subroutine spline (x, y, b, c, d, n)
!======================================================================
!  Calculate the coefficients b(i), c(i), and d(i), i=1,2,...,n
!  for cubic spline interpolation
!  s(x) = y(i) + b(i)*(x-x(i)) + c(i)*(x-x(i))**2 + d(i)*(x-x(i))**3
!  for  x(i) <= x <= x(i+1)
!  Alex G: January 2010
!----------------------------------------------------------------------
!  input..
!  x = the arrays of data abscissas (in strictly increasing order)
!  y = the arrays of data ordinates
!  n = size of the arrays xi() and yi() (n>=2)
!  output..
!  b, c, d  = arrays of spline coefficients
!  comments ...
!  spline.f90 program is based on fortran version of program spline.f
!  the accompanying function fspline can be used for interpolation
!======================================================================
implicit none
integer n
double precision x(n), y(n), b(n), c(n), d(n)
integer i, j, gap
double precision h

gap = n-1
! check input
if ( n < 2 ) return
if ( n < 3 ) then
  b(1) = (y(2)-y(1))/(x(2)-x(1))   ! linear interpolation
  c(1) = 0.
  d(1) = 0.
  b(2) = b(1)
  c(2) = 0.
  d(2) = 0.
  return
end if
!
! step 1: preparation
!
d(1) = x(2) - x(1)
c(2) = (y(2) - y(1))/d(1)
do i = 2, gap
  d(i) = x(i+1) - x(i)
  b(i) = 2.0*(d(i-1) + d(i))
  c(i+1) = (y(i+1) - y(i))/d(i)
  c(i) = c(i+1) - c(i)
end do
!
! step 2: end conditions 
!
b(1) = -d(1)
b(n) = -d(n-1)
c(1) = 0.0
c(n) = 0.0
if(n /= 3) then
  c(1) = c(3)/(x(4)-x(2)) - c(2)/(x(3)-x(1))
  c(n) = c(n-1)/(x(n)-x(n-2)) - c(n-2)/(x(n-1)-x(n-3))
  c(1) = c(1)*d(1)**2/(x(4)-x(1))
  c(n) = -c(n)*d(n-1)**2/(x(n)-x(n-3))
end if
!
! step 3: forward elimination 
!
do i = 2, n
  h = d(i-1)/b(i-1)
  b(i) = b(i) - h*d(i-1)
  c(i) = c(i) - h*c(i-1)
end do
!
! step 4: back substitution
!
c(n) = c(n)/b(n)
do j = 1, gap
  i = n-j
  c(i) = (c(i) - d(i)*c(i+1))/b(i)
end do
!
! step 5: compute spline coefficients
!
b(n) = (y(n) - y(gap))/d(gap) + d(gap)*(c(gap) + 2.0*c(n))
do i = 1, gap
  b(i) = (y(i+1) - y(i))/d(i) - d(i)*(c(i+1) + 2.0*c(i))
  d(i) = (c(i+1) - c(i))/d(i)
  c(i) = 3.*c(i)
end do
c(n) = 3.0*c(n)
d(n) = d(n-1)
end subroutine spline

  function ispline(u, x, y, b, c, d, n)
!======================================================================
! function ispline evaluates the cubic spline interpolation at point z
! ispline = y(i)+b(i)*(u-x(i))+c(i)*(u-x(i))**2+d(i)*(u-x(i))**3
! where  x(i) <= u <= x(i+1)
!----------------------------------------------------------------------
! input..
! u       = the abscissa at which the spline is to be evaluated
! x, y    = the arrays of given data points
! b, c, d = arrays of spline coefficients computed by spline
! n       = the number of data points
! output:
! ispline = interpolated value at point u
!=======================================================================
implicit none
double precision ispline
integer n
double precision  u, x(n), y(n), b(n), c(n), d(n)
integer i, j, k
double precision dx

! if u is ouside the x() interval take a boundary value (left or right)
if(u <= x(1)) then
  ispline = y(1)
  return
end if
if(u >= x(n)) then
  ispline = y(n)
  return
end if

!*
!  binary search for for i, such that x(i) <= u <= x(i+1)
!*
i = 1
j = n+1
do while (j > i+1)
  k = (i+j)/2
  if(u < x(k)) then
    j=k
    else
    i=k
   end if
end do
!*
!  evaluate spline interpolation
!*
dx = u - x(i)
ispline = y(i) + dx*(b(i) + dx*(c(i) + dx*d(i)))
end function ispline
