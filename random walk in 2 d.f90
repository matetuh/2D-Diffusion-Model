program ranWalk2d
    implicit none
    integer :: L = 10
    integer :: Lp = 10
    integer :: N = 10
    integer :: MCS = 100
    integer :: i,k,j,b
    real :: C = 0.
    real :: r = 0.
    real, dimension(:), allocatable :: dr
    real, dimension(:), allocatable :: drr
    real, dimension(:), allocatable :: dx
    real, dimension(:), allocatable :: dy
    real, dimension(:), allocatable :: D
    integer, dimension(:), allocatable :: x
    integer, dimension(:), allocatable:: y
    integer, dimension(:,:), allocatable:: A
    integer, dimension(:), allocatable:: INN
    integer, dimension(:), allocatable:: IPP

    allocate(dr(MCS))
    allocate(D(MCS))
    allocate(drr(MCS))
    allocate(dx(MCS))
    allocate(dy(MCS))
    allocate(x(N))
    allocate(y(N))
    allocate(INN(L))
    allocate(IPP(L))
    allocate(A(L,L))

    do i=1,MCS
        D(i)=0
    enddo

    do i=1,L
        INN(i)=i+1
        IPP(i)=i-1
    end do

    INN(L)=1
    IPP(1)=L

    do k=1,Lp
        do i=1,L
            do j = 1,L
                A(i,j)=0
            enddo
        enddo

        do i=1,MCS
            dx(i)=0
            dy(i)=0
        enddo

        do i=1,N
10          continue
            call random_number(r)
            x(i)=int(r*L+1.)
            call random_number(r)
            y(i)=int(r*L+1.)
            if(A(x(i),y(i))==0)then
                A(x(i),y(i))=1
            else
                goto 10
            endif
        enddo

        do j=1,MCS
            do i=1,N
                call random_number(r)
                b=int(r*4.+1.)
                if (b==1) then
                    if(A(INN(x(i)),y(i))==0)then
                        A(x(i),y(i))=0
                        A(INN(x(i)),y(i))=1
                        dx(j)=dx(j)+1
                        x(i)=INN(x(i))
                    endif
                elseif (b==2) then
                    if(A(IPP(x(i)),y(i))==0)then
                        A(x(i),y(i))=0
                        A(IPP(x(i)),y(i))=1
                        dx(j)=dx(j)-1
                        x(i)=IPP(x(i))
                    endif
                elseif (b==3) then
                    if(A(x(i),INN(y(i)))==0)then
                        A(x(i),y(i))=0
                        A(x(i),INN(y(i)))=1
                        dy(j)=dy(j)+1
                        y(i)=INN(y(i))
                    endif
                elseif (b==4) then
                    if(A(x(i),IPP(y(i)))==0)then
                        A(x(i),y(i))=0
                        A(x(i),IPP(y(i)))=1
                        dy(j)=dy(j)-1
                        y(i)=IPP(y(i))
                    endif
                endif
            enddo
        enddo
        dr(1)=(dx(1))**2+(dy(1))**2
        do i=1,MCS-1
            dr(i+1)=dr(i)+(dx(i))**2+(dy(i))**2
        enddo
        do i=1,MCS
            drr(i)=dr(i)/float(4*i)
        enddo
        do i=1,MCS
            D(i)=D(i)+drr(i)
        enddo
    enddo
    open(1, file = "Diffusion_L10_N50_0.csv", status = "unknown")
    do i=1,MCS
        D(i)=D(i)/float(Lp*N)
        write(1,*) i ,",", D(i)
    enddo
    close(1)
end program
