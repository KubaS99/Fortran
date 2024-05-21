        integer counter,choose
        allocatable :: a(:)
        character(len=32) :: fileName
1       print *, '1. Podaj ciąg liczb'
        print *, '2. Wczytaj ciąg z pliku (pionowy format)'
        read *, choose
        if(choose .gt. 2) then
                goto 1
        endif

        print *, 'Podaj ilosc liczb w ciagu'
        read *, ilosc
        ilosc=ilosc+1
        allocate(a(ilosc))
        do i=1,ilosc
               a(i)=0
        enddo
        if(choose .eq. 1) then
                print *,'Podaj wartosci ciagu'
                do i=1,ilosc-1
                       read(*,*,end=10) a(i)
                enddo
        else
                print *,'Podaj nazwe pliku'
                read *,filename
                open(2,file=fileName)
                do i=1,ilosc-1
                        read(2,*,end=10) a(i)
                enddo
                close(2)
        endif
10      indeksCiagu=1
        counter=2
        do i=3,ilosc
          if(a(i-2) .ne. 0) then
                if(a(i) .eq. (a(i-1)*(a(i-1)/a(i-2)))) then
                        counter=counter+1
                else
                        if(counter .gt. 2) then
                                print *,'Podciag nr:',indeksCiagu
                                do j=i-counter,i-1
                                        print *,a(j)
                                enddo
                                indeksCiagu=indeksCiagu+1
                        endif
                        counter=2
                endif
          endif
        enddo
       end
