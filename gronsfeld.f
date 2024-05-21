        character(len=1000) :: input
        character(len=:), allocatable :: text 
        integer opcja,dlugosc,temp
        integer, allocatable :: kod(:)
1       print *,'Wpisz tekst wiadomosci (tylko wielkie litery)'
        read (*,'(A)') input
        text=trim(adjustl(input))
        do i=1,len(text)
                if(ICHAR(text(i:i)) .gt. 90) then
                        print *,'Akceptowalne sa jedynie wielkie litery'
                        goto 1
                endif
                if(ICHAR(text(i:i)) .lt. 65) then
                        print *,'Akceptowalne sa jedynie wielkie litery'
                        goto 1
                endif
        enddo
        print *,'Podaj dlugosc klucza'
        read *,dlugosc
        allocate(kod(dlugosc))
        print *,'Wprowadz klucz (format: xxxxxxx)'
        read *,temp
        i=dlugosc
        do while(temp .gt. 0)
                kod(i) = MOD(temp,10)
                temp=temp/10
                i=i-1
        enddo                                

30      print *,'1. Koduj'
        print *,'2. Dekoduj'
        read *, opcja
        if(opcja .eq. 1) then
                goto 10
        elseif(opcja .eq. 2) then
                goto 20
        else
                goto 30
        endif
10      j=1
        do i=1,len(text)
                temp=ICHAR(text(i:i))+kod(j)
                if(temp .gt. 90) then
                        temp=temp-90
                        text(i:i)=CHAR(64+temp)
                else
                        text(i:i)=CHAR(temp)
                endif
                j=j+1
                if(j .eq. dlugosc+1) then
                        j=1
                endif
        enddo
        print *,'Zakodowany tekst:'
        print *,text
        goto 40
20      j=1
        do i=1,len(text)
                temp=ICHAR(text(i:i))-kod(j)
                if(temp .lt. 65) then
                        text(i:i)=CHAR(temp+26)
                else
                        text(i:i)=CHAR(temp)
                endif
                j=j+1
                if(j .eq. dlugosc+1) then
                        j=1
                endif
        enddo
        print *,'Odkodowany tekst:'
        print *,text
40      end
