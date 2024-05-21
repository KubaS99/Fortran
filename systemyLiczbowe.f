        character znak(36), znak36*36, wynik(31)
        equivalence(znak,znak36)
        data znak36/'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'/

        do i=1,31
                wynik(i)=znak(1)
        enddo
1       print *,'Podaj liczbę naturalną'
        read *,l
        if(l.lt.1) go to 1
2       print *,'Podaj poststawę'
        read *,m
        if(m.lt.2 .or. m.gt.36)  go to 2
        i=31
        do while(l .gt. 0)
                wynik(i) = znak(MOD(l,m)+1)
                l=l/m
                i=i-1
        enddo
        i=1
        do while(wynik(i) .eq. '0')
                i=i+1
        enddo
        do j=i,31
                write(*, fmt="(a)", advance="no") wynik(j)
        enddo
        print *,''
        end
