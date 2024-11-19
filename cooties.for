      program cooties
        implicit none

        integer, parameter :: npeople = 20, nroom = 6, mturns = 20
        integer :: roccupancy(nroom) = [1, 3, 0, 2, 1, 2]
        integer :: infected(npeople), rassignment(npeople)
        integer :: turn, i, j
        integer :: nvisits(npeople), itime(npeople)
        logical :: hinfected(nroom), iinfected(npeople)
        integer :: tiinfected, trinfected
        integer :: ithisturn, mtuninfected, mtinfected
        integer :: tempmax, tempmin
        integer :: iinfectedi

        call random_seed()

        infected = 0
        rassignment = 0
        iinfected = .false.
        nvisits = 0
        itime = mturns + 1
        hinfected = .false.

        tempmax = 0
        tempmin = mturns + 1
        

        call initialize(infected, rassignment,
     &                  nvisits,npeople, iinfectedi)


        tiinfected = 0
        trinfected = 0
        mtinfected = mturns + 1
        mtuninfected = 0
       
        do turn = 1, mturns
          call spreadinfected(infected, rassignment, npeople,
     &                        ithisturn)
     
          call simulatemovement(rassignment, nvisits,
     &                          npeople, roccupancy)
 
          write(*,*) "Turn:", turn, "Infected this turn:", ithisturn
          if(ithisturn .eq. 0) exit
        END do

      do i = 1, npeople
        if(iinfected(i)) then
          tiinfected = tiinfected + 1
        END if
        if(itime(i) .gt. mtuninfected)then
          mtuninfected = itime(i)
        END if
        if(itime(i) .lt. mtinfected)then
          mtinfected = itime(i)
        END if
      END do

      do i = 1, nroom
        if(hinfected(i))then
         trinfected = trinfected + 1
        END if
      END do

      write(*,*) "Total initially infected out of 20:", tiinfected
      write(*,*) "Total room infected:", trinfected
      write(*,*) "Longest time uninfected:", mtuninfected
      write(*,*) "Shortest time to infection:", mtinfected

      contains

        subroutine initialize(infected, rassignment,
     &                        nvisits, npeople, iinfectedi)
          integer, intent(out) :: infected(npeople)
          integer, intent(out) :: rassignment(npeople) 
          integer, intent(inout) :: nvisits(npeople)
          integer, intent(in) :: npeople
          integer, intent(out) :: iinfectedi
          real :: rnd


          call random_number(rnd)
          iinfectedi = 1 + int(rnd * npeople)
          infected(iinfectedi) = 1
          iinfected(iinfectedi) = .true.
          rassignment = 0
          nvisits = 0
        END subroutine initialize

        subroutine spreadinfected(infected, rassignment,
     &                            npeople, ithisturn)
          integer, intent(inout) :: infected(npeople)
          integer, intent(in) :: rassignment(npeople)
          integer, intent(in) :: npeople
          integer, intent(out) :: ithisturn
          real :: rnd
          integer :: i, j

          ithisturn = 0
          do i = 1, npeople
            if(infected(i) .eq. 1) then
              do j = 1, npeople
                if(rassignment(i) .eq. rassignment(j) .and.
     &             infected(j) .eq. 0) then
                  call random_number(rnd)
                  if(rnd .lt. 0.5) then
                    infected(j) = 1
                    ithisturn = ithisturn + 1
                    if(itime(j) .eq. mturns + 1) itime(j) = turn
                    if(j .le. 20) iinfected(j) = .true.
                    hinfected(rassignment(j)) = .true.
                  END if
                END if
              END do
            END if
          END do
        END subroutine spreadinfected

        subroutine simulatemovement(rassignment, tbuilding,
     &                              npeople, roccupancy)
          integer, intent(inout) :: rassignment(npeople),
     &             tbuilding(npeople), roccupancy(nroom)
          integer, intent(in) :: npeople
          real :: rnd
          integer :: i, nextroom, croom

          do i = 1, npeople
            croom = rassignment(i)
            call random_number(rnd)
            nextroom = 1 + int(rnd * nroom)

            if(tbuilding(i) .lt. 4 .or. nextroom .eq. 6) then
              if(nextroom .ne. 6) then
                tbuilding(i) = tbuilding(i) + 1
              else
                tbuilding(i) = 0
            END if
            rassignment(i) = nextroom
          else
            if(nextroom .eq. 3 .and. roccupancy(3) .lt. 3)
     &         then
              nextroom = 3
            elseif(croom .eq. 4 .and. 
     &               nextroom .eq. 2) then
                do
                  call random_number(rnd)
                  nextroom = 1 + int(rnd * nroom)
                  if(nextroom .ne. 2) exit
                END do
              elseif(croom .eq. 1) then
                nextroom = 5
              END if
              if(rassignment(i) .ne. nextroom .and. 
     &           nextroom .ne. 6) then
                  roccupancy(croom) = 
     &                       roccupancy(croom) - 1
                  roccupancy(nextroom) = 
     &                       roccupancy(nextroom) + 1
                  rassignment(i) = nextroom
              END if
            END if
          END do
        END subroutine simulatemovement

      END program cooties
