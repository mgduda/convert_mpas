module timer

    integer, parameter :: I8KIND = selected_int_kind(18)

    type timer_type
        integer(kind=I8KIND) :: count_start
        integer(kind=I8KIND) :: count_stop
        integer(kind=I8KIND) :: count_rate
    end type timer_type


    contains


    subroutine timer_start(t)

        implicit none

        type (timer_type), intent(out) :: t

        call system_clock(count=t % count_start, count_rate=t % count_rate)
       
    end subroutine timer_start


    subroutine timer_stop(t)

        implicit none

        type (timer_type), intent(inout) :: t

        call system_clock(count=t % count_stop)
       
    end subroutine timer_stop


    double precision function timer_time(t)

        implicit none

        type (timer_type), intent(in) :: t

        timer_time = dble(t % count_stop - t % count_start) / dble(t % count_rate)

    end function timer_time

end module timer
