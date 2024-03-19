module calculation_module
    use arithmetic_module
    implicit none

contains

    ! Unterprogramm zur Durchführung der Berechnung
    subroutine perform_calculation(num1, num2, operator, result, error)
        real, intent(in) :: num1, num2
        character(len=*), intent(in) :: operator
        real, intent(out) :: result
        logical, intent(out) :: error

        error = .false. ! Fehlerflag zuerst auf falsch setzen

        ! Entsprechende Unterprogramme basierend auf dem Operator aufrufen
        select case(trim(operator))
            case('+')
                call add(num1, num2, result)
            case('-')
                call subtract(num1, num2, result)
            case('*')
                call multiply(num1, num2, result)
            case('exp')
                call exponent(num1, num2, result)
            case('mod')
                call modulus(num1, num2, result)
            case('/')
                call divide(num1, num2, result, error)
                if (error) then
                    print *, 'Fehler: Division durch Null!'
                end if
            case default
                print *, 'Fehler: Ungültiger Operator!'
                error = .true. ! Fehlerflag für ungültigen Operator setzen
        end select
    end subroutine perform_calculation

end module calculation_module
