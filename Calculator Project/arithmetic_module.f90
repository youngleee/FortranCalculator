module arithmetic_module
    implicit none

contains

    ! Subroutine to add two numbers
    subroutine add(a, b, result)
        real, intent(in) :: a, b
        real, intent(out) :: result
        result = a + b
    end subroutine add

    ! Subroutine to subtract two numbers
    subroutine subtract(a, b, result)
        real, intent(in) :: a, b
        real, intent(out) :: result
        result = a - b
    end subroutine subtract

    ! Subroutine to multiply two numbers
    subroutine multiply(a, b, result)
        real, intent(in) :: a, b
        real, intent(out) :: result
        result = a * b
    end subroutine multiply

    ! Subroutine to divide two numbers
    subroutine divide(a, b, result, error)
        real, intent(in) :: a, b
        real, intent(out) :: result
        logical, intent(out) :: error
        if (b /= 0.0) then
            result = a / b
            error = .false.
        else
            result = 0.0
            error = .true.
        end if
    end subroutine divide

    ! Subroutine to calculate the exponentiation of a number
    subroutine exponent(a, b, result)
        real, intent(in) :: a, b
        real, intent(out) :: result
        result = a ** b
    end subroutine exponent

    ! Subroutine to calculate the modulus of two numbers
    subroutine modulus(a, b, result)
        real, intent(in) :: a, b
        real, intent(out) :: result
        result = modulo(a, b)
    end subroutine modulus

    ! Subroutine to calculate the square root of a number
    subroutine square_root(a, result)
        real, intent(in) :: a
        real, intent(out) :: result
        result = sqrt(a)
    end subroutine square_root

    ! Subroutine to calculate the sine of a number
    subroutine sine(a, result)
        real, intent(in) :: a
        real, intent(out) :: result
        result = sin(a)
    end subroutine sine

    ! Subroutine to calculate the cosine of a number
    subroutine cosine(a, result)
        real, intent(in) :: a
        real, intent(out) :: result
        result = cos(a)
    end subroutine cosine

    ! Subroutine to calculate the tangent of a number
    subroutine tangent(a, result)
        real, intent(in) :: a
        real, intent(out) :: result
        result = tan(a)
    end subroutine tangent

end module arithmetic_module
