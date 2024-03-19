program calculator
    use arithmetic_module
    use calculation_module
    implicit none
    real :: num1, num2, result, previous_result
    character(10) :: operator
    logical :: error
    character(1) :: retry

    ! Initializierung der vorherigen Antwort zur 0
    previous_result = 0.0

    ! Haupt-Schleife f�r mehrere Berechnungen
    do
        ! Linie f�r optische Trennung
        print *, '---------------------------------------------'

        ! Vorheriges Ergebnis anzeigen
        print *, 'Vorheriges Ergebnis:', previous_result

        ! Linie f�r optische Trennung
        print *, '---------------------------------------------'

        ! Benutzereingabe f�r die erste Zahl
        print *, 'Geben Sie die erste Zahl ein:'
        read(*, *) num1

        ! Linie f�r optische Trennung
        print *, '---------------------------------------------'

        ! Verf�gbare Operatoren anzeigen
        print *, 'Verf�gbare Operatoren: +, -, *, /, exp, mod, sqrt, sin, cos, tan'

        ! Benutzereingabe f�r den Operator
        print *, 'Geben Sie den Operator ein:'
        read(*, '(A10)') operator

        ! Quadratwurzel, Sinus, Cosinus oder Tangens Operationen separat ausf�hren
        if (trim(operator) == 'sqrt' .or. trim(operator) == 'sin' .or. &
            trim(operator) == 'cos' .or. trim(operator) == 'tan') then
            call perform_unary_operation(num1, operator, result)
        else
            ! Benutzereingabe f�r die zweite Zahl
            print *, 'Geben Sie die zweite Zahl ein:'
            read(*, *) num2

            ! Berechnung basierend auf dem Operator durchf�hren
            call perform_calculation(num1, num2, operator, result, error)

            ! �berpr�fen, ob eine Division durch Null vorliegt
            if (error) then
                print *, 'Fehler: Division durch Null!'
            else
                ! Vorheriges Ergebnis aktualisieren, wenn kein Fehler auftritt
                previous_result = result
            end if
        end if

        ! Linie f�r optische Trennung
        print *, '---------------------------------------------'

        ! Ergebnis anzeigen
        print *, 'Ergebnis:', result

        ! Linie f�r optische Trennung
        print *, '---------------------------------------------'

        ! Benutzer fragen, ob eine weitere Berechnung durchgef�hrt werden soll
        print *, 'M�chten Sie eine weitere Berechnung durchf�hren? (J/N)'
        read(*, '(A1)') retry

        ! Schleife verlassen, wenn der Benutzer nicht fortsetzen m�chte
        if (trim(adjustl(retry)) /= 'J') exit
    end do

contains

    ! Unterprogramm zur Ausf�hrung von un�ren Operationen (Wurzel, Sinus, Cosinus, Tangens)
    subroutine perform_unary_operation(num, operator, result)
        real, intent(in) :: num
        character(len=10), intent(in) :: operator
        real, intent(out) :: result

        select case (trim(operator))
            case ('sqrt')
                call square_root(num, result)
            case ('sin')
                call sine(num, result)
            case ('cos')
                call cosine(num, result)
            case ('tan')
                call tangent(num, result)
        end select
    end subroutine perform_unary_operation

end program calculator
