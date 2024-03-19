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

    ! Haupt-Schleife für mehrere Berechnungen
    do
        ! Linie für optische Trennung
        print *, '---------------------------------------------'

        ! Vorheriges Ergebnis anzeigen
        print *, 'Vorheriges Ergebnis:', previous_result

        ! Linie für optische Trennung
        print *, '---------------------------------------------'

        ! Benutzereingabe für die erste Zahl
        print *, 'Geben Sie die erste Zahl ein:'
        read(*, *) num1

        ! Linie für optische Trennung
        print *, '---------------------------------------------'

        ! Verfügbare Operatoren anzeigen
        print *, 'Verfügbare Operatoren: +, -, *, /, exp, mod, sqrt, sin, cos, tan'

        ! Benutzereingabe für den Operator
        print *, 'Geben Sie den Operator ein:'
        read(*, '(A10)') operator

        ! Quadratwurzel, Sinus, Cosinus oder Tangens Operationen separat ausführen
        if (trim(operator) == 'sqrt' .or. trim(operator) == 'sin' .or. &
            trim(operator) == 'cos' .or. trim(operator) == 'tan') then
            call perform_unary_operation(num1, operator, result)
        else
            ! Benutzereingabe für die zweite Zahl
            print *, 'Geben Sie die zweite Zahl ein:'
            read(*, *) num2

            ! Berechnung basierend auf dem Operator durchführen
            call perform_calculation(num1, num2, operator, result, error)

            ! Überprüfen, ob eine Division durch Null vorliegt
            if (error) then
                print *, 'Fehler: Division durch Null!'
            else
                ! Vorheriges Ergebnis aktualisieren, wenn kein Fehler auftritt
                previous_result = result
            end if
        end if

        ! Linie für optische Trennung
        print *, '---------------------------------------------'

        ! Ergebnis anzeigen
        print *, 'Ergebnis:', result

        ! Linie für optische Trennung
        print *, '---------------------------------------------'

        ! Benutzer fragen, ob eine weitere Berechnung durchgeführt werden soll
        print *, 'Möchten Sie eine weitere Berechnung durchführen? (J/N)'
        read(*, '(A1)') retry

        ! Schleife verlassen, wenn der Benutzer nicht fortsetzen möchte
        if (trim(adjustl(retry)) /= 'J') exit
    end do

contains

    ! Unterprogramm zur Ausführung von unären Operationen (Wurzel, Sinus, Cosinus, Tangens)
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
