! -----------------------------------------------------------------
! Description: readWord: Takes input from the user in the form of  a word to be encrypted, 
!                        and returns the word to the calling program.
! Param w: user submitted word in ASCII (out)
! -----------------------------------------------------------------
subroutine readWord(w)
    implicit none
    character(len=10) :: w
    
    write(*, *) 'Enter word to be encrypted'
    read(*, 1000) w

    1000 format(A10)
    return
end

! -----------------------------------------------------------------
! Description: word2hex: converts the ASCII word obtained using readWord(), 
!                        to hexadecimal, before it is passed to the subroutine expand()
! Param w: word (in) 
! Param h: word in hexidecimal (out) 
! Param l: length of the word entered (in)
! Param hexLength: length of the resulting hex word
! -----------------------------------------------------------------
subroutine word2hex(w, h, l, hexLength)
    implicit none

    integer :: l, i, j, result, hIndex, tempIndex, hexLength
    real :: remainder
    character(len=l) :: w
    integer, dimension(0:l-1) :: h
    integer, dimension(0: l) :: asciiWord
    integer, dimension(0:1) :: x


!   Convert char to ASCII int array
    do i = 1, l + 1
        if (i == l + 1) then
            asciiWord(i - 1) = 0
        else
            asciiWord(i - 1) = iachar(w(i:i))
        end if
    end do
    
!   Keep track of where in the hex string to insert next value
    hIndex = 0

!   Logic for converting a word to hex. Loop through every letter in the word
    do i = 0, l - 1
        result = asciiWord(i)
        tempIndex = 1

!       This loop is to convert the decimal to hex
        do
            if (result <= 0) exit
            remainder = modulo(result, 16)
            result = result / 16

!           I do this because when converting decimal to hex, the desired values are reversed, so we load it into a temp array in reverse
!           This way when reading it front to back we have the correct value
            x(tempIndex) = int(remainder)
            tempIndex = tempIndex - 1
        end do

!       Load in the temp array with the hex value into the new hex array
        do j = 0, 1
            h(hIndex) = x(j)
            hIndex = hIndex + 1
        end do
    end do

    hexLength = hIndex

!   Any remaining values should be set to 0
    do i = hIndex, 32
        h(i) = 0
    end do

    return
end

! -----------------------------------------------------------------
! Description: printhex: Prints a word into hex format
! Param h: word entered in hexidecimal (out)
! Param l: length of the word entered (out)
! -----------------------------------------------------------------
subroutine printhex(h, l)
    implicit none

    integer :: l, i
    integer, dimension(0:31) :: h
    integer, dimension(0:l - 1) :: formattedWord

    do i = 0, l - 1
        formattedWord(i) = h(i)
    end do

    write(*,1007) formattedWord
    
    1007 format(1x,32z1.1)
    return
end