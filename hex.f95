! -----------------------------------------------------------------
! Description: readWord: Takes input from the user in the form of  a word to be encrypted, 
!                        and returns the word to the calling program.
! Param word: user submitted word in ASCII (out)
! -----------------------------------------------------------------
subroutine readWord(word)
    implicit none
    character(len=10), intent(out) :: word
    
    write(*, *) 'Enter word to be encrypted (max 10 characters)'
    read(*, 1000) word

    1000 format(A10)
    return
end

! -----------------------------------------------------------------
! Description: word2hex: converts the ASCII word obtained using readWord(), 
!                        to hexadecimal, before it is passed to the subroutine expand()
! Param word: word (in) 
! Param hexWord: word in hexidecimal (out) 
! Param length: length of the word entered (in)
! Param hexLength: length of the resulting hex word (out)
! -----------------------------------------------------------------
subroutine word2hex(word, hexWord, length, hexLength)
    implicit none
    integer, intent(in) :: length
    character(len=length), intent(in) :: word
    integer, dimension(0:length-1), intent(out) :: hexWord
    integer, intent(out) :: hexLength

    integer :: i, j
    integer :: asciiIntValue, hIndex, tempIndex
    real :: remainder
    integer, dimension(0: length) :: asciiWord
    integer, dimension(0:1) :: x


!   Convert char to ASCII int array
    do i = 1, length + 1
        if (i == length + 1) then
            asciiWord(i - 1) = 0
        else
            asciiWord(i - 1) = iachar(word(i:i))
        end if
    end do
    
!   Keep track of where in the hex string to insert next value
    hIndex = 0

!   Logic for converting a word to hex. Loop through every letter in the word
    do i = 0, length - 1
        asciiIntValue = asciiWord(i)
        tempIndex = 1

!       This loop is to convert the decimal to hex
        do
            if (asciiIntValue <= 0) exit
            remainder = modulo(asciiIntValue, 16)
            asciiIntValue = asciiIntValue / 16

!           I do this because when converting decimal to hex, the desired values are reversed, so we load it into a temp array in reverse
!           This way when reading it front to back we have the correct value
            x(tempIndex) = int(remainder)
            tempIndex = tempIndex - 1
        end do

!       Load in the temp array with the hex value into the new hex array
        do j = 0, 1
            hexWord(hIndex) = x(j)
            hIndex = hIndex + 1
        end do
    end do

    hexLength = hIndex

!   Any remaining values should be set to 0
    do i = hIndex, 32
        hexWord(i) = 0
    end do

    return
end

! -----------------------------------------------------------------
! Description: printhex: Prints a word into hex format
! Param hexWord: word entered in hexidecimal (out)
! Param length: length of the word entered (in)
! -----------------------------------------------------------------
subroutine printhex(hexWord, length)
    implicit none
    integer, intent(in) :: length
    integer, dimension(0:31), intent(out) :: hexWord

    integer :: i
    integer, dimension(0:length - 1) :: formattedWord

    do i = 0, length - 1
        formattedWord(i) = hexWord(i)
    end do

    write(*,1007) formattedWord
    
    1007 format(1x,32z1.1)
    return
end
