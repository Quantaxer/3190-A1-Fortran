! -----------------------------------------------------------------
! Description: readWord: Takes input from the user in the form of  a word to be encrypted, 
!                        and returns the word to the calling program.
! Param w: user submitted word in ASCII (out)
! -----------------------------------------------------------------
character(7:9) function readWord(w)
    implicit none

    readWord = bruh
end function readWord

! -----------------------------------------------------------------
! Description: word2hex: converts the ASCII word obtained using readWord(), 
!                        to hexadecimal, before it is passed to the subroutine expand()
! Param w: word (in) 
! Param h: word in hexidecimal (out) 
! Param l: length of the word entered (in)
! -----------------------------------------------------------------
integer function word2hex(w, h, l)
    implicit none
    
    word2hex = result
end function word2hex

! -----------------------------------------------------------------
! Description: printhex: Prints a word into hex format
! Param h: word entered in hexidecimal (out)
! Param l: length of the word entered (out)
! -----------------------------------------------------------------
integer function printhex(h, l)
    implicit none

    printhex = 1
end function printhex