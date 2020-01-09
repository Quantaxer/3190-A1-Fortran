! -----------------------------------------------------------------
! Description: luc: The main program that runs
! -----------------------------------------------------------------
program luc 

    implicit none

!   Declare integers and arrays
    integer :: d, hexLength
    integer, dimension(0:7, 0:15) :: k
    integer, dimension(0:7, 0:7, 0:1) :: m
    integer, dimension(0:127) :: key, message
    integer, dimension(0:31) :: kb, mb, rb
    character(len=10) :: word

!   Get user input
    write(*, *) 'Enter key'
    read(*, 1004) kb

    call readWord(word)

    call word2hex(word, mb, len_trim(word), hexLength)

!   Calculate length of hex word
    call printhex(mb, hexLength)

    call expand(message, mb, 32)
    call expand(key, kb, 32) 

    k = reshape(key, (/ 8, 16 /))
    m = reshape(message, (/ 8, 8, 2/))

    d = 0 
    call lucifer(d, k, m) 

!   Display encrpyted message
    write(*, *) 'Encrypted message'
    call compress(m, rb, 32)
    write(*, *) ' ciphertext'
    write(*, 1007) rb

    d = 1 
    call lucifer(d, k, m) 

    call compress(message, mb, 32)
    call compress(key, kb, 32)

!   Display decrypted message
    write(*, *) 'Decrypted message'
    write(*, *) 'key'
    write(*, 1007) kb
    write(*, *) 'plain'
    write(*, 1007) mb

    1004 format(32z1.1)
    1007 format(1x,32z1.1)

end

! -----------------------------------------------------------------
! Description: lucifer: implements the Lucifer algorithm, allowing for both encryption and decryption
! Param d: encipher (0) or decipher (1) (in)
! Param k: key (array of  values,  0 or 1) (in) 
! Param m: message  (array of  values,  0 or 1) (in/out)
! -----------------------------------------------------------------
subroutine lucifer(d, k, m)
    implicit none
    integer :: d, l, ks, kk, kc, jjj, jj, ii, h1, h0, h, v

    integer, dimension(0:7, 0:7, 0:1) :: m
    integer, dimension(0:7, 0:15) :: k
    integer, dimension(0:7) :: tr
    integer, dimension(0:7, 0:7) :: sw

!   diffusion pattern
    integer, dimension(0:7) :: o = (/7,6,2,1,5,0,3,4/)

!   inverse of fixed permutation
    integer, dimension(0:7) :: pr = (/2,5,4,0,3,1,7,6/)

!   S-box permutations
    integer, dimension(0:15) :: s0 = (/12,15,7,10,14,13,11,0,2,6,3,1,9,4,5,8/)
    integer, dimension(0:15) :: s1 = (/7,2,14,9,3,11,0,4,12,13,1,10,6,15,8,5/)

    h0 = 0 
    h1 = 1 

    kc = 0 
    if (d == 1) kc = 8 
    do ii = 1, 16, 1 
        if (d == 1) kc = mod(kc + 1, 16)
        ks = kc 
        do jj = 0, 7, 1
            l = 0 
            h = 0 

            do kk = 0, 3, 1
                l = l * 2 + m(7 - kk, jj, h1)
            end do

            do kk = 4, 7, 1
                h = h * 2 + m(7 - kk, jj, h1)
            end do

            v = (s0(l) + 16 * s1(h)) * (1 - k(jj, ks)) + (s0(h) + 16 * s1(l)) * k(jj, ks)

            do kk = 0, 7, 1
                tr(kk) = mod(v, 2)
                v = v/2
            end do

            do kk = 0, 7, 1
                m(kk, mod(o(kk) + jj, 8), h0) = mod(k(pr(kk), kc) + tr(pr(kk)) + m(kk, mod(o(kk) + jj, 8), h0), 2)
            end do

            if (jj < 7 .or. d == 1) kc = mod(kc + 1, 16)

        end do

        jjj = h0
        h0 = h1 
        h1 = jjj 

    end do

    do jj = 0, 7, 1
        do kk = 0, 7, 1
            sw(kk, jj) = m(kk, jj, 0)
            m(kk, jj, 0) = m(kk, jj, 1)
            m(kk, jj, 1) = sw(kk, jj)
        end do
    end do
    return 
end

! -----------------------------------------------------------------
! Description: expand: expands input bytes into binary array format
! Param a: array in bit array format (out) 
! Param b: array in byte format (in) 
! Param l: length of  array in hexdigits (in)
! -----------------------------------------------------------------
subroutine expand(a, b, l)
    implicit none
    integer :: l, i, j, v
    integer, dimension(0:*) :: a, b

    do i = 0, l - 1, 1
        v = b(i)
        do j = 0, 3, 1
            a((3 - j) + i * 4) = mod(v, 2)
            v = v / 2
        end do
    end do
    return
end

! -----------------------------------------------------------------
! Description: compress: compresses array format back into byte format.
! Param a: array in bit array format (in) 
! Param b: array in byte format (out) 
! Param l: length of  array in hexdigits (in)
! -----------------------------------------------------------------
subroutine compress(a, b, l)
    implicit none
    integer :: l, i, j, v
    integer, dimension(0:*) :: a, b

    do i = 0, l - 1, 1
        v = 0 
        do j = 0, 3, 1
            v = v * 2 + mod(a(j + i * 4), 2)
        end do
        b(i) = v
    end do
    return 
end 