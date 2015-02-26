! Code Written by William Maher
! CIS * 3190
! Reverse Polish notation
! 
! source: Input String
! opstck: Operator stack
! polish: Output polish String

!returns true if character is an operand
logical function isOperand(a) result(b)
  implicit none
    character,intent(in)::a
    if((a.eq.' ').or.(a=='(').or.(a==')').or.(a=='+').or.(a=='-').or.(a=='*').or.(a=='/').or.(a=='^')) then
       !not an operand
        b=.false.
    else
       !is an operand
        b=.true. 
    end if
end function isOperand
!returns precedence value
integer function precedence(c) result(b)
    implicit none
    character::c
    if(c=='(')then
        b=0
    else if(c=='-'.or.c=='+')then
        b=1
    else if(c=='*'.or.c=='/')then
        b=2
    else if(c=='^')then
        b=3
    end if
end function precedence
!Use program in bash like:  ./a.out < test.txt
! You may also use the program like; ./a.out, but
! stdin will be open and so you must enter q to quit.
program wmaher_a1
    implicit none
    integer::i,j,size,precedence,p,q,opstckhead,ioErr
    logical::isOperand
    character(len=64):: source, polish, opstck
    character::currChar
    character::currOPSTCK
    !c is the current character
    !d is the most recent character on the opstck
   
    !Read from text file    
    open(unit=5,action='read')
    do
        !initialize array counters
        opstckhead=1
        size=1
        do i=1,64
            !clear stacks
            polish(i:i)=' '
            opstck(i:i)=' '
            source(i:i)=' '
        end do    
        !Read String
        read (*,"(50a)",iostat=ioErr) source

        if(ioErr>0)then
            !err
            close(5)
            stop
        else if(ioErr<0.OR.source(1:1)=='q')then
            !eof or q to quit
            close(5)
            stop
        else
        print"(a8a40)",' input: ',source
        !normal    
            !loop with iterate through string (no blankspaces)
            do i=1,len(trim(source))
                !set current character from source string.
                currChar=source(i:i)
                if(opstckhead==0)then
                    opstckhead=1
                end if
                if(isOperand(currChar))then
                    !if the character is an operand
                    !put operands in polish
                    polish(size:size)=currChar
                    size=size+1
                else if(currChar/=')')then
                    !otherwise, if it's not a right bracket and an operand
                    if(len(trim(opstck))==0)then
                        !if empty opstck 
                        !prevents memory access errors
                        opstck(1:1)=currChar
                        opstckhead=1
                    else
                        !initialize currOPSTCK character to head of OPSTCK
                        currOPSTCK=opstck(opstckhead:opstckhead)
                        if(precedence(currChar)>precedence(currOPSTCK))then
                            !if current operator has greater precedence
                            !Add operator to stack
                            opstckhead=opstckhead+1
                            opstck(opstckhead:opstckhead)=currChar
                        else if(precedence(currChar)==precedence(currOPSTCK))then
                            !if the precedence is the same
                            !Add most recent OPSTCK to polish
                            !
                            polish(size:size)=currOPSTCK
                            size=size+1
                            !add character to OPSTCK
                            opstck(opstckhead:opstckhead)=currChar
                        else if(precedence(currChar)<precedence(currOPSTCK).AND.currChar/='(')then
                            !If the precedence of the current operator
                            !is greater than what's on the stack
                            !we must empty stack until same or less precedence occurs
                            do while(precedence(currChar)<=precedence(currOPSTCK).AND.opstckhead/=0)
                                !Add OPSTCK to polish
                                polish(size:size)=currOPSTCK
                                size=size+1
                                !move Polish pointer
                                opstck(opstckhead:opstckhead)=' '
                                opstckhead=opstckhead-1
                                !reset opstck character
                                currOPSTCK=opstck(opstckhead:opstckhead)
                            end do
                            !add character to OPSTCK after emptied
                            opstckhead=opstckhead+1  
                            opstck(opstckhead:opstckhead)=currChar
                        else
                            !put left bracket in stack
                            opstckhead=opstckhead+1  
                            opstck(opstckhead:opstckhead)=currChar
                        end if
                    end if
                else
                    !empty opstck onto polish until left bracket
                    currOPSTCK=opstck(opstckhead:opstckhead)
                    do while(currOPSTCK/='(')
                        !put OPSTCK head on polish
                        polish(size:size)=currOPSTCK
                        size = size + 1
                        !move polish pointer
                        opstck(opstckhead:opstckhead)=' '
                        opstckhead = opstckhead - 1
                        currOPSTCK = opstck(opstckhead:opstckhead)
                    end do
                    !remove left bracket from OPSTCK
                    opstck(opstckhead:opstckhead)=' '
                    opstckhead=opstckhead-1
                end if
            end do
            if(len(trim(opstck))/=0)then
            !empty rest of stack
                do i=0,len(trim(opstck))-1
                ! // operator to concatenate strings
                    polish=trim(polish)//opstck(opstckhead-i:opstckhead-i)
                end do
            end if
            print"(a8a40)", ' rpn:   ',polish
        end if
    end do
end program wmaher_a1