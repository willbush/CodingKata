# This program promps user for size of the array,
# which must be a power of 2 at most 32 size. Then the program
# ask the user to input the values of the array. The program then
# merge sorts the array and displaces the sorted array.

  .data

list:
  .space 128                 # enough space for 32 integers
subListA:
  .space 64                  # enough space for 16 integers
subListB:
  .space 64                  # enough space for 16 integers
ask_size:
  .asciiz "How many integers in your list?\n"
incorrect_size:
  .asciiz "Size must be a power of 2 but less than or equal to 32\n"
ask_int:
  .asciiz "\nPlease enter the next int: "
space:                       # using space as a delimiter
  .asciiz " "

  .text
  .globl main

main:
  la   $s1, list             # initialize addresses
  la   $s2, subListA
  la   $s3, subListB

# section: get input and validate
  li   $v0, 4
  la   $a0, ask_size
  syscall                    # ask list size

get_size:
  li   $v0, 5
  syscall
  move $s0, $v0              # store size input

  li   $t1, 2
  div  $s0, $t1              # mod input by 2
  mfhi $t1                   # store mod result
  bnez $t1, ask_size_again   # size must be divisible by 2
  li   $t1, 33
  slt  $t1, $s0, $t1         # check if the input size is strictly less than 33
  beqz $t1, ask_size_again

  move $t0, $zero
  j    for_size_get_int

ask_size_again:
  li   $v0, 4
  la   $a0, incorrect_size
  syscall                    # ask for list size correction
  j    get_size

for_size_get_int:            # for i < size ask and store int
  sll  $t1, $t0, 2           # shift to the word at the index
  add  $t1, $s1, $t1         # set the address of the list[i]

  li   $v0, 4
  la   $a0, ask_int
  syscall                    # ask for int to enter into list

  li   $v0, 5
  syscall
  sw   $v0, 0($t1)           # store the int into the list
  addi $t0, $t0, 1           # i++
  slt  $t1, $t0, $s0         # set (i < size)
  bnez $t1, for_size_get_int # branch if true

# section: mergeSort
  li   $s4, 2                # let powerOf2 = 2

for_powerOf2:

  move $s5, $zero            # let i = 0
fori:
  srl  $s6, $s4, 1           # subListLength = powerOf2 / 2

  move $a0, $s5              # put i into argument 0
  add  $a1, $s5, $s6         # i + subListLength into arg 1
  jal  copyToSubList

  move $a0, $s5              # put i into arg0
  jal  merge

  add  $s5, $s5, $s4         # i += powerOf2
  slt  $v0, $s5, $s0         # i < list.length
  bnez $v0, fori

  sll  $s4, $s4, 1           # powerOf2 *= 2
  sle  $v0, $s4, $s0         # powerOf2 <= list.length
  bnez $v0, for_powerOf2     # branch if expression is true

  jal printFinalList
  j   exit

# copyToSubList subroutine
copyToSubList:               # a0 = a index, a1 = b index
  move $t0, $zero            # let i = 0
list_fori:
  sll  $t1, $t0, 2           # store i * 4
  add  $t2, $s2, $t1         # set address of subListA[i*4]
  add  $t3, $s3, $t1         # set address of subListB[i*4]

  sll  $t1, $a0, 2           # store a * 4
  add  $t4, $s1, $t1         # set address of list[a*4]
  sll  $t1, $a1, 2           # store b * 4
  add  $t5, $s1, $t1         # set address of list[b*4]
  lw   $t4, 0($t4)           # load value of list[a]
  lw   $t5, 0($t5)           # load value of list[b]
  sw   $t4, 0($t2)           # subListA[i] = list[a]
  sw   $t5, 0($t3)           # subListB[i] = list[b]

  addi $a0, $a0, 1           # a++
  addi $a1, $a1, 1           # b++
  addi $t0, $t0, 1           # i++

  slt  $v0, $t0, $s6         # i < subListLength
  bnez $v0, list_fori        # branch if expression is true
  jr   $ra

# merge subroutine
merge:                       # $a0 = i,
  move $t0, $zero            # let a = 0 be the subListA index
  move $t1, $zero            # let b = 0 be the subListB index
  sll  $t3, $s6, 1           # length = 2 * subListLength
  add  $t2, $a0, $t3         # let listEndPoint = index + length

merge_fori:                  # loops for the length of the list
  sll  $t3, $a0, 2           # scale interators to word size
  sll  $t6, $t0, 2
  sll  $t7, $t1, 2
  add  $t3, $s1, $t3         # set address of list[i]
  add  $t6, $s2, $t6         # set address of subListA[a]
  add  $t7, $s3, $t7         # set address of subListB[b]
  lw   $t8, 0($t6)           # load value for subListA[a]
  lw   $t9, 0($t7)           # load value for subListB[b]

  slt  $t4, $t0, $s6         # set aHasNext = a < subListLength
  slt  $t5, $t1, $s6         # set bHasNext = a < subListLength
  and  $v0, $t4, $t5         # (aHasNext && bHasNext)
  beq  $v0, $zero, elseIf    # branch if expression is false

  slt  $v0, $t8, $t9         # (subListA[a] < subListB[b])
  beq  $v0, $zero, innerElse # branch to innerElse if not:
  sw   $t8, 0($t3)           # list[i] = subListA[a]
  addi $t0, $t0, 1           # a++
  j    endIf

innerElse:
  sw   $t9, 0($t3)           # list[i] = subListB[b]
  addi $t1, $t1, 1           # b++
  j    endIf

elseIf:                      # else if (aHasNext)
  beq  $t4, $zero, elseIf2
  sw   $t8, 0($t3)           # list[i] = subListA[a]
  addi $t0, $t0, 1           # a++
  j    endIf

elseIf2:
  beq  $t5, $zero, endIf     # else if (bHasNext)
  sw   $t9, 0($t3)           # list[i] = subListB[b]
  addi $t1, $t1, 1           # b++
  j    endIf

endIf:
  addi $a0, $a0, 1           # i++
  slt  $v0, $a0, $t2         # i < listEndPoint
  bnez $v0, merge_fori       # branch if expression is true
  jr   $ra

# printFinalList subroutine
printFinalList:
  move $t0, $zero            # let i = 0

print_fori:
  sll  $t1, $t0, 2           # scale i to word size
  add  $t1, $s1, $t1         # set address of list[i]
  lw   $t1, 0($t1)           # load value of list[i]
  li   $v0, 1
  move $a0, $t1
  syscall                    # print list[i]

  li   $v0, 4
  la   $a0, space
  syscall                    # print a space delimiter

  addi $t0, $t0, 1           # i++
  slt  $v0, $t0, $s0         # i < list.length
  bnez $v0, print_fori       # branch if true
  jr   $ra

exit:
  li   $v0, 10
  syscall                    # exit
