program compute
  implicit none
  character(LEN = *),parameter:: tex_file = "quiz.tex"
  character(LEN=*), parameter:: base_file = "base/quiz.tex"
  character(LEN=*), parameter:: operator_file = "operator.txt"
  integer,dimension(2):: wanted = (/0,2/)
  integer, parameter:: initial_state = 2
  integer  middle_state
  integer parameters,last_parameters !parameters inside the sqrt
  integer, parameter:: order=4 !! setting how many a's
  integer n,i,j
  integer,dimension(:,:),allocatable ::operators !1 for creation operators, 0 for annihilation
  character(LEN=1024):: tmp
  !! read operators
  n=0
  open(10,FILE = operator_file)
  do
     read(10,*,ERR=100,END=100) tmp
     n=n+1
  end do
  if (n==0) stop "No operators given."
100 close(10)
  !! just for test
  !n=1
  !!!!!!!!!!!!!!!!!!!!!!!!!
  write(*,*) "Simulating ", n," term of operators with order",order," and initial state ", initial_state
  allocate(operators(order,n))
  open(10,FILE = operator_file)
  do i=1,n
        read(10,*) operators(:,i)
  end do
  !!--Compute part--
  open(10, file = "tmp.tex")
  write(10,*) "\title{升降算符计算}"
  write(10,*) "\begin{document}"
  write(10,*) "\maketitle"
  write(10,*) "$("
  do j=1,n
     do i=1,order
        if (operators(i,j) .eq. 1) then
           write(10,*) "\hat{a}^\dagger"
        else if (operators(i,j).eq. 0) then
           write(10,*) "\hat{a}"
        else
           stop "something wrong with the data"
        end if
     end do
     if (j/=n)  write(10,*) "+"
  end do
  write(10,*) ")"
  !write initial state
  !write(*,*) "order = ",order,"n=",n
  write(10,*) "|",initial_state,"\rangle ="
  !! raise and lower operators computation
  
  do j=1,n ! for the j-th terms
 
       last_parameters = 1 !initialize the last parameters
       middle_state = initial_state !initialize states
       parameters = 1
       do i=order,1,-1 ! for the i-th operator
        if(operators(i,j) .eq. 1) then !if it is a raise operator
           parameters = middle_state +1 !
           middle_state = middle_state +1
        else if (operators(i,j) .eq. 0) then
           parameters = middle_state
           middle_state =middle_state-1
        else
           stop "wrong with the raw data."
        end if
        
        last_parameters = last_parameters *parameters
       ! write(*,*) "middle_state=",middle_state
        end do
        
        if(middle_state >= 0) then
           if(last_parameters/=0) then 
              write(10,*) "\sqrt{",last_parameters,"}|", middle_state,"\rangle"
                   if(j/=n) write(10,*) "+"
           end if
        end if

  end do

  
  write(10,*) "$"
  write(10,*) "\end{document}"
  close(10)
  call system("cat "//base_file//" tmp.tex > "//tex_file)
  call system("xelatex "//tex_file)
  call system("xelatex "//tex_file)
  
           
  
end program compute
