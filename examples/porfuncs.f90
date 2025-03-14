MODULE PORFUNCS
     implicit none
     contains
          subroutine iargc_subr(n) ! wraps iargc function so it can be use-associated
               implicit none
               integer,intent(out) :: n
 
               integer iargc
 
               n = iargc()
          end subroutine iargc_subr
 
          subroutine fork_subr(pid) ! returns zero in the child process, PID of child in parent process
               implicit none
               integer, intent(inout) :: pid
 
               integer fork
 
               pid=fork()
          end subroutine fork_subr
 
          subroutine system_subr(JobString,ExitStatus)
               implicit none
 
               character(len=*),intent(in) :: JobString
               integer,intent(out) :: ExitStatus
 
               integer system
 
               ExitStatus=system(JobString)
               ExitStatus=ishft(ExitStatus,-8)
          end subroutine system_subr
 
          subroutine wait_subr(pid,ExitStatus)
               implicit none
 
               integer,intent(inout) :: pid,ExitStatus
 
               integer wait
 
               pid=wait(ExitStatus)
               ExitStatus=ishft(ExitStatus,-8)
          end subroutine wait_subr
 
          subroutine getpid_subr(pid)
               implicit none
 
               integer,intent(out) :: pid
 
               integer getpid
 
               pid=getpid()
          end subroutine getpid_subr
END MODULE PORFUNCS
