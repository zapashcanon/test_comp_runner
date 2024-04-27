#define CAML_INTERNALS
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/signals.h>
#undef CAML_INTERNALS

#include <sys/wait.h>
#include <sys/resource.h>

/*   struct rusage { */
/*     struct timeval ru_utime; /\* user CPU time used *\/ */
/*     struct timeval ru_stime; /\* system CPU time used *\/ */
/*     long   ru_maxrss;        /\* maximum resident set size *\/ */
/*     long   ru_ixrss;         /\* integral shared memory size *\/ */
/*     long   ru_idrss;         /\* integral unshared data size *\/ */
/*     long   ru_isrss;         /\* integral unshared stack size *\/ */
/*     long   ru_minflt;        /\* page reclaims (soft page faults) *\/ */
/*     long   ru_majflt;        /\* page faults (hard page faults) *\/ */
/*     long   ru_nswap;         /\* swaps *\/ */
/*     long   ru_inblock;       /\* block input operations *\/ */
/*     long   ru_oublock;       /\* block output operations *\/ */
/*     long   ru_msgsnd;        /\* IPC messages sent *\/ */
/*     long   ru_msgrcv;        /\* IPC messages received *\/ */
/*     long   ru_nsignals;      /\* signals received *\/ */
/*     long   ru_nvcsw;         /\* voluntary context switches *\/ */
/*     long   ru_nivcsw;        /\* involuntary context switches *\/ */
/* }; */

value caml_wait4(value vpid) {
  CAMLparam1(vpid);
  CAMLlocal4(ret, ret_status, vutime, vstime);
  int wstatus;
  struct rusage usage;
  int options = 0;

  int rpid = wait4(Int_val(vpid), &wstatus, options, &usage);

  if (WIFEXITED(wstatus)) {
    ret_status = caml_alloc_1(0, Val_int(WEXITSTATUS(wstatus)));
  }
  else if (WIFSTOPPED(wstatus)) {
    ret_status = caml_alloc_1(2, Val_int(caml_rev_convert_signal_number(WSTOPSIG(wstatus))));
  }
  else {
    ret_status = caml_alloc_1(1, Val_int(caml_rev_convert_signal_number(WTERMSIG(wstatus))));
  }

  double utime = ((double) usage.ru_utime.tv_sec + (double) usage.ru_utime.tv_usec / 1e6);
  vutime = caml_copy_double(utime);

  double stime = ((double) usage.ru_stime.tv_sec + (double) usage.ru_stime.tv_usec / 1e6);
  vstime = caml_copy_double(stime);

  ret = caml_alloc_4(0, Val_int(rpid), ret_status, vutime, vstime);

  CAMLreturn(ret);
}
