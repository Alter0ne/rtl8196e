$(OBJDIR)/erl_sched_spec_pre_alloc.o: beam/erl_sched_spec_pre_alloc.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/erl_process.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_process_lock.h beam/erl_port.h \
 beam/erl_vm.h beam/erl_message.h beam/erl_proc_sig_queue.h \
 beam/external.h beam/erl_node_tables.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_process_dict.h beam/erl_node_container_utils.h beam/erl_ptab.h \
 beam/erl_time.h beam/erl_monitor_link.h beam/erl_utils.h beam/hash.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 beam/erl_drv_nif.h ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_binary.h beam/erl_hl_timer.h \
 $(TTF_DIR)/erl_atom_table.h beam/erl_async.h \
 beam/erl_gc.h beam/erl_trace.h beam/erl_db.h beam/export.h beam/index.h \
 beam/code_ix.h $(TTF_DIR)/beam_opcodes.h \
 beam/beam_load.h beam/erl_bits.h
$(OBJDIR)/erl_term.o: beam/erl_term.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 beam/erl_map.h
$(OBJDIR)/erl_printf_term.o: beam/erl_printf_term.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/erl_printf_term.h ../include/internal/erl_printf_format.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h \
 ../include/internal/erl_printf.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/big.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 beam/erl_binary.h beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h \
 beam/erl_trace.h beam/erl_db.h beam/erl_bits.h beam/module.h \
 beam/register.h beam/erl_fun.h beam/erl_debug.h beam/error.h \
 beam/erl_io_queue.h beam/erl_nif.h beam/erl_nif_api_funcs.h \
 beam/dtrace-wrapper.h beam/erl_map.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_bif_unique.h
$(OBJDIR)/erl_bif_ddll.o: beam/erl_bif_ddll.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/big.h beam/dist.h \
 x86_64-pc-linux-gnu/erl_version.h beam/erl_bif_unique.h
$(OBJDIR)/external.o: beam/external.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/big.h beam/dist.h \
 beam/erl_bif_unique.h beam/erl_zlib.h beam/erl_map.h
$(OBJDIR)/erl_bestfit_alloc.o: beam/erl_bestfit_alloc.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 beam/erl_bestfit_alloc.h
$(OBJDIR)/erl_bif_lists.o: beam/erl_bif_lists.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h
$(OBJDIR)/erl_proc_sig_queue.o: beam/erl_proc_sig_queue.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 beam/dist.h beam/beam_bp.h beam/big.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_bif_unique.h
$(OBJDIR)/erl_ptab.o: beam/erl_ptab.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/erl_ptab.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_time.h beam/erl_monitor_link.h \
 beam/erl_proc_sig_queue.h beam/erl_utils.h beam/erl_thr_progress.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_port.h beam/erl_vm.h beam/erl_message.h \
 beam/external.h beam/erl_node_tables.h beam/erl_process_dict.h \
 beam/erl_node_container_utils.h beam/hash.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h $(TTF_DIR)/erl_atom_table.h \
 beam/erl_async.h beam/erl_gc.h beam/erl_trace.h beam/erl_db.h \
 beam/export.h beam/index.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/beam_load.h \
 beam/erl_bits.h beam/bif.h $(TTF_DIR)/erl_bif_table.h \
 beam/global.h beam/atom.h beam/module.h beam/register.h beam/erl_fun.h \
 beam/erl_debug.h beam/error.h beam/erl_io_queue.h beam/erl_nif.h \
 beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/erl_bif_unique.h \
 beam/big.h
$(OBJDIR)/erl_lock_flags.o: beam/erl_lock_flags.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/erl_lock_flags.h
$(OBJDIR)/erl_afit_alloc.o: beam/erl_afit_alloc.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 beam/erl_afit_alloc.h
$(OBJDIR)/erl_bits.o: beam/erl_bits.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/big.h \
 beam/erl_bif_unique.h
$(OBJDIR)/erl_bif_guard.o: beam/erl_bif_guard.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/big.h \
 beam/erl_bif_unique.h beam/erl_map.h
$(OBJDIR)/erl_zlib.o: beam/erl_zlib.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/erl_zlib.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h
$(OBJDIR)/erl_bif_binary.o: beam/erl_bif_binary.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/big.h \
 beam/erl_bif_unique.h
$(OBJDIR)/erl_process_dump.o: beam/erl_process_dump.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_db_util.h \
 beam/erl_bif_unique.h beam/big.h beam/erl_db_hash.h beam/erl_db_tree.h \
 beam/dist.h beam/beam_catches.h beam/erl_map.h
$(OBJDIR)/erl_db.o: beam/erl_db.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_db_util.h \
 beam/erl_bif_unique.h beam/big.h beam/erl_db_hash.h beam/erl_db_tree.h \
 beam/erl_rbtree.h
$(OBJDIR)/erl_lock_check.o: beam/erl_lock_check.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h
$(OBJDIR)/erl_bif_re.o: beam/erl_bif_re.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_bif_unique.h \
 beam/big.h pcre/pcre.h
$(OBJDIR)/erl_bif_chksum.o: beam/erl_bif_chksum.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_bif_unique.h \
 beam/big.h
$(OBJDIR)/erl_alloc_util.o: beam/erl_alloc_util.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/big.h \
 beam/erl_mtrace.h beam/erl_bif_unique.h beam/erl_rbtree.h
$(OBJDIR)/hash.o: beam/hash.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/hash.h
$(OBJDIR)/erl_time_sup.o: beam/erl_time_sup.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/big.h \
 beam/bif.h $(TTF_DIR)/erl_bif_table.h
$(OBJDIR)/erl_db_hash.o: beam/erl_db_hash.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_db_util.h \
 beam/erl_bif_unique.h beam/big.h beam/erl_db_hash.h beam/erl_db_tree.h
$(OBJDIR)/beam_bp.o: beam/beam_bp.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_bif_unique.h \
 beam/big.h beam/beam_bp.h beam/erl_nfunc_sched.h
$(OBJDIR)/erl_thr_progress.o: beam/erl_thr_progress.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/erl_thr_progress.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_vm.h beam/erl_message.h \
 beam/erl_proc_sig_queue.h beam/external.h beam/erl_node_tables.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_process_dict.h beam/erl_node_container_utils.h beam/erl_ptab.h \
 beam/erl_time.h beam/erl_monitor_link.h beam/erl_utils.h beam/hash.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 beam/erl_drv_nif.h ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_binary.h beam/erl_hl_timer.h \
 $(TTF_DIR)/erl_atom_table.h beam/erl_async.h \
 beam/erl_gc.h beam/erl_trace.h beam/erl_db.h beam/export.h beam/index.h \
 beam/code_ix.h $(TTF_DIR)/beam_opcodes.h \
 beam/beam_load.h beam/erl_bits.h beam/global.h beam/atom.h beam/module.h \
 beam/register.h beam/erl_fun.h beam/erl_debug.h beam/error.h \
 beam/erl_io_queue.h beam/erl_nif.h beam/erl_nif_api_funcs.h \
 beam/dtrace-wrapper.h
$(OBJDIR)/erl_md5.o: beam/erl_md5.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_bif_unique.h \
 beam/big.h
$(OBJDIR)/export.o: beam/export.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h
$(OBJDIR)/erl_bif_info.o: beam/erl_bif_info.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h \
 $(TTF_DIR)/erl_alloc_types.h \
 ../include/internal/ethread.h ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 beam/erl_thr_progress.h beam/erl_alloc_util.h sys/common/erl_mseg.h \
 sys/common/erl_mmap.h beam/lttng-wrapper.h \
 beam/erl_sched_spec_pre_alloc.h beam/erl_node_container_utils.h \
 beam/erl_ptab.h beam/erl_time.h beam/erl_monitor_link.h \
 beam/erl_proc_sig_queue.h beam/erl_utils.h beam/hash.h beam/index.h \
 beam/atom.h $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/big.h \
 x86_64-pc-linux-gnu/erl_version.h \
 $(TTF_DIR)/erl_compile_flags.h beam/erl_db_util.h \
 beam/erl_bif_unique.h beam/erl_db_hash.h beam/erl_db_tree.h \
 beam/erl_mtrace.h beam/dist.h beam/erl_cpu_topology.h beam/erl_map.h \
 sys/common/erl_check_io.h beam/erl_sys_driver.h
$(OBJDIR)/erl_cpu_topology.o: beam/erl_cpu_topology.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_cpu_topology.h
$(OBJDIR)/erl_async.o: beam/erl_async.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_sys_driver.h beam/erl_driver.h \
 beam/erl_drv_nif.h ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h \
 beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_binary.h beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h \
 beam/erl_trace.h beam/erl_db.h beam/erl_bits.h beam/module.h \
 beam/register.h beam/erl_fun.h beam/erl_debug.h beam/error.h \
 beam/erl_io_queue.h beam/erl_nif.h beam/erl_nif_api_funcs.h \
 beam/dtrace-wrapper.h beam/erl_thr_queue.h
$(OBJDIR)/erl_bif_os.o: beam/erl_bif_os.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/big.h beam/dist.h \
 x86_64-pc-linux-gnu/erl_version.h
$(OBJDIR)/erl_lock_count.o: beam/erl_lock_count.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h
$(OBJDIR)/erl_bif_unique.o: beam/erl_bif_unique.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/export.h \
 beam/index.h beam/hash.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/beam_load.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_port.h \
 beam/erl_message.h beam/erl_proc_sig_queue.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_utils.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h $(TTF_DIR)/erl_atom_table.h \
 beam/erl_async.h beam/erl_gc.h beam/erl_trace.h beam/erl_db.h \
 beam/erl_bits.h beam/bif.h $(TTF_DIR)/erl_bif_table.h \
 beam/erl_bif_unique.h beam/big.h beam/global.h beam/atom.h beam/module.h \
 beam/register.h beam/erl_fun.h beam/erl_debug.h beam/error.h \
 beam/erl_io_queue.h beam/erl_nif.h beam/erl_nif_api_funcs.h \
 beam/dtrace-wrapper.h
$(OBJDIR)/binary.o: beam/binary.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/big.h \
 beam/erl_bif_unique.h
$(OBJDIR)/erl_nif.o: beam/erl_nif.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/erl_nif.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_nif_api_funcs.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_bif_unique.h \
 beam/big.h beam/erl_map.h beam/beam_bp.h beam/erl_nfunc_sched.h
$(OBJDIR)/erl_alloc.o: beam/erl_alloc.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h \
 $(TTF_DIR)/erl_alloc_types.h \
 ../include/internal/ethread.h ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/global.h beam/erl_alloc.h \
 beam/erl_thr_progress.h beam/erl_alloc_util.h sys/common/erl_mseg.h \
 sys/common/erl_mmap.h beam/lttng-wrapper.h \
 beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_db_util.h \
 beam/erl_bif_unique.h beam/big.h beam/erl_db_hash.h beam/erl_db_tree.h \
 beam/erl_mtrace.h beam/erl_cpu_topology.h beam/erl_thr_queue.h \
 beam/erl_nfunc_sched.h sys/common/erl_check_io.h beam/erl_sys_driver.h \
 sys/common/erl_poll.h sys/common/erl_poll_api.h beam/erl_port_task.h \
 beam/erl_goodfit_alloc.h beam/erl_bestfit_alloc.h beam/erl_afit_alloc.h \
 beam/erl_ao_firstfit_alloc.h
$(OBJDIR)/utils.o: beam/utils.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_map.h \
 beam/erl_fun.h beam/erl_bits.h beam/erl_trace.h beam/erl_db.h \
 beam/module.h beam/register.h beam/erl_debug.h beam/error.h \
 beam/erl_io_queue.h beam/erl_nif.h beam/erl_nif_api_funcs.h \
 beam/dtrace-wrapper.h beam/big.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_bif_unique.h \
 beam/packet_parser.h beam/erl_driver.h beam/erl_db_util.h \
 beam/erl_db_hash.h beam/erl_db_tree.h beam/dist.h beam/erl_thr_queue.h \
 beam/beam_bp.h sys/common/erl_check_io.h beam/erl_sys_driver.h \
 beam/erl_nfunc_sched.h
$(OBJDIR)/erl_bif_trace.o: beam/erl_bif_trace.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/big.h beam/dist.h \
 x86_64-pc-linux-gnu/erl_version.h beam/beam_bp.h beam/erl_bif_unique.h
$(OBJDIR)/erl_port_task.o: beam/erl_port_task.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 beam/dist.h sys/common/erl_check_io.h beam/erl_sys_driver.h
$(OBJDIR)/break.o: beam/break.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 beam/version.h beam/bif.h $(TTF_DIR)/erl_bif_table.h \
 beam/erl_db_util.h beam/erl_bif_unique.h beam/big.h beam/erl_db_hash.h \
 beam/erl_db_tree.h x86_64-pc-linux-gnu/erl_version.h
$(OBJDIR)/beam_bif_load.o: beam/beam_bif_load.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/big.h beam/beam_bp.h \
 beam/beam_catches.h beam/erl_bif_unique.h beam/erl_nfunc_sched.h
$(OBJDIR)/erl_bif_port.o: beam/erl_bif_port.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/erl_sys_driver.h \
 beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_thr_progress.h beam/erl_alloc_util.h sys/common/erl_mseg.h \
 sys/common/erl_mmap.h beam/lttng-wrapper.h \
 beam/erl_sched_spec_pre_alloc.h beam/erl_node_container_utils.h \
 beam/erl_ptab.h beam/erl_time.h beam/erl_monitor_link.h \
 beam/erl_proc_sig_queue.h beam/erl_utils.h beam/hash.h beam/index.h \
 beam/atom.h $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_binary.h beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h \
 beam/erl_trace.h beam/erl_db.h beam/erl_bits.h beam/module.h \
 beam/register.h beam/erl_fun.h beam/erl_debug.h beam/error.h \
 beam/erl_io_queue.h beam/erl_nif.h beam/erl_nif_api_funcs.h \
 beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/big.h beam/dist.h \
 x86_64-pc-linux-gnu/erl_version.h beam/erl_bif_unique.h \
 beam/erl_db_util.h beam/packet_parser.h beam/erl_driver.h
$(OBJDIR)/beam_emu.o: beam/beam_emu.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/big.h \
 beam/erl_bif_unique.h beam/erl_map.h beam/dist.h beam/beam_bp.h \
 beam/beam_catches.h beam/erl_nfunc_sched.h \
 $(TTF_DIR)/beam_hot.h \
 $(TTF_DIR)/beam_warm.h \
 $(TTF_DIR)/beam_cold.h
$(OBJDIR)/erl_mtrace.o: beam/erl_mtrace.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 beam/erl_sock.h ../include/internal/erl_memory_trace_protocol.h \
 beam/erl_mtrace.h
$(OBJDIR)/erl_math.o: beam/erl_math.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/big.h
$(OBJDIR)/dist.o: beam/dist.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 beam/dist.h beam/bif.h $(TTF_DIR)/erl_bif_table.h \
 beam/erl_bif_unique.h beam/big.h
$(OBJDIR)/beam_ranges.o: beam/beam_ranges.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 beam/erl_unicode.h
$(OBJDIR)/code_ix.o: beam/code_ix.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/code_ix.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h $(TTF_DIR)/beam_opcodes.h \
 beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 beam/beam_catches.h
$(OBJDIR)/copy.o: beam/copy.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_map.h \
 beam/erl_fun.h beam/erl_bits.h beam/erl_trace.h beam/erl_db.h \
 beam/module.h beam/register.h beam/erl_debug.h beam/error.h \
 beam/erl_io_queue.h beam/erl_nif.h beam/erl_nif_api_funcs.h \
 beam/dtrace-wrapper.h beam/big.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_bif_unique.h
$(OBJDIR)/erl_debug.o: beam/erl_debug.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/big.h \
 beam/bif.h $(TTF_DIR)/erl_bif_table.h \
 beam/beam_catches.h beam/erl_map.h
$(OBJDIR)/erlang_lttng.o: beam/erlang_lttng.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h
$(OBJDIR)/erl_arith.o: beam/erl_arith.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/big.h
$(OBJDIR)/erl_posix_str.o: beam/erl_posix_str.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 ../include/internal/erl_errno.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h
$(OBJDIR)/index.o: beam/index.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h
$(OBJDIR)/erl_io_queue.o: beam/erl_io_queue.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_bif_unique.h \
 beam/big.h
$(OBJDIR)/erl_gc.o: beam/erl_gc.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_map.h \
 beam/erl_fun.h beam/erl_bits.h beam/erl_trace.h beam/erl_db.h \
 beam/module.h beam/register.h beam/erl_debug.h beam/error.h \
 beam/erl_io_queue.h beam/erl_nif.h beam/erl_nif_api_funcs.h \
 beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_db_util.h \
 beam/erl_bif_unique.h beam/big.h beam/erl_db_hash.h beam/erl_db_tree.h \
 beam/beam_catches.h beam/dist.h beam/erl_nfunc_sched.h
$(OBJDIR)/erl_process.o: beam/erl_process.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_db_util.h \
 beam/erl_bif_unique.h beam/big.h beam/erl_db_hash.h beam/erl_db_tree.h \
 beam/dist.h beam/beam_catches.h beam/beam_bp.h beam/erl_cpu_topology.h \
 beam/erl_thr_queue.h beam/erl_nfunc_sched.h sys/common/erl_check_io.h \
 beam/erl_sys_driver.h sys/common/erl_poll.h sys/common/erl_poll_api.h
$(OBJDIR)/erl_db_util.o: beam/erl_db_util.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_db_util.h \
 beam/erl_bif_unique.h beam/big.h beam/erl_db_hash.h beam/erl_db_tree.h \
 beam/erl_map.h
$(OBJDIR)/bif.o: beam/bif.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/erl_sys_driver.h \
 beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_thr_progress.h beam/erl_alloc_util.h sys/common/erl_mseg.h \
 sys/common/erl_mmap.h beam/lttng-wrapper.h \
 beam/erl_sched_spec_pre_alloc.h beam/erl_node_container_utils.h \
 beam/erl_ptab.h beam/erl_time.h beam/erl_monitor_link.h \
 beam/erl_proc_sig_queue.h beam/erl_utils.h beam/hash.h beam/index.h \
 beam/atom.h $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_binary.h beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h \
 beam/erl_trace.h beam/erl_db.h beam/erl_bits.h beam/module.h \
 beam/register.h beam/erl_fun.h beam/erl_debug.h beam/error.h \
 beam/erl_io_queue.h beam/erl_nif.h beam/erl_nif_api_funcs.h \
 beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/big.h beam/dist.h \
 x86_64-pc-linux-gnu/erl_version.h beam/erl_bif_unique.h beam/beam_bp.h \
 beam/erl_db_util.h beam/erl_map.h beam/erl_nfunc_sched.h
$(OBJDIR)/erl_drv_thread.o: beam/erl_drv_thread.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h
$(OBJDIR)/erl_ao_firstfit_alloc.o: beam/erl_ao_firstfit_alloc.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 beam/erl_ao_firstfit_alloc.h
$(OBJDIR)/erl_message.o: beam/erl_message.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_bif_unique.h \
 beam/big.h beam/beam_bp.h
$(OBJDIR)/big.o: beam/big.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/big.h \
 beam/bif.h $(TTF_DIR)/erl_bif_table.h
$(OBJDIR)/erl_unicode.o: beam/erl_unicode.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_bif_unique.h \
 beam/big.h beam/erl_unicode.h beam/erl_unicode_normalize.h
$(OBJDIR)/register.o: beam/register.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h
$(OBJDIR)/erl_init.o: beam/erl_init.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 x86_64-pc-linux-gnu/erl_version.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_db_util.h \
 beam/erl_bif_unique.h beam/big.h beam/erl_db_hash.h beam/erl_db_tree.h \
 beam/beam_bp.h beam/dist.h beam/erl_mtrace.h beam/erl_printf_term.h \
 ../include/internal/erl_printf_format.h ../include/internal/erl_printf.h \
 beam/packet_parser.h beam/erl_driver.h beam/erl_cpu_topology.h \
 beam/erl_thr_queue.h sys/common/erl_check_io.h beam/erl_sys_driver.h
$(OBJDIR)/erl_nfunc_sched.o: beam/erl_nfunc_sched.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_nfunc_sched.h
$(OBJDIR)/erl_map.o: beam/erl_map.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_bif_unique.h \
 beam/big.h beam/erl_map.h
$(OBJDIR)/erl_fun.o: beam/erl_fun.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h
$(OBJDIR)/atom.o: beam/atom.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_sys_driver.h beam/erl_driver.h \
 beam/erl_drv_nif.h ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_binary.h beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h \
 beam/erl_trace.h beam/erl_db.h beam/erl_bits.h beam/module.h \
 beam/register.h beam/erl_fun.h beam/erl_debug.h beam/error.h \
 beam/erl_io_queue.h beam/erl_nif.h beam/erl_nif_api_funcs.h \
 beam/dtrace-wrapper.h
$(OBJDIR)/module.o: beam/module.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 beam/beam_catches.h
$(OBJDIR)/beam_catches.o: beam/beam_catches.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/beam_catches.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_thr_progress.h beam/erl_alloc_util.h sys/common/erl_mseg.h \
 sys/common/erl_mmap.h beam/lttng-wrapper.h \
 beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h
$(OBJDIR)/erl_node_tables.o: beam/erl_node_tables.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 beam/dist.h beam/big.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_bif_unique.h \
 beam/erl_db_util.h beam/erl_db_hash.h beam/erl_db_tree.h
$(OBJDIR)/erl_msacc.o: beam/erl_msacc.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 beam/erl_bif_unique.h beam/big.h beam/erl_map.h \
 $(TTF_DIR)/erl_bif_table.h
$(OBJDIR)/io.o: beam/io.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_sys_driver.h beam/erl_driver.h \
 beam/erl_drv_nif.h ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/erl_vm.h beam/global.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_thr_progress.h beam/erl_alloc_util.h sys/common/erl_mseg.h \
 sys/common/erl_mmap.h beam/lttng-wrapper.h \
 beam/erl_sched_spec_pre_alloc.h beam/erl_node_container_utils.h \
 beam/erl_ptab.h beam/erl_time.h beam/erl_monitor_link.h \
 beam/erl_proc_sig_queue.h beam/erl_utils.h beam/hash.h beam/index.h \
 beam/atom.h $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_binary.h beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h \
 beam/erl_trace.h beam/erl_db.h beam/erl_bits.h beam/module.h \
 beam/register.h beam/erl_fun.h beam/erl_debug.h beam/error.h \
 beam/erl_io_queue.h beam/dtrace-wrapper.h beam/dist.h beam/big.h \
 beam/bif.h $(TTF_DIR)/erl_bif_table.h \
 beam/erl_bif_unique.h x86_64-pc-linux-gnu/erl_version.h beam/erl_map.h
$(OBJDIR)/beam_load.o: beam/beam_load.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 x86_64-pc-linux-gnu/erl_version.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/beam_bp.h beam/big.h \
 beam/beam_catches.h beam/erl_bif_unique.h beam/erl_zlib.h beam/erl_map.h \
 beam/erl_unicode.h $(TTF_DIR)/beam_pred_funcs.h \
 $(TTF_DIR)/beam_tr_funcs.h
$(OBJDIR)/erl_monitor_link.o: beam/erl_monitor_link.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 beam/erl_rbtree.h
$(OBJDIR)/erl_goodfit_alloc.o: beam/erl_goodfit_alloc.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 beam/erl_goodfit_alloc.h
$(OBJDIR)/erl_thr_queue.o: beam/erl_thr_queue.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/erl_thr_queue.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_port.h beam/erl_vm.h beam/erl_message.h \
 beam/erl_proc_sig_queue.h beam/external.h beam/erl_node_tables.h \
 beam/erl_process_dict.h beam/erl_node_container_utils.h beam/erl_ptab.h \
 beam/erl_time.h beam/erl_monitor_link.h beam/erl_utils.h beam/hash.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 beam/erl_drv_nif.h ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_binary.h beam/erl_hl_timer.h \
 $(TTF_DIR)/erl_atom_table.h beam/erl_async.h \
 beam/erl_gc.h beam/erl_trace.h beam/erl_db.h beam/export.h beam/index.h \
 beam/code_ix.h $(TTF_DIR)/beam_opcodes.h \
 beam/beam_load.h beam/erl_bits.h
$(OBJDIR)/packet_parser.o: beam/packet_parser.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/packet_parser.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/sys.h \
 sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h
$(OBJDIR)/time.o: beam/time.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h
$(OBJDIR)/beam_debug.o: beam/beam_debug.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/big.h beam/beam_bp.h \
 beam/erl_bif_unique.h beam/erl_nfunc_sched.h beam/beam_catches.h
$(OBJDIR)/elib_memmove.o: beam/elib_memmove.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h
$(OBJDIR)/erl_process_lock.o: beam/erl_process_lock.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/erl_process.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_process_lock.h beam/erl_port.h \
 beam/erl_vm.h beam/erl_message.h beam/erl_proc_sig_queue.h \
 beam/external.h beam/erl_node_tables.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_process_dict.h beam/erl_node_container_utils.h beam/erl_ptab.h \
 beam/erl_time.h beam/erl_monitor_link.h beam/erl_utils.h beam/hash.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 beam/erl_drv_nif.h ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_binary.h beam/erl_hl_timer.h \
 $(TTF_DIR)/erl_atom_table.h beam/erl_async.h \
 beam/erl_gc.h beam/erl_trace.h beam/erl_db.h beam/export.h beam/index.h \
 beam/code_ix.h $(TTF_DIR)/beam_opcodes.h \
 beam/beam_load.h beam/erl_bits.h
$(OBJDIR)/erl_bif_op.o: beam/erl_bif_op.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/big.h beam/dist.h \
 x86_64-pc-linux-gnu/erl_version.h beam/erl_bif_unique.h beam/erl_map.h
$(OBJDIR)/safe_hash.o: beam/safe_hash.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/safe_hash.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h
$(OBJDIR)/erl_hl_timer.o: beam/erl_hl_timer.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_bif_unique.h \
 beam/big.h beam/erl_rbtree.h
$(OBJDIR)/erl_db_tree.o: beam/erl_db_tree.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/erl_db_util.h \
 beam/erl_bif_unique.h beam/big.h beam/erl_db_hash.h beam/erl_db_tree.h
$(OBJDIR)/erl_process_dict.o: beam/erl_process_dict.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/big.h beam/dist.h \
 x86_64-pc-linux-gnu/erl_version.h
$(OBJDIR)/erl_trace.o: beam/erl_trace.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/erl_threads.h beam/erl_lock_flags.h \
 beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/big.h \
 beam/bif.h $(TTF_DIR)/erl_bif_table.h beam/dist.h \
 beam/beam_bp.h beam/erl_bif_unique.h beam/erl_map.h
$(OBJDIR)/ram_file_drv.o: drivers/common/ram_file_drv.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h \
 drivers/common/gzio.h
$(OBJDIR)/inet_drv.o: drivers/common/inet_drv.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/sys.h \
 sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/packet_parser.h beam/erl_sock.h
$(OBJDIR)/gzio.o: drivers/common/gzio.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/sys.h \
 sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h drivers/common/gzio_zutil.h beam/erl_zlib.h \
 drivers/common/gzio.h
$(OBJDIR)/sig_drv.o: drivers/unix/sig_drv.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h
$(OBJDIR)/ttsl_drv.o: drivers/unix/ttsl_drv.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/sys.h \
 sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h
$(OBJDIR)/bin_drv.o: drivers/unix/bin_drv.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h
$(OBJDIR)/multi_drv.o: drivers/unix/multi_drv.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/sys.h \
 sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h
$(OBJDIR)/erl_tracer_nif.o: nifs/common/erl_tracer_nif.c beam/erl_nif.h \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/erl_drv_nif.h ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_nif_api_funcs.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/sys.h \
 beam/erl_threads.h beam/erl_lock_flags.h beam/erl_term.h \
 sys/common/erl_mmap.h ../include/internal/erl_printf.h \
 ../include/internal/ethread.h ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h
$(OBJDIR)/zlib_nif.o: nifs/common/zlib_nif.c beam/erl_nif.h \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/erl_drv_nif.h ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_nif_api_funcs.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/sys.h \
 beam/erl_threads.h beam/erl_lock_flags.h beam/erl_term.h \
 sys/common/erl_mmap.h ../include/internal/erl_printf.h \
 ../include/internal/ethread.h ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h
$(OBJDIR)/prim_buffer_nif.o: nifs/common/prim_buffer_nif.c beam/erl_nif.h \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/erl_drv_nif.h ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_nif_api_funcs.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/sys.h \
 beam/erl_threads.h beam/erl_lock_flags.h beam/erl_term.h \
 sys/common/erl_mmap.h ../include/internal/erl_printf.h \
 ../include/internal/ethread.h ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h
$(OBJDIR)/prim_file_nif.o: nifs/common/prim_file_nif.c beam/erl_nif.h \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/erl_drv_nif.h ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_nif_api_funcs.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/sys.h \
 beam/erl_threads.h beam/erl_lock_flags.h beam/erl_term.h \
 sys/common/erl_mmap.h ../include/internal/erl_printf.h \
 ../include/internal/ethread.h ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_driver.h nifs/common/prim_file_nif.h
$(OBJDIR)/unix_prim_file.o: nifs/unix/unix_prim_file.c beam/erl_nif.h \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/erl_drv_nif.h ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_nif_api_funcs.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/sys.h \
 beam/erl_threads.h beam/erl_lock_flags.h beam/erl_term.h \
 sys/common/erl_mmap.h ../include/internal/erl_printf.h \
 ../include/internal/ethread.h ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h nifs/common/prim_file_nif.h
$(OBJDIR)/sys_env.o: sys/unix/sys_env.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_thr_progress.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_vm.h beam/erl_message.h \
 beam/erl_proc_sig_queue.h beam/external.h beam/erl_node_tables.h \
 beam/erl_alloc.h beam/erl_process_dict.h beam/erl_node_container_utils.h \
 beam/erl_ptab.h beam/erl_time.h beam/erl_monitor_link.h beam/erl_utils.h \
 beam/hash.h beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 beam/erl_drv_nif.h ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_binary.h beam/erl_hl_timer.h \
 $(TTF_DIR)/erl_atom_table.h beam/erl_async.h \
 beam/erl_gc.h beam/erl_trace.h beam/erl_db.h beam/export.h beam/index.h \
 beam/code_ix.h $(TTF_DIR)/beam_opcodes.h \
 beam/beam_load.h beam/erl_bits.h
$(OBJDIR)/sys_uds.o: sys/unix/sys_uds.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 sys/unix/sys_uds.h
$(OBJDIR)/sys_float.o: sys/unix/sys_float.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 beam/erl_process.h
$(OBJDIR)/erl_main.o: sys/unix/erl_main.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h
$(OBJDIR)/sys_drivers.o: sys/unix/sys_drivers.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_threads.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 beam/erl_cpu_topology.h beam/erl_sys_driver.h sys/unix/sys_uds.h \
 sys/unix/erl_child_setup.h
$(OBJDIR)/erl_child_setup.o: sys/unix/erl_child_setup.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h sys/unix/sys_uds.h \
 beam/hash.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_term.h sys/unix/erl_child_setup.h
$(OBJDIR)/erl_unix_sys_ddll.o: sys/unix/erl_unix_sys_ddll.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h
$(OBJDIR)/sys_time.o: sys/unix/sys_time.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 sys/common/erl_os_monotonic_time_extender.h beam/erl_threads.h
$(OBJDIR)/sys.o: sys/unix/sys.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_thr_progress.h beam/erl_process.h \
 beam/erl_process_lock.h beam/erl_port.h beam/erl_vm.h beam/erl_message.h \
 beam/erl_proc_sig_queue.h beam/external.h beam/erl_node_tables.h \
 beam/erl_alloc.h $(TTF_DIR)/erl_alloc_types.h \
 beam/erl_thr_progress.h beam/erl_alloc_util.h sys/common/erl_mseg.h \
 sys/common/erl_mmap.h beam/lttng-wrapper.h \
 beam/erl_sched_spec_pre_alloc.h beam/erl_process_dict.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_utils.h beam/hash.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 beam/erl_drv_nif.h ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_binary.h beam/erl_hl_timer.h \
 $(TTF_DIR)/erl_atom_table.h beam/erl_async.h \
 beam/erl_gc.h beam/erl_trace.h beam/erl_db.h beam/export.h beam/index.h \
 beam/code_ix.h $(TTF_DIR)/beam_opcodes.h \
 beam/beam_load.h beam/erl_bits.h beam/erl_threads.h beam/global.h \
 beam/atom.h beam/module.h beam/register.h beam/erl_fun.h \
 beam/erl_debug.h beam/error.h beam/erl_io_queue.h beam/erl_nif.h \
 beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h sys/common/erl_check_io.h \
 beam/erl_sys_driver.h beam/erl_cpu_topology.h
$(OBJDIR)/erl_sys_common_misc.o: sys/common/erl_sys_common_misc.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h
$(OBJDIR)/erl_os_monotonic_time_extender.o: \
 sys/common/erl_os_monotonic_time_extender.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 sys/common/erl_os_monotonic_time_extender.h beam/sys.h \
 sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_threads.h
$(OBJDIR)/erl_poll.o $(OBJDIR)/erl_poll.flbk.o: sys/common/erl_poll.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 sys/common/erl_poll.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/sys.h \
 beam/erl_threads.h beam/erl_lock_flags.h beam/erl_term.h \
 sys/common/erl_mmap.h ../include/internal/erl_printf.h \
 ../include/internal/ethread.h ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h sys/common/erl_poll_api.h beam/erl_thr_progress.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_port.h beam/erl_vm.h \
 beam/erl_message.h beam/erl_proc_sig_queue.h beam/external.h \
 beam/erl_node_tables.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h \
 beam/erl_process_dict.h beam/erl_node_container_utils.h beam/erl_ptab.h \
 beam/erl_time.h beam/erl_monitor_link.h beam/erl_utils.h beam/hash.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 beam/erl_drv_nif.h ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_binary.h beam/erl_hl_timer.h \
 $(TTF_DIR)/erl_atom_table.h beam/erl_async.h \
 beam/erl_gc.h beam/erl_trace.h beam/erl_db.h beam/export.h beam/index.h \
 beam/code_ix.h $(TTF_DIR)/beam_opcodes.h \
 beam/beam_load.h beam/erl_bits.h beam/erl_driver.h beam/erl_alloc.h \
 beam/erl_msacc.h
$(OBJDIR)/erl_osenv.o: sys/common/erl_osenv.c sys/common/erl_osenv.h \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 beam/erl_alloc.h beam/erl_process.h beam/erl_rbtree.h
$(OBJDIR)/erl_mtrace_sys_wrap.o: sys/common/erl_mtrace_sys_wrap.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_mtrace.h \
 $(TTF_DIR)/erl_alloc_types.h
$(OBJDIR)/erl_mseg.o: sys/common/erl_mseg.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h \
 ../include/internal/erl_printf.h \
 $(TTF_DIR)/erl_alloc_types.h \
 ../include/internal/ethread.h ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/global.h beam/erl_alloc.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h beam/lttng-wrapper.h \
 beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 beam/erl_threads.h beam/erl_mtrace.h beam/erl_time.h beam/erl_alloc.h \
 beam/big.h beam/global.h beam/erl_thr_progress.h \
 sys/common/erl_util_queue.h
$(OBJDIR)/erl_mmap.o: sys/common/erl_mmap.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h \
 ../include/internal/erl_printf.h \
 $(TTF_DIR)/erl_alloc_types.h \
 ../include/internal/ethread.h ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_vm.h beam/erl_message.h \
 beam/erl_proc_sig_queue.h beam/external.h beam/erl_node_tables.h \
 beam/erl_alloc.h beam/erl_thr_progress.h beam/erl_alloc_util.h \
 sys/common/erl_mseg.h sys/common/erl_mmap.h beam/lttng-wrapper.h \
 beam/erl_sched_spec_pre_alloc.h beam/erl_process.h \
 beam/erl_process_dict.h beam/erl_node_container_utils.h beam/erl_ptab.h \
 beam/erl_time.h beam/erl_monitor_link.h beam/erl_utils.h beam/hash.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 beam/erl_drv_nif.h ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_binary.h beam/erl_hl_timer.h \
 $(TTF_DIR)/erl_atom_table.h beam/erl_async.h \
 beam/erl_gc.h beam/erl_trace.h beam/erl_db.h beam/export.h beam/index.h \
 beam/code_ix.h $(TTF_DIR)/beam_opcodes.h \
 beam/beam_load.h beam/erl_bits.h beam/atom.h
$(OBJDIR)/erl_check_io.o: sys/common/erl_check_io.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/global.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h \
 beam/erl_port.h sys/common/erl_check_io.h beam/erl_sys_driver.h \
 sys/common/erl_poll.h sys/common/erl_poll_api.h beam/erl_port_task.h \
 beam/erl_thr_progress.h beam/erl_bif_unique.h beam/big.h beam/global.h \
 beam/dtrace-wrapper.h beam/lttng-wrapper.h beam/erl_time.h
$(OBJDIR)/preload.o: x86_64-pc-linux-gnu/preload.c
$(OBJDIR)/beam_opcodes.o: $(TTF_DIR)/beam_opcodes.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/sys.h sys/unix/erl_unix_sys.h ../include/internal/erl_errno.h \
 ../include/internal/erl_misc_utils.h ../include/internal/erl_errno.h \
 beam/erl_lock_check.h beam/sys.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_vm.h beam/export.h beam/index.h \
 beam/hash.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/beam_load.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_port.h beam/erl_vm.h \
 beam/erl_message.h beam/erl_proc_sig_queue.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_utils.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h $(TTF_DIR)/erl_atom_table.h \
 beam/erl_async.h beam/erl_gc.h beam/erl_trace.h beam/erl_db.h \
 beam/export.h beam/erl_bits.h beam/erl_process.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h \
 $(TTF_DIR)/erl_atom_table.h beam/beam_load.h
$(OBJDIR)/driver_tab.o: $(TTF_DIR)/driver_tab.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/global.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_vm.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_proc_sig_queue.h beam/erl_utils.h \
 beam/hash.h beam/index.h beam/atom.h \
 $(TTF_DIR)/erl_atom_table.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/export.h \
 beam/beam_load.h beam/erl_process.h beam/erl_process_lock.h \
 beam/erl_port.h beam/erl_message.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h beam/erl_async.h beam/erl_gc.h beam/erl_trace.h \
 beam/erl_db.h beam/erl_bits.h beam/module.h beam/register.h \
 beam/erl_fun.h beam/erl_debug.h beam/error.h beam/erl_io_queue.h \
 beam/erl_nif.h beam/erl_nif_api_funcs.h beam/dtrace-wrapper.h
$(OBJDIR)/erl_atom_table.o: $(TTF_DIR)/erl_atom_table.c
$(OBJDIR)/erl_bif_table.o: $(TTF_DIR)/erl_bif_table.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/export.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/index.h beam/hash.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/beam_load.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_port.h beam/erl_vm.h \
 beam/erl_message.h beam/erl_proc_sig_queue.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_utils.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h $(TTF_DIR)/erl_atom_table.h \
 beam/erl_async.h beam/erl_gc.h beam/erl_trace.h beam/erl_db.h \
 beam/export.h beam/erl_bits.h beam/erl_vm.h beam/erl_process.h \
 beam/bif.h $(TTF_DIR)/erl_bif_table.h \
 $(TTF_DIR)/erl_bif_table.h \
 $(TTF_DIR)/erl_atom_table.h
$(OBJDIR)/erl_bif_wrap.o: $(TTF_DIR)/erl_bif_wrap.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/export.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/index.h beam/hash.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/beam_load.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_port.h beam/erl_vm.h \
 beam/erl_message.h beam/erl_proc_sig_queue.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_utils.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h $(TTF_DIR)/erl_atom_table.h \
 beam/erl_async.h beam/erl_gc.h beam/erl_trace.h beam/erl_db.h \
 beam/export.h beam/erl_bits.h beam/erl_vm.h beam/global.h beam/atom.h \
 beam/module.h beam/register.h beam/erl_fun.h beam/erl_debug.h \
 beam/error.h beam/erl_io_queue.h beam/erl_nif.h beam/erl_nif_api_funcs.h \
 beam/dtrace-wrapper.h beam/erl_process.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h \
 $(TTF_DIR)/erl_bif_table.h \
 $(TTF_DIR)/erl_atom_table.h
$(OBJDIR)/erl_dirty_bif_wrap.o: $(TTF_DIR)/erl_dirty_bif_wrap.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/erl_process.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/erl_process_lock.h beam/erl_port.h \
 beam/erl_vm.h beam/erl_message.h beam/erl_proc_sig_queue.h \
 beam/external.h beam/erl_node_tables.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/erl_process.h \
 beam/erl_process_dict.h beam/erl_node_container_utils.h beam/erl_ptab.h \
 beam/erl_time.h beam/erl_monitor_link.h beam/erl_utils.h beam/hash.h \
 beam/erl_port_task.h beam/erl_sys_driver.h beam/erl_driver.h \
 beam/erl_drv_nif.h ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h \
 beam/erl_binary.h beam/erl_hl_timer.h \
 $(TTF_DIR)/erl_atom_table.h beam/erl_async.h \
 beam/erl_gc.h beam/erl_trace.h beam/erl_db.h beam/export.h beam/index.h \
 beam/code_ix.h $(TTF_DIR)/beam_opcodes.h \
 beam/beam_load.h beam/erl_bits.h beam/erl_nfunc_sched.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h beam/error.h \
 $(TTF_DIR)/erl_bif_table.h \
 $(TTF_DIR)/erl_atom_table.h
$(OBJDIR)/erl_guard_bifs.o: $(TTF_DIR)/erl_guard_bifs.c \
 $(ERL_TOP)/erts/x86_64-pc-linux-gnu/config.h \
 beam/export.h beam/sys.h sys/unix/erl_unix_sys.h \
 ../include/internal/erl_errno.h ../include/internal/erl_misc_utils.h \
 ../include/internal/erl_errno.h beam/erl_lock_check.h beam/erl_threads.h \
 beam/erl_lock_flags.h beam/erl_term.h sys/common/erl_mmap.h beam/sys.h \
 ../include/internal/erl_printf.h ../include/internal/ethread.h \
 ../include/internal/ethread_inline.h \
 ../include/internal/x86_64/ethread.h \
 ../include/internal/x86_64/../i386/ethread.h \
 ../include/internal/x86_64/../i386/ethr_membar.h \
 ../include/internal/x86_64/../i386/atomic.h \
 ../include/internal/x86_64/../i386/ethr_dw_atomic.h \
 ../include/internal/x86_64/../i386/spinlock.h \
 ../include/internal/x86_64/../i386/rwlock.h \
 ../include/internal/gcc/ethread.h \
 ../include/internal/libatomic_ops/ethread.h \
 ../include/internal/ethr_atomics.h \
 ../include/internal/ethr_optimized_fallbacks.h \
 ../include/internal/pthread/ethr_event.h \
 ../include/internal/ethr_mutex.h beam/erl_lock_count.h beam/erl_msacc.h \
 sys/common/erl_osenv.h beam/index.h beam/hash.h beam/erl_alloc.h \
 $(TTF_DIR)/erl_alloc_types.h beam/erl_thr_progress.h \
 beam/erl_alloc_util.h sys/common/erl_mseg.h sys/common/erl_mmap.h \
 beam/lttng-wrapper.h beam/erl_sched_spec_pre_alloc.h beam/code_ix.h \
 $(TTF_DIR)/beam_opcodes.h beam/beam_load.h \
 beam/erl_process.h beam/erl_process_lock.h beam/erl_port.h beam/erl_vm.h \
 beam/erl_message.h beam/erl_proc_sig_queue.h beam/external.h \
 beam/erl_node_tables.h beam/erl_process_dict.h \
 beam/erl_node_container_utils.h beam/erl_ptab.h beam/erl_time.h \
 beam/erl_monitor_link.h beam/erl_utils.h beam/erl_port_task.h \
 beam/erl_sys_driver.h beam/erl_driver.h beam/erl_drv_nif.h \
 ../include/x86_64-pc-linux-gnu/erl_int_sizes_config.h beam/erl_binary.h \
 beam/erl_hl_timer.h $(TTF_DIR)/erl_atom_table.h \
 beam/erl_async.h beam/erl_gc.h beam/erl_trace.h beam/erl_db.h \
 beam/export.h beam/erl_bits.h beam/erl_vm.h beam/global.h beam/atom.h \
 beam/module.h beam/register.h beam/erl_fun.h beam/erl_debug.h \
 beam/error.h beam/erl_io_queue.h beam/erl_nif.h beam/erl_nif_api_funcs.h \
 beam/dtrace-wrapper.h beam/erl_process.h beam/bif.h \
 $(TTF_DIR)/erl_bif_table.h \
 $(TTF_DIR)/erl_bif_table.h
$(ZLIB_OBJDIR)/adler32.o: zlib/adler32.c \
 /home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_21.0/erts/x86_64-pc-linux-gnu/config.h \
 zlib/zutil.h zlib/zlib.h zlib/zconf.h
$(ZLIB_OBJDIR)/compress.o: zlib/compress.c \
 /home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_21.0/erts/x86_64-pc-linux-gnu/config.h \
 zlib/zlib.h zlib/zconf.h
$(ZLIB_OBJDIR)/crc32.o: zlib/crc32.c \
 /home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_21.0/erts/x86_64-pc-linux-gnu/config.h \
 zlib/zutil.h zlib/zlib.h zlib/zconf.h zlib/crc32.h
$(ZLIB_OBJDIR)/uncompr.o: zlib/uncompr.c \
 /home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_21.0/erts/x86_64-pc-linux-gnu/config.h \
 zlib/zlib.h zlib/zconf.h
$(ZLIB_OBJDIR)/deflate.o: zlib/deflate.c \
 /home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_21.0/erts/x86_64-pc-linux-gnu/config.h \
 zlib/deflate.h zlib/zutil.h zlib/zlib.h zlib/zconf.h
$(ZLIB_OBJDIR)/trees.o: zlib/trees.c \
 /home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_21.0/erts/x86_64-pc-linux-gnu/config.h \
 zlib/deflate.h zlib/zutil.h zlib/zlib.h zlib/zconf.h zlib/trees.h
$(ZLIB_OBJDIR)/zutil.o: zlib/zutil.c \
 /home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_21.0/erts/x86_64-pc-linux-gnu/config.h \
 zlib/zutil.h zlib/zlib.h zlib/zconf.h zlib/gzguts.h
$(ZLIB_OBJDIR)/inflate.o: zlib/inflate.c \
 /home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_21.0/erts/x86_64-pc-linux-gnu/config.h \
 zlib/zutil.h zlib/zlib.h zlib/zconf.h zlib/inftrees.h zlib/inflate.h \
 zlib/inffast.h zlib/inffixed.h
$(ZLIB_OBJDIR)/inftrees.o: zlib/inftrees.c \
 /home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_21.0/erts/x86_64-pc-linux-gnu/config.h \
 zlib/zutil.h zlib/zlib.h zlib/zconf.h zlib/inftrees.h
$(ZLIB_OBJDIR)/inffast.o: zlib/inffast.c \
 /home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_21.0/erts/x86_64-pc-linux-gnu/config.h \
 zlib/zutil.h zlib/zlib.h zlib/zconf.h zlib/inftrees.h zlib/inflate.h \
 zlib/inffast.h
