// Process enumeration using only public macOS APIs.
//
// Memory: uses proc_pid_rusage(RUSAGE_INFO_V2).ri_phys_footprint, which is the
// same number macOS Activity Monitor "Memory" column and `top` MEM column show.
// It includes resident pages plus compressed memory and IOKit mappings, i.e.
// the real RAM cost the system attributes to the process. Falls back to
// pti_resident_size only when rusage is unavailable (rare; some kernel tasks).
//
// CPU: computes per-process %CPU as a delta of (user_time + system_time)
// between two snapshots, using the same "per busy core" convention as Activity
// Monitor and `top`. A single saturated core is 100%, so multi-threaded
// processes can exceed 100% on multi-core systems. Values are smoothed across
// the configured update interval.
//
// No task_for_pid required, no entitlements, works on signed/sandboxed apps
// (we only read task info via libproc, which is publicly accessible).

#ifndef PROCS_H
#define PROCS_H

#include <libproc.h>
#include <mach/mach_time.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/proc_info.h>
#include <sys/resource.h>
#define PROCS_TOP_N 10
#define PROCS_NAME_LEN 64
typedef struct {
  pid_t pid;
  char name[PROCS_NAME_LEN];
  // Memory in bytes (resident set size / current RAM occupancy).
  uint64_t mem_bytes;
  // CPU in Activity Monitor / `top` style: 100% == one saturated core.
  double cpu_percent;
} procs_entry_t;

typedef struct {
  pid_t pid;
  uint64_t cpu_ns;
} procs_prev_t;

typedef struct {
  // Previous CPU times, sorted by pid for binary search.
  procs_prev_t *prev;
  size_t prev_count;
  size_t prev_capacity;
  // Mach absolute -> nanosecond conversion.
  mach_timebase_info_data_t timebase;
  uint64_t last_sample_abs;
  // System CPU core count used only as a sanity cap for per-process %CPU.
  int ncpu;
} procs_state_t;

static inline void procs_init(procs_state_t *st, int ncpu) {
  st->prev = NULL;
  st->prev_count = 0;
  st->prev_capacity = 0;
  st->last_sample_abs = 0;
  st->ncpu = ncpu > 0 ? ncpu : 1;
  mach_timebase_info(&st->timebase);
}

static int procs_cmp_prev_pid(const void *a, const void *b) {
  pid_t pa = ((const procs_prev_t *)a)->pid;
  pid_t pb = ((const procs_prev_t *)b)->pid;
  return (pa > pb) - (pa < pb);
}

static int procs_cmp_cpu_desc(const void *a, const void *b) {
  double da = ((const procs_entry_t *)a)->cpu_percent;
  double db = ((const procs_entry_t *)b)->cpu_percent;
  if (db > da) return 1;
  if (db < da) return -1;
  return 0;
}

static int procs_cmp_mem_desc(const void *a, const void *b) {
  uint64_t ma = ((const procs_entry_t *)a)->mem_bytes;
  uint64_t mb = ((const procs_entry_t *)b)->mem_bytes;
  if (mb > ma) return 1;
  if (mb < ma) return -1;
  return 0;
}

static uint64_t procs_lookup_prev_cpu(const procs_state_t *st, pid_t pid) {
  if (!st->prev || st->prev_count == 0) return 0;
  procs_prev_t key = { .pid = pid };
  procs_prev_t *hit = bsearch(&key, st->prev, st->prev_count,
                              sizeof(procs_prev_t), procs_cmp_prev_pid);
  return hit ? hit->cpu_ns : 0;
}

// Sample all processes once. Fills `top_cpu` and `top_mem` with up to
// PROCS_TOP_N entries each, sorted descending. Returns true on success.
static bool procs_sample(procs_state_t *st,
                         procs_entry_t *top_cpu, int *top_cpu_n,
                         procs_entry_t *top_mem, int *top_mem_n) {
  *top_cpu_n = 0;
  *top_mem_n = 0;

  // Time delta since last sample (nanoseconds), used to convert CPU time
  // deltas into percentages.
  uint64_t now_abs = mach_absolute_time();
  uint64_t now_ns = now_abs * st->timebase.numer / st->timebase.denom;
  uint64_t last_ns = st->last_sample_abs
      ? st->last_sample_abs * st->timebase.numer / st->timebase.denom
      : 0;
  uint64_t dt_ns = (last_ns > 0 && now_ns > last_ns) ? (now_ns - last_ns) : 0;
  st->last_sample_abs = now_abs;

  // Enumerate PIDs.
  int needed = proc_listallpids(NULL, 0);
  if (needed <= 0) return false;
  // Add headroom for races (processes spawning between sizing and read).
  int buf_count = needed + 64;
  pid_t *pids = (pid_t *)calloc((size_t)buf_count, sizeof(pid_t));
  if (!pids) return false;

  // proc_listallpids() returns a PID count, not a byte count.
  int got = proc_listallpids(pids, buf_count * (int)sizeof(pid_t));
  if (got <= 0) { free(pids); return false; }
  if (got > buf_count) got = buf_count;

  // Reusable per-process scratch.
  procs_entry_t *all = (procs_entry_t *)calloc((size_t)got, sizeof(procs_entry_t));
  if (!all) { free(pids); return false; }
  int all_n = 0;

  // Fresh prev table for next call. Sized to the exact pid count we observe.
  procs_prev_t *next_prev = (procs_prev_t *)calloc((size_t)got, sizeof(procs_prev_t));
  if (!next_prev) {
    free(all);
    free(pids);
    return false;
  }
  size_t next_prev_n = 0;

  for (int i = 0; i < got; i++) {
    pid_t pid = pids[i];
    if (pid <= 0) continue;

    struct proc_taskinfo taskinfo;
    if (proc_pidinfo(pid, PROC_PIDTASKINFO, 0, &taskinfo, PROC_PIDTASKINFO_SIZE)
        != PROC_PIDTASKINFO_SIZE) {
      continue;
    }
    uint64_t mem = taskinfo.pti_resident_size;
    // Prefer phys_footprint (matches Activity Monitor / top MEM column) when
    // rusage is accessible. Some kernel-owned tasks reject rusage; fall back to
    // resident size silently for those.
    struct rusage_info_v2 ru;
    if (proc_pid_rusage(pid, RUSAGE_INFO_V2, (rusage_info_t *)&ru) == 0
        && ru.ri_phys_footprint > 0) {
      mem = ru.ri_phys_footprint;
    }
    // proc_taskinfo CPU totals are reported in mach_absolute_time units, not
    // nanoseconds. Convert via the cached timebase so the delta math matches
    // dt_ns (which is also in nanoseconds).
    uint64_t cpu_units = taskinfo.pti_total_user + taskinfo.pti_total_system;
    uint64_t cpu_ns_now = cpu_units * st->timebase.numer / st->timebase.denom;

    // Process name. proc_name returns the executable basename (max 32 on
    // current macOS); fall back to proc_pidpath basename if empty.
    char name[PROCS_NAME_LEN] = {0};
    if (proc_name(pid, name, sizeof(name)) <= 0 || name[0] == '\0') {
      char path[PROC_PIDPATHINFO_MAXSIZE];
      if (proc_pidpath(pid, path, sizeof(path)) > 0) {
        const char *base = strrchr(path, '/');
        strncpy(name, base ? base + 1 : path, sizeof(name) - 1);
      } else {
        snprintf(name, sizeof(name), "pid:%d", pid);
      }
    }

    // CPU delta -> Activity Monitor / `top` style percentage.
    double cpu_pct = 0.0;
    if (dt_ns > 0) {
      uint64_t prev_ns = procs_lookup_prev_cpu(st, pid);
      if (prev_ns > 0 && cpu_ns_now > prev_ns) {
        // (cpu_delta_ns / interval_ns) gives single-core utilization fraction,
        // so 100% means one saturated core and multi-threaded processes can
        // exceed 100%.
        cpu_pct = ((double)(cpu_ns_now - prev_ns) / (double)dt_ns) * 100.0;
        double cpu_cap = (double)st->ncpu * 100.0;
        if (cpu_pct > cpu_cap) cpu_pct = cpu_cap;
      }
    }

    // Stash for next iteration.
    next_prev[next_prev_n].pid = pid;
    next_prev[next_prev_n].cpu_ns = cpu_ns_now;
    next_prev_n++;

    all[all_n].pid = pid;
    strncpy(all[all_n].name, name, PROCS_NAME_LEN - 1);
    all[all_n].name[PROCS_NAME_LEN - 1] = '\0';
    all[all_n].mem_bytes = mem;
    all[all_n].cpu_percent = cpu_pct;
    all_n++;
  }

  free(pids);

  // Replace prev table (sorted by pid for next bsearch).
  qsort(next_prev, next_prev_n, sizeof(procs_prev_t), procs_cmp_prev_pid);
  free(st->prev);
  st->prev = next_prev;
  st->prev_count = next_prev_n;
  st->prev_capacity = next_prev_n;

  // Top-N selection. Simple full sort; 500-1000 entries is trivial.
  qsort(all, (size_t)all_n, sizeof(procs_entry_t), procs_cmp_cpu_desc);
  int n_cpu = all_n < PROCS_TOP_N ? all_n : PROCS_TOP_N;
  for (int i = 0; i < n_cpu; i++) top_cpu[i] = all[i];
  *top_cpu_n = n_cpu;

  qsort(all, (size_t)all_n, sizeof(procs_entry_t), procs_cmp_mem_desc);
  int n_mem = all_n < PROCS_TOP_N ? all_n : PROCS_TOP_N;
  for (int i = 0; i < n_mem; i++) top_mem[i] = all[i];
  *top_mem_n = n_mem;

  free(all);
  return true;
}

// Format a byte count as a compact string matching Activity Monitor style:
// e.g. "23.4 GB", "512 MB", "4096 KB", "999 B".
static void procs_format_bytes(uint64_t bytes, char *out, size_t out_len) {
  const uint64_t KB = 1024ULL;
  const uint64_t MB = KB * 1024ULL;
  const uint64_t GB = MB * 1024ULL;
  if (bytes >= GB) {
    snprintf(out, out_len, "%.1f GB", (double)bytes / (double)GB);
  } else if (bytes >= MB) {
    snprintf(out, out_len, "%llu MB", (unsigned long long)(bytes / MB));
  } else if (bytes >= KB) {
    snprintf(out, out_len, "%llu KB", (unsigned long long)(bytes / KB));
  } else {
    snprintf(out, out_len, "%llu B", (unsigned long long)bytes);
  }
}

// Replace characters that would break the trigger key=value line.
static void procs_sanitize_name(char *s) {
  for (; *s; s++) {
    unsigned char c = (unsigned char)*s;
    if (c == '\'' || c == '|' || c == '\n' || c == '\r' || c < 0x20) *s = '_';
  }
}

#endif // PROCS_H
