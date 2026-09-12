#include <CoreFoundation/CoreFoundation.h>
#include <IOKit/IOKitLib.h>
#include <IOKit/hid/IOHIDKeys.h>
#include <IOKit/hidsystem/IOHIDEventSystemClient.h>
#include <IOKit/hidsystem/IOHIDServiceClient.h>
#include <mach/mach.h>
#include <math.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/sysctl.h>
#include <unistd.h>

#include "cpu.h"
#include "procs.h"
#include "../sketchybar.h"

static int clamp_int(int value, int min, int max) {
  if (value < min) return min;
  if (value > max) return max;
  return value;
}

typedef struct __IOHIDEvent *IOHIDEventRef;

IOHIDEventSystemClientRef IOHIDEventSystemClientCreateWithType(CFAllocatorRef allocator,
                                                               int type,
                                                               CFDictionaryRef options);
IOHIDEventRef IOHIDServiceClientCopyEvent(IOHIDServiceClientRef service,
                                          int32_t eventType,
                                          int64_t timestamp,
                                          uint32_t options);
double IOHIDEventGetFloatValue(IOHIDEventRef event, int32_t field);

static IOHIDEventSystemClientRef hid_client = NULL;
static CFArrayRef hid_services = NULL;

static bool ensure_hid_services(void) {
  if (hid_services) return true;

  hid_client = IOHIDEventSystemClientCreateWithType(kCFAllocatorDefault, 1, NULL);
  if (!hid_client) return false;

  hid_services = IOHIDEventSystemClientCopyServices(hid_client);
  if (!hid_services) {
    CFRelease(hid_client);
    hid_client = NULL;
    return false;
  }
  return true;
}

static bool cfstring_contains(CFTypeRef value, const char *needle) {
  if (!value || CFGetTypeID(value) != CFStringGetTypeID() || !needle) return false;

  char buffer[256];
  if (!CFStringGetCString((CFStringRef)value, buffer, sizeof(buffer), kCFStringEncodingUTF8)) {
    return false;
  }
  return strstr(buffer, needle) != NULL;
}

static double read_hid_service_temperature(IOHIDServiceClientRef service) {
  enum { kHIDTemperatureEventType = 15 };
  const int32_t field = (kHIDTemperatureEventType << 16);

  IOHIDEventRef event = IOHIDServiceClientCopyEvent(service, kHIDTemperatureEventType, 0, 0);
  if (!event) return -1.0;

  double temp = IOHIDEventGetFloatValue(event, field);
  CFRelease(event);

  if (!isfinite(temp) || temp <= 0.0) return -1.0;
  return temp;
}

static void read_temperatures(int *cpu_temp, int *gpu_temp) {
  if (cpu_temp) *cpu_temp = -1;
  if (gpu_temp) *gpu_temp = -1;
  if (!ensure_hid_services()) return;

  double cpu_sum = 0.0;
  int cpu_count = 0;
  double gpu_max = -1.0;

  CFIndex count = CFArrayGetCount(hid_services);
  for (CFIndex i = 0; i < count; i++) {
    IOHIDServiceClientRef service = (IOHIDServiceClientRef)CFArrayGetValueAtIndex(hid_services, i);
    if (!IOHIDServiceClientConformsTo(service, 0xff00, 5)) continue;

    CFTypeRef product = IOHIDServiceClientCopyProperty(service, CFSTR(kIOHIDProductKey));
    bool is_tdie = cfstring_contains(product, "PMU tdie");
    bool is_tdev = cfstring_contains(product, "PMU tdev");

    if (is_tdie || is_tdev) {
      double temp = read_hid_service_temperature(service);
      if (temp > 0.0) {
        if (is_tdie) {
          cpu_sum += temp;
          cpu_count++;
        }
        if (is_tdev && temp > gpu_max) {
          gpu_max = temp;
        }
      }
    }

    if (product) CFRelease(product);
  }

  if (cpu_temp && cpu_count > 0) {
    *cpu_temp = (int)lround(cpu_sum / (double)cpu_count);
  }
  if (gpu_temp && gpu_max > 0.0) {
    *gpu_temp = (int)lround(gpu_max);
  }
}

static bool read_memory_stats(uint64_t *used_bytes, uint64_t *total_bytes, int *percent) {
  if (!used_bytes || !total_bytes || !percent) return false;

  uint64_t total = 0;
  size_t total_len = sizeof(total);
  if (sysctlbyname("hw.memsize", &total, &total_len, NULL, 0) != 0) return false;

  mach_msg_type_number_t count = HOST_VM_INFO64_COUNT;
  vm_statistics64_data_t vmstat;
  if (host_statistics64(mach_host_self(),
                        HOST_VM_INFO64,
                        (host_info64_t)&vmstat,
                        &count) != KERN_SUCCESS) {
    return false;
  }

  mach_port_t host = mach_host_self();
  vm_size_t page_size = 0;
  if (host_page_size(host, &page_size) != KERN_SUCCESS) return false;

  uint64_t used_pages = (uint64_t)vmstat.active_count + (uint64_t)vmstat.wire_count + (uint64_t)vmstat.compressor_page_count;
  uint64_t used = used_pages * (uint64_t)page_size;
  int pct = total > 0 ? (int)((double)used / (double)total * 100.0) : 0;

  *used_bytes = used;
  *total_bytes = total;
  *percent = clamp_int(pct, 0, 100);
  return true;
}

static int read_gpu_utilization(void) {
  io_iterator_t iterator;
  if (IOServiceGetMatchingServices(kIOMainPortDefault,
                                   IOServiceMatching("IOAccelerator"),
                                   &iterator) != KERN_SUCCESS) {
    return -1;
  }

  int best = -1;
  io_object_t service;
  while ((service = IOIteratorNext(iterator))) {
    CFMutableDictionaryRef props = NULL;
    if (IORegistryEntryCreateCFProperties(service, &props, kCFAllocatorDefault, 0) == KERN_SUCCESS && props) {
      CFDictionaryRef stats = (CFDictionaryRef)CFDictionaryGetValue(props, CFSTR("PerformanceStatistics"));
      if (stats && CFGetTypeID(stats) == CFDictionaryGetTypeID()) {
        CFNumberRef num = (CFNumberRef)CFDictionaryGetValue(stats, CFSTR("Device Utilization %"));
        if (!num) num = (CFNumberRef)CFDictionaryGetValue(stats, CFSTR("Renderer Utilization %"));
        if (num && CFGetTypeID(num) == CFNumberGetTypeID()) {
          int value = 0;
          if (CFNumberGetValue(num, kCFNumberIntType, &value)) {
            if (value > best) best = value;
          }
        }
      }
      CFRelease(props);
    }
    IOObjectRelease(service);
  }
  IOObjectRelease(iterator);

  return (best >= 0) ? clamp_int(best, 0, 100) : -1;
}

// Append a top-N process list as trigger args:
//   top_cpu_1='name|pct' top_cpu_2='...' ...
//   top_mem_1='name|bytes_human' top_mem_2='...' ...
// Pipe-separated values keep the wire format compact and easy to parse in Lua.
static int append_procs_args(char *buf, int offset, int buf_size,
                             const char *prefix, bool is_mem,
                             procs_entry_t *list, int n) {
  for (int i = 0; i < n; i++) {
    char name[PROCS_NAME_LEN];
    strncpy(name, list[i].name, sizeof(name));
    name[sizeof(name) - 1] = '\0';
    procs_sanitize_name(name);

    char value[96];
    if (is_mem) {
      char human[32];
      procs_format_bytes(list[i].mem_bytes, human, sizeof(human));
      snprintf(value, sizeof(value), "%s|%s", name, human);
    } else {
      snprintf(value, sizeof(value), "%s|%.1f", name, list[i].cpu_percent);
    }

    int written = snprintf(buf + offset, (size_t)(buf_size - offset),
                           " %s_%d='%s'", prefix, i + 1, value);
    if (written < 0 || written >= buf_size - offset) {
      // Out of space; drop remaining entries to stay under sketchybar's
      // event arg limit.
      return offset;
    }
    offset += written;
  }
  return offset;
}

int main(int argc, char **argv) {
  float update_freq;
  if (argc < 3 || (sscanf(argv[2], "%f", &update_freq) != 1)) {
    printf("Usage: %s \"<event-name>\" \"<event_freq>\"\n", argv[0]);
    return 1;
  }

  alarm(0);
  struct cpu cpu;
  cpu_init(&cpu);

  // Process enumeration shares the CPU core count with cpu_update so per-proc
  // %CPU values are normalized the same way as the system total.
  int ncpu = 1;
  size_t ncpu_len = sizeof(ncpu);
  sysctlbyname("hw.ncpu", &ncpu, &ncpu_len, NULL, 0);
  procs_state_t procs_st;
  procs_init(&procs_st, ncpu);

  // Prime the prev-CPU table; first sample produces zeros for cpu_percent
  // (no baseline) and would otherwise waste a render cycle.
  procs_entry_t top_cpu[PROCS_TOP_N];
  procs_entry_t top_mem[PROCS_TOP_N];
  int top_cpu_n = 0, top_mem_n = 0;
  procs_sample(&procs_st, top_cpu, &top_cpu_n, top_mem, &top_mem_n);

  char event_message[256];
  snprintf(event_message, sizeof(event_message), "--add event '%s'", argv[1]);
  sketchybar(event_message);

  // Trigger buffer sized for system fields + 2 * PROCS_TOP_N entries.
  // Each entry max ~96 bytes incl. quoting + key prefix.
  char trigger_message[4096];
  for (;;) {
    cpu_update(&cpu);

    uint64_t mem_used = 0;
    uint64_t mem_total = 0;
    int mem_percent = -1;
    bool mem_ok = read_memory_stats(&mem_used, &mem_total, &mem_percent);

    int gpu_util = read_gpu_utilization();
    int cpu_temp = -1;
    int gpu_temp = -1;
    read_temperatures(&cpu_temp, &gpu_temp);

    procs_sample(&procs_st, top_cpu, &top_cpu_n, top_mem, &top_mem_n);

    int offset = snprintf(trigger_message,
                          sizeof(trigger_message),
                          "--trigger '%s' "
                          "cpu_user='%d' "
                          "cpu_sys='%d' "
                          "cpu_total='%d' "
                          "mem_used_percent='%d' "
                          "mem_used_bytes='%llu' "
                          "mem_total_bytes='%llu' "
                          "gpu_util='%d' "
                          "cpu_temp='%d' "
                          "gpu_temp='%d'",
                          argv[1],
                          cpu.user_load,
                          cpu.sys_load,
                          cpu.total_load,
                          mem_ok ? mem_percent : -1,
                          (unsigned long long)(mem_ok ? mem_used : 0ULL),
                          (unsigned long long)(mem_ok ? mem_total : 0ULL),
                          gpu_util,
                          cpu_temp,
                          gpu_temp);

    if (offset > 0 && offset < (int)sizeof(trigger_message)) {
      offset = append_procs_args(trigger_message, offset, (int)sizeof(trigger_message),
                                 "top_cpu", false, top_cpu, top_cpu_n);
      offset = append_procs_args(trigger_message, offset, (int)sizeof(trigger_message),
                                 "top_mem", true, top_mem, top_mem_n);
    }

    sketchybar(trigger_message);

    usleep((useconds_t)(update_freq * 1000000.0f));
  }
  return 0;
}
